#' Fit K-means clustering
#'
#' @param data A data frame or matrix of features.
#' @param ks A vector of integers specifying the number of clusters to fit.
#' @param preprocess_fun A function to preprocess the data before clustering.
#' @param num_init Number of initializations for K-means.
#' @param initializer How to initialize K-means
#' @param ... Additional arguments to pass to `ClusterR::Kmeans_rcpp()`
#'
#' @returns A list containing the cluster IDs and centroids for each value of
#'   `k`.
fit_kmeans <- function(data, ks, preprocess_fun = NULL,
                       num_init = 10, initializer = "kmeans++", ...) {
  if (!is.null(preprocess_fun)) {
    data <- preprocess_fun(data)
  }
  fit_ls <- purrr::map(
    ks,
    function(k) {
      fit <- ClusterR::KMeans_rcpp(
        data = data,
        clusters = k,
        num_init = num_init,
        initializer = initializer,
        ...
      )
      return(fit)
    }
  )
  cluster_ids_ls <- purrr::map(fit_ls, "clusters") |>
    setNames(sprintf("k = %s", ks))
  centroids_ls <- purrr::map(fit_ls, "centroids") |>
    setNames(sprintf("k = %s", ks))
  out <- list(
    cluster_ids = cluster_ids_ls,
    centers = centroids_ls
  )
  return(out)
}


#' Fit hierarchical clustering
#'
#' @param data A data frame or matrix of features.
#' @param ks A vector of integers specifying the number of clusters to fit.
#' @param preprocess_fun A function to preprocess the data before clustering.
#' @param d The distance metric to use. Default is "euclidean".
#' @param linkage The linkage method to use. Default is "ward.D".
#' @param ... Additional arguments to pass to `hclust()`.
#'
#' @returns A list containing the cluster IDs and dendrogram for each value of
#'   `k`.
fit_hclust <- function(data, ks = NULL, preprocess_fun = NULL,
                       d = "euclidean", linkage = "ward.D", ...) {
  if (!is.null(preprocess_fun)) {
    data <- preprocess_fun(data)
  }
  fit <- hclust(dist(data, method = d), method = linkage, ...)
  dend_tree <- as.dendrogram(fit)
  if (!is.null(ks)) {
    cluster_ids_ls <- list()
    for (k in ks) {
      cluster_ids_ls[[sprintf("k = %s", k)]] <- cutree(fit, k)
    }
  } else {
    cluster_ids_ls <- NULL
  }
  out <- list(
    cluster_ids = cluster_ids_ls,
    dend_tree = dend_tree
  )
  return(out)
}


#' Fit spectral clustering
#'
#' @param data A data frame or matrix of features.
#' @param ks A vector of integers specifying the number of clusters to fit.
#' @param preprocess_fun A function to preprocess the data before clustering.
#' @param affinity The type of affinity to use. Default is "nearest_neighbors".
#'   Currently, only "nearest_neighbors" is supported.
#' @param n_components The number of components to use for spectral clustering.
#'   Default is `k`.
#' @param n_neighbors The number of neighbors to use for KNN graph.
#'   Default is 10.
#' @param ... Additional arguments to pass to `fit_kmeans()`.
#'
#' @returns A list containing the cluster IDs and embedding for each value of
#'   `k`.
fit_spectral_clustering <- function(data, ks, preprocess_fun = NULL,
                                    affinity = "nearest_neighbors",
                                    n_components = NULL,
                                    n_neighbors = 10,
                                    ...) {
  if (!is.null(preprocess_fun)) {
    data <- preprocess_fun(data)
  }
  if (is.null(n_components)) {
    n_components <- ks
  }

  # Step 1: Build KNN Graph
  knn_graph <- FNN::get.knn(data, k = n_neighbors - 1)  # subtract 1 since get.knn does not include self
  W <- matrix(0, nrow(data), nrow(data))
  for (i in 1:nrow(data)) {
    W[i, knn_graph$nn.index[i, ]] <- 1
  }
  W <- 1 / 2 * (W + t(W))  # make it symmetric

  # Step 2: Compute normalized Laplacian
  D <- diag(rowSums(W))  # degree matrix
  L <- D - W  # unnormalized Laplacian
  d_invsqrt <- 1 / sqrt(diag(D))
  L_sym <- t(t(d_invsqrt * L) * d_invsqrt)
  diag(L_sym) <- 1

  # Step 3: Eigen decomposition
  eig <- RSpectra::eigs_sym(L_sym, k = max(n_components), which = "SA")
  embedding <- eig$vectors[, max(n_components):1]  # take smallest eigenvectors
  U <- embedding * d_invsqrt  # recover u = D^{-1/2} x

  # Step 4: Cluster with k-means
  cluster_ids_ls <- purrr::map2(
    ks, n_components,
    function(k, n_component) {
      fit_kmeans(U[, 1:n_component, drop = FALSE], ks = k, ...)$cluster_ids[[1]]
    }
  ) |>
    setNames(sprintf("k = %s", ks))

  out <- list(
    cluster_ids = cluster_ids_ls,
    embedding = embedding
  )
  return(out)
}


#' Wrapper to fit best clustering pipeline
#'
#' @param data_ls A list of data frames or matrices to fit clustering on. E.g.,
#'   `list("Mean-imputed" = mean_imputed_data, "RF-imputed" = rf_imputed_data)`.
#' @param k The number of clusters to fit.
#'
#' @returns A vector of cluster IDs from the best clustering pipeline.
fit_best_pipeline <- function(data_ls, k) {

  #### choose number of features ####
  feature_modes <- list(
    "Small" = 7,
    "Medium" = 11,
    "Big" = 19
  )

  #### choose dimension reduction methods ####
  # raw data
  identity_fun_ls <- list("Raw" = function(x) x)

  # pca
  pca_fun_ls <- list("PCA" = purrr::partial(fit_pca, ndim = 4))

  # tsne
  tsne_perplexities <- c(30, 100)
  tsne_fun_ls <- purrr::map(
    tsne_perplexities,
    ~ purrr::partial(fit_tsne, dims = 2, perplexity = .x)
  ) |>
    setNames(sprintf("tSNE (perplexity = %d)", tsne_perplexities))

  # putting it together
  dr_fun_ls <- c(
    identity_fun_ls,
    pca_fun_ls,
    tsne_fun_ls
  )

  #### choose clustering methods ####
  clust_fun_ls <- list("K-means" = purrr::partial(fit_kmeans, ks = k))

  #### Fit Clustering Pipelines ####
  pipe_tib <- tidyr::expand_grid(
    data = data_ls,
    feature_mode = feature_modes,
    dr_method = dr_fun_ls,
    clust_method = clust_fun_ls
  ) |>
    dplyr::mutate(
      impute_mode_name = names(data),
      feature_mode_name = names(feature_mode),
      dr_method_name = names(dr_method),
      clust_method_name = names(clust_method),
      name = stringr::str_glue(
        "{clust_method_name} [{impute_mode_name} + {feature_mode_name} + {dr_method_name}]"
      )
    ) |>
    # remove some clustering pipelines to reduce computation burden
    dplyr::filter(
      # remove all big feature set + dimension-reduction runs
      !((dr_method_name != "Raw") & (feature_mode_name == "Big"))
    )
  pipe_ls <- split(pipe_tib, seq_len(nrow(pipe_tib))) |>
    setNames(pipe_tib$name)

  # fit clustering pipelines (if not already cached)
  clust_fit_ls <- purrr::map(
    pipe_ls,
    function(pipe_df) {
      g <- create_preprocessing_pipeline(
        feature_mode = pipe_df$feature_mode[[1]],
        preprocess_fun = pipe_df$dr_method[[1]]
      )
      clust_out <- pipe_df$clust_method[[1]](
        data = pipe_df$data[[1]], preprocess_fun = g
      )
      return(clust_out)
    }
  )
  # get consensus clusters
  clust_fit_ls <- purrr::map(clust_fit_ls, ~ .x$cluster_ids) |>
    purrr::list_flatten(name_spec = "{inner}: {outer}")
  nbhd_mat <- get_consensus_neighborhood_matrix(clust_fit_ls)
  consensus_out <- fit_consensus_clusters(nbhd_mat, k = k)
  return(consensus_out$cluster_ids)
}
