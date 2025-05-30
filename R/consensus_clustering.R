#' Compute consensus neighborhood matrix
#'
#' @param cluster_list List of cluster label vectors (encoded as integers).
#'
#' @returns A matrix of consensus neighborhood values, where each entry (i, j)
#'   is the fraction of times that samples i and j are in the same cluster.
get_consensus_neighborhood_matrix <- function(cluster_list) {
  sum_mat <- matrix(0, length(cluster_list[[1]]), length(cluster_list[[1]]))
  count_mat <- matrix(0, length(cluster_list[[1]]), length(cluster_list[[1]]))
  for (clusters in cluster_list) {
    cur_nbhd_mat <- outer(
      X = clusters, Y = clusters, FUN = function(a, b) {a == b}
    )
    sum_mat <- sum_mat + ifelse(is.na(cur_nbhd_mat), 0, cur_nbhd_mat)
    count_mat <- count_mat + !is.na(cur_nbhd_mat)
  }
  nbhd_mat <- sum_mat / count_mat
  return(nbhd_mat)
}


#' Aggregate clusters via consensus clustering
#'
#' @param nbhd_mat A matrix of consensus neighborhood values, where each entry
#'   (i, j) is the fraction of times that samples i and j are in the same
#'   cluster.
#' @param k (Optional) Number of clusters to estimate.
#' @param ... Additional arguments passed to `ggwrappers::plot_hclust()`.
#'
#' @returns A list containing the cluster IDs (if `k` is provided), hierarchical
#'   clustering object, dendrogram, and plot.
fit_consensus_clusters <- function(nbhd_mat, k = NULL, ...) {
  hclust_out <- ggwrappers::plot_hclust(nbhd_mat, ...)
  if (!is.null(k)) {
    cluster_ids <- cutree(hclust_out$hclust, k = k)
  } else {
    cluster_ids <- NULL
  }
  return(list(
    cluster_ids = cluster_ids,
    hclust_fit = hclust_out$hclust,
    hclust_dend = hclust_out$dend,
    hclust_plot = hclust_out$plot
  ))
}


#' Compute average consensus neighborhood value within home cluster per sample
#'
#' @param nbhd_mat A matrix of consensus neighborhood values, where each entry
#'   (i, j) is the fraction of times that samples i and j are in the same
#'   cluster.
#' @param cluster_ids A vector of cluster IDs for each sample.
#'
#' @returns A vector of local stability values for each sample, where each value
#'   is the average consensus neighborhood value within the sample's home
#'   cluster.
eval_local_stability <- function(nbhd_mat, cluster_ids) {
  local_stability <- rep(NA, length(cluster_ids))
  for (cluster_id in unique(cluster_ids)) {
    cluster_samples <- cluster_ids == cluster_id
    cluster_nbhd <- nbhd_mat[cluster_samples, cluster_samples]
    diag(cluster_nbhd) <- NA
    local_stability[cluster_samples] <- rowMeans(cluster_nbhd, na.rm = TRUE)
  }
  return(local_stability)
}
