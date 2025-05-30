#' Fit PCA dimension reduction
#'
#' @param data A data frame or matrix.
#' @param ndim The number of dimensions to reduce to. If NULL, all dimensions
#'   are returned.
#' @param ... Additional arguments to be passed to the `prcomp` function.
#'
#' @returns A list containing the PCA scores, loadings, and proportion of
#'   variance explained.
fit_pca <- function(data, ndim = NULL, ...) {
  fit <- prcomp(data, ...)
  scores <- as.data.frame(fit$x)
  loadings <- as.data.frame(fit$rotation)
  pve <- fit$sdev^2 / sum(fit$sdev^2)
  if (!is.null(ndim)) {
    scores <- scores[, 1:ndim, drop = FALSE]
    loadings <- loadings[, 1:ndim, drop = FALSE]
    pve <- pve[1:ndim]
  }
  out <- list(
    scores = scores,
    loadings = loadings,
    prop_var_explained = pve
  )
  return(out)
}


#' Fit tSNE dimension reduction
#'
#' @param data A data frame or matrix.
#' @param ... Additional arguments to be passed to the `Rtsne` function.
#'
#' @returns A list containing the tSNE scores.
fit_tsne <- function(data, ...) {
  # check if there are duplicates
  duplicate_idxs <- duplicated(data)
  if (sum(duplicate_idxs) > 0) {
    distinct_data <- data[!duplicate_idxs, , drop = FALSE]
  } else {
    distinct_data <- data
  }
  fit <- Rtsne::Rtsne(distinct_data, ...)
  distinct_scores <- as.data.frame(fit$Y) |>
    setNames(paste0("tSNE", 1:ncol(fit$Y)))
  if (sum(duplicate_idxs) > 0) {
    scores <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(fit$Y))) |>
      setNames(paste0("tSNE", 1:ncol(fit$Y)))
    scores[which(!duplicate_idxs), ] <- distinct_scores
    # add back the duplicates
    for (idx in which(duplicate_idxs)) {
      matched_idx <- which(
        duplicated(data[1:idx, , drop = FALSE], fromLast = TRUE)
      )[1]
      scores[idx, ] <- distinct_scores[matched_idx, ]
    }
  } else {
    scores <- distinct_scores
  }
  out <- list(
    scores = scores
  )
  return(out)
}


#' Fit UMAP dimension reduction
#'
#' @param data A data frame or matrix.
#' @param ... Additional arguments to be passed to the `umap` function.
#'
#' @returns A list containing the UMAP scores.
fit_umap <- function(data, ...) {
  # check if there are duplicates
  duplicate_idxs <- duplicated(data)
  if (sum(duplicate_idxs) > 0) {
    distinct_data <- data[!duplicate_idxs, , drop = FALSE]
  } else {
    distinct_data <- data
  }
  fit <- umap::umap(distinct_data, ...)
  distinct_scores <- as.data.frame(fit$layout) |>
    setNames(paste0("UMAP", 1:ncol(fit$layout)))
  if (sum(duplicate_idxs) > 0) {
    scores <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(fit$layout))) |>
      setNames(paste0("UMAP", 1:ncol(fit$layout)))
    scores[which(!duplicate_idxs), ] <- distinct_scores
    # add back the duplicates
    for (idx in which(duplicate_idxs)) {
      matched_idx <- which(
        duplicated(data[1:idx, , drop = FALSE], fromLast = TRUE)
      )[1]
      scores[idx, ] <- distinct_scores[matched_idx, ]
    }
  } else {
    scores <- distinct_scores
  }
  out <- list(
    scores = scores
  )
  return(out)
}


#' Evaluate neighborhood retention
#'
#' @description Evaluate the neighborhood retention of a dimension reduction
#'   method by comparing the k nearest neighbors of the original data and the
#'   reduced data.
#'
#' @param orig_data A data frame or matrix containing the original data.
#' @param dr_data A data frame or matrix containing the reduced data.
#' @param ks A vector of integers indicating the number of nearest neighbors to
#'   consider.
#'
#' @returns A data frame containing the `k` values and the corresponding
#'   retention.
eval_neighborhood_retention <- function(orig_data, dr_data, ks) {
  # Create a data frame to store the results
  results <- data.frame(
    k = integer(),
    retention = numeric()
  )

  # Loop through each k value
  for (k in ks) {
    # Get the k nearest neighbors for the original data
    orig_knn <- FNN::get.knn(orig_data, k = k)$nn.index

    # Get the k nearest neighbors for the reduced data
    dr_knn <- FNN::get.knn(dr_data, k = k)$nn.index

    # Calculate the retention rate
    retention_rate <- purrr::map_dbl(
      1:nrow(orig_knn),
      ~ sum(orig_knn[.x, ] %in% dr_knn[.x, ]) / k
    ) |>
      mean()

    # Store the results
    results <- rbind(results, data.frame(k = k, retention = retention_rate))
  }

  return(results)
}
