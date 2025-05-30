#' Evaluate stability of clustering results
#'
#' @param cluster_list List of cluster label vectors (encoded as integers).
#' @param max_pairs Number of cluster pairs to evaluate similarity.
#' @param metrics Character vector of metrics to compute. Options are "ARI" and
#'   "Jaccard".
#'
#' @returns A data frame with `max_eval` rows, containing the computed metrics
#'   for each pair of evaluated clusters.
cluster_stability <- function(cluster_list, max_pairs = 100,
                              metrics = c("ARI", "Jaccard")) {
  metrics <- match.arg(metrics, several.ok = TRUE)

  stability_df <- purrr::map(
    1:max_pairs,
    function(b) {
      clusts <- sample(cluster_list, 2, replace = FALSE)
      keep_samples <- !is.na(clusts[[1]]) & !is.na(clusts[[2]])
      out <- list()
      if ("Jaccard" %in% metrics) {
        out$Jaccard <- jaccard(
          clusts[[1]][keep_samples], clusts[[2]][keep_samples]
        )
      }
      if ("ARI" %in% metrics) {
        out$ARI <- mclust::adjustedRandIndex(
          clusts[[1]][keep_samples], clusts[[2]][keep_samples]
        )
      }
      return(as.data.frame(out))
    }
  ) |>
    dplyr::bind_rows()

  return(stability_df)
}


#' Compute Jaccard similarity between two cluster label vectors
#'
#' @param x Numeric vector of cluster memberships (encoded as integers).
#' @param y Numeric vector of cluster memberships (encoded as integers).
#'
#' @returns Computed Jaccard similarity metric according to Ben-Hur (2001).
jaccard <- function(x, y) {

  n <- length(x) # length of x; number of samples

  # step 1: recode cluster label vectors into binary matrix representations
  # Note: subtract diagonal of 1's since C_{ii} = 0 by definition
  Cx <- outer(X = x, Y = x, FUN = function(a, b) {a == b}) - diag(n)
  Cy <- outer(X = y, Y = y, FUN = function(a, b) {a == b}) - diag(n)

  # compute Jaccard similarity
  Cxy_prod <- sum(Cx * Cy)
  j <- Cxy_prod / (sum(Cx^2) + sum(Cy^2) - Cxy_prod)
  return(j)
}
