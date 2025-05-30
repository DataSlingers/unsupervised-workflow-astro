#' Evaluate the generalizability of clustering results
#'
#' @param X_train Training data matrix (features).
#' @param cluster_train Vector of cluster labels for training data. User must
#'   supply either `cluster_train` or `cluster_model`. If both are provided,
#'   `cluster_train` will be used.
#' @param X_test Test data matrix (features).
#' @param cluster_test Vector of cluster labels for test data. User must supply
#'   either `cluster_test` or `cluster_model`. If both are provided,
#'   `cluster_test` will be used.
#' @param cluster_model Function to compute clusters from data. Should take in
#'   feature data matrix and return vector of cluster labels. Only used if
#'   `cluster_train` or `cluster_test` is not provided.
#' @param prediction_model Function to predict clusters from feature data
#'   matrix. If not provided, a random forest model will be used.
#'
#' @returns List of generalizability metrics, including the adjusted Rand index
#'   (ARI) between the training and test clusters, the test cluster labels, and
#'   the predicted cluster labels for the test data.
cluster_generalizability <- function(X_train, cluster_train = NULL,
                                     X_test, cluster_test = NULL,
                                     cluster_model = NULL,
                                     prediction_model = NULL) {
  if (is.null(cluster_train)) {
    if (is.null(cluster_model)) {
      stop("Either `cluster_model` or `cluster_train` must be provided.")
    }
    cluster_train <- cluster_model(X_train)
  }
  if (is.null(cluster_test)) {
    if (is.null(cluster_model)) {
      stop("Either `cluster_model` or `cluster_test` must be provided.")
    }
    cluster_test <- cluster_model(X_test)
  }

  if (is.null(prediction_model)) {
    # Default to random forest
    fit <- ranger::ranger(
      formula = .y ~ .,
      data = dplyr::bind_cols(.y = as.factor(cluster_train), X_train),
      num.threads = 1
    )
    cluster_preds <- predict(fit, data = X_test, num.threads = 1)$predictions
  } else {
    fit <- prediction_model(X_train, cluster_train)
    cluster_preds <- predict(fit, X_test)
  }
  return(list(
    ARI = mclust::adjustedRandIndex(cluster_test, cluster_preds),
    cluster_test = cluster_test,
    cluster_preds = cluster_preds
  ))
}
