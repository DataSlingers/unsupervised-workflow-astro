#' Wrapper function to create clustering pipeline
#'
#' @param feature_mode A numeric value indicating the number of features to
#'   use in pipeline. Options are 7, 11, or 19.
#' @param preprocess_fun A function to apply to the data (e.g., apply PCA,
#'   tSNE, UMAP, ...)
#'
#' @returns A function that takes a data frame and returns a preprocessed data
#'   frame.
create_preprocessing_pipeline <- function(feature_mode, preprocess_fun) {
  g <- function(data) {
    data <- get_abundance_data(data, feature_mode = feature_mode) |>
      preprocess_fun()
    if ("scores" %in% names(data)) {
      data <- as.data.frame(data$scores)
    } else {
      data <- as.data.frame(data)
    }
    return(data)
  }
  return(g)
}


#' Subchunkify a quarto code chunk
#'
#' @description Helper function for showing figures of different sizes within
#'   the same quarto code chunk.
#'
#' @param g A plot, table, or object to be displayed.
#' @param i An integer indicating the index of the subchunk.
#' @param prefix A string indicating the prefix for the subchunk.
#' @param fig_height A numeric value indicating the height of the figure.
#' @param fig_width A numeric value indicating the width of the figure.
#' @param caption A string indicating the caption for the figure.
#' @param add_class A string indicating the class to be added to the div.
#' @param other_args A string indicating any other arguments to be passed to the
#'   subchunk.
subchunkify <- function(g, i = NULL, prefix = NULL,
                        fig_height = 6, fig_width = 10, caption = "''",
                        add_class = NULL, other_args = "") {

  g_deparsed <- paste0(deparse(function() {g}), collapse = '')

  if (!identical(other_args, "")) {
    if (!startsWith(other_args, ",")) {
      other_args <- paste0(", ", other_args)
    }
  }
  if (!is.null(prefix)) {
    prefix <- paste0(prefix, "-")
  }
  if (is.null(i)) {
    i <- subchunk_idx
    subchunk_idx <<- subchunk_idx + 1
  }

  sub_chunk <- paste0(
    "\n\n```{r subchunk-", prefix, i, "}\n#| fig-height: ", fig_height,
    "\n#| fig-width: ", fig_width,
    "\n#| fig-cap: ", caption,
    "\n#| echo: false",
    "\n", other_args, "\n(", g_deparsed, ")()",
    "\n```\n\n"
  )

  if (!is.null(add_class)) {
    cat(sprintf("<div class='%s'>", paste(add_class)))
  }
  cat(knitr::knit_child(text = sub_chunk, envir = environment(), quiet = TRUE))
  if (!is.null(add_class)) {
    cat("</div>")
  }
}


#' Align clusters using the Hungarian algorithm
#'
#' @param true_labels A vector of true labels.
#' @param predicted_labels A vector of predicted labels.
#'
#' @returns A vector of predicted labels after alignment.
align_clusters <- function(true_labels, predicted_labels) {
  # Create a contingency table
  contingency <- table(true_labels, predicted_labels)

  # Use the Hungarian algorithm to find the best label mapping
  assignment <- clue::solve_LSAP(contingency, maximum = TRUE)

  # Map predicted labels to aligned labels
  aligned_labels <- assignment[predicted_labels]

  return(aligned_labels)
}


#' Convert dimension reduction results list to clean, annotated data frame
#'
#' @param results_ls A list of dimension reduction results.
#'
#' @returns A data frame with the results of the dimension reduction.
dr_results_to_df <- function(results_ls) {
  results_ls |>
    dplyr::bind_rows(.id = "pipeline_name") |>
    dplyr::mutate(
      feature_mode_name = stringr::str_extract(
        pipeline_name, "(?<=\\[)(.*)(?=\\])"
      ) |>
        factor(levels = names(train_data_ls)),
      dr_method_name = stringr::str_extract(pipeline_name, "^(.*)(?=\\[)") |>
        stringr::str_trim() |>
        factor(levels = names(dr_fun_ls)),
      dr_method_type_name = stringr::str_remove(dr_method_name, "\\(.*\\)") |>
        stringr::str_trim() |>
        as.factor()
    ) |>
    dplyr::arrange(dr_method_type_name, dr_method_name, feature_mode_name) |>
    dplyr::mutate(
      pipeline_name = forcats::fct_inorder(pipeline_name)
    )
}


#' Add columns to clustering results data frame with pipeline information
#'
#' @param results_data A data frame with clustering results with the `name`
#'   column containing the full pipeline name
#'
#' @returns A data frame with additional columns for `k`, `pipeline_name`,
#'   `impute_mode_name`, `feature_mode_name`, `dr_method_name`, and
#'   `clust_method_name`.
annotate_clustering_results <- function(results_data) {
  results_data <- results_data |>
    dplyr::mutate(
      k = as.numeric(stringr::str_extract(name, "(?<=k = )\\d+")),
      pipeline_name = stringr::str_remove(name, "^.*:\\s") |>
        stringr::str_remove("\\s\\d+$"),
      impute_mode_name = stringr::str_extract(name, "(?<=\\[)(.*)(?=\\])") |>
        stringr::str_split(pattern = " \\+ ") |>
        purrr::map_chr(~ .x[[1]]),
      feature_mode_name = stringr::str_extract(name, "(?<=\\[)(.*)(?=\\])") |>
        stringr::str_split(pattern = " \\+ ") |>
        purrr::map_chr(~ .x[[2]]),
      dr_method_name = stringr::str_extract(name, "(?<=\\[)(.*)(?=\\])") |>
        stringr::str_split(pattern = " \\+ ") |>
        purrr::map_chr(~ .x[[3]]),
      clust_method_name = stringr::str_extract(name, "(?<=:\\s)[^\\[]+") |>
        stringr::str_trim()
    )
  return(results_data)
}
