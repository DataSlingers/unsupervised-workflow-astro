#' Plot consensus matrix and dendrogram
#'
#' @param nbhd_mat A matrix of consensus neighborhood values, where each entry
#'   (i, j) is the fraction of times that samples i and j are in the same
#'   cluster.
#' @param custom_colors (Optional) Custom color palette for the heatmap.
#' @param custom_color_values (Optional) Custom color values for the heatmap.
#' @param ... Additional arguments passed to `ggwrappers::plot_hclust()`.
#'
#' @returns A patchwork object containing the consensus matrix and dendrogram
#'   ggplots.
plot_consensus_clusters <- function(nbhd_mat,
                                    custom_colors = NULL,
                                    custom_color_values = NULL,
                                    ...) {

  hclust_out <- ggwrappers::plot_hclust(nbhd_mat, ...)
  hclust_out$plot <- hclust_out$plot +
    ggplot2::labs(
      title = "", color = "Cluster ID"
    )
  order_idx <- hclust_out$hclust$order
  heatmap_plt <- ggwrappers::plot_heatmap(
    X = as.data.frame(nbhd_mat[order_idx, order_idx]),
    show_ytext = FALSE,
    show_xtext = FALSE
  ) +
    ggplot2::labs(
      fill = "Frequency", x = "Star", y = "Star"
    ) +
    ggplot2::theme(
      panel.spacing = grid::unit(.05, "lines"),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, linewidth = 0
      )
    )
  if (!is.null(custom_colors) && !is.null(custom_color_values)) {
    heatmap_plt <- heatmap_plt +
      ggplot2::scale_fill_gradientn(
        colors = custom_colors, values = scales::rescale(custom_color_values)
      )
  }
  plt <- patchwork::wrap_plots(
    list(hclust_out$plot, heatmap_plt),
    ncol = 1, heights = c(1, 2)
  )
  return(plt)
}


#' Plot GC composition per cluster
#'
#' @param cluster_ids A vector of cluster IDs.
#' @param gcs A vector of GC names corresponding to the cluster IDs.
#'
#' @returns A ggplot object showing the GC composition per cluster.
plot_gcs_per_cluster <- function(cluster_ids, gcs) {
  plt <- data.frame(
    cluster_id = as.factor(cluster_ids),
    `GC Name` = gcs,
    check.names = FALSE
  ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = cluster_id,
      fill = `GC Name`
    ) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::labs(x = "Cluster ID", y = "Proportion") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    vthemes::theme_vmodern()
  return(plt)
}


#' Plot local stability on galactic coordinates
#'
#' @param local_stability A vector of local stability values (typically, output
#'   of `eval_local_stability()`.
#' @param cluster_ids A vector of cluster IDs.
#' @param metadata A data frame containing metadata with columns `GLON`, `GLAT`,
#'   and `GC_NAME`.
#'
#' @returns A ggplot object showing the local stability on galactic coordinates.
plot_local_stability_on_galactic <- function(local_stability,
                                             cluster_ids,
                                             metadata) {
  local_stability_df <- metadata |>
    dplyr::mutate(
      cluster_id = !!cluster_ids,
      local_stability = !!local_stability,
      id = 1:dplyr::n()
    )
  local_stability_plt <- local_stability_df |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = GLON,
      y = GLAT,
      shape = as.factor(cluster_id),
      color = local_stability,
      label = GC_NAME
    ) +
    ggplot2::geom_jitter(size = 1, width = 15, height = 8) +
    ggplot2::scale_color_viridis_c(option = "A", end = 0.95, limits = c(0, 1)) +
    ggplot2::scale_shape_manual(
      values = c(19:0)[1:length(unique(cluster_ids))]
    ) +
    ggplot2::labs(shape = "Cluster", color = "Local\nStability") +
    vthemes::theme_vmodern()
  return(local_stability_plt)
}


#' Plot local stability on dimension reduction visualizations
#'
#' @param local_stability A vector of local stability values (typically, output
#'   of `eval_local_stability()`.
#' @param cluster_ids A vector of cluster IDs.
#' @param metadata A data frame containing metadata with columns `GLON`, `GLAT`,
#'   and `GC_NAME`.
#' @param dr_data A long data frame containing the dimension reduction results
#'   with columns `Component 1`, `Component 2`, `method`, and `id`, containing
#'   the dimension reduction coordinates, method name, and unique ID for each
#'   sample.
#'
#' @returns A ggplot object showing the local stability on galactic coordinates.
plot_local_stability_on_dr <- function(local_stability,
                                       cluster_ids,
                                       metadata,
                                       dr_data) {
  local_stability_df <- metadata |>
    dplyr::mutate(
      cluster_id = !!cluster_ids,
      local_stability = !!local_stability,
      id = 1:dplyr::n()
    )
  dr_plt <- dplyr::left_join(dr_data, local_stability_df, by = "id") |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = `Component 1`,
      y = `Component 2`,
      shape = as.factor(cluster_id),
      color = local_stability,
      label = GC_NAME
    ) +
    ggplot2::geom_point(size = 0.3) +
    ggplot2::facet_wrap(~ method, scales = "free") +
    ggplot2::scale_color_viridis_c(option = "A", end = 0.95, limits = c(0, 1)) +
    ggplot2::scale_shape_manual(
      values = c(19:0)[1:length(unique(cluster_ids))]
    ) +
    ggplot2::labs(shape = "Cluster", color = "Local\nStability") +
    vthemes::theme_vmodern()
  return(dr_plt)
}
