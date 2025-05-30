#' Shiny app to view clustering results on training data
view_clusters_app <- function(...) {
  ui <- bslib::page_sidebar(
    sidebar = bslib::sidebar(
      open = "open",
      width = 400,
      shiny::selectInput(
        "impute_mode",
        label = shiny::HTML("<b>Imputation Method</b>"),
        choices = c("Mean-imputed", "RF-imputed"),
        selected = "Mean-imputed"
      ),
      shiny::selectInput(
        "feature_mode",
        label = shiny::HTML("<b>Feature Mode</b>"),
        choices = c("Small", "Medium", "Big"),
        selected = "Small"
      ),
      shiny::selectInput(
        "dr_method",
        label = shiny::HTML("<b>Dimension Reduction Method</b>"),
        choices = c(
          "Raw", "PCA", "tSNE (perplexity = 30)", "tSNE (perplexity = 100)"
        ),
        selected = "PCA"
      ),
      shiny::selectInput(
        "clust_method",
        label = shiny::HTML("<b>Clustering Method</b>"),
        choices = c(
          "K-means",
          "Hierarchical (dist = euclidean, link = complete)",
          "Hierarchical (dist = euclidean, link = ward.D)",
          "Spectral (n_neighbors = 5)",
          "Spectral (n_neighbors = 30)",
          "Spectral (n_neighbors = 60)",
          "Spectral (n_neighbors = 100)"
        ),
        selected = "Spectral (n_neighbors = 60)"
      ),
      shiny::numericInput(
        "k",
        label = shiny::HTML("<b>Number of Clusters</b>"),
        min = 2, max = 30, step = 1, value = 2
      )
    ),
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Galactic Location Plot",
        shiny::htmlOutput("plot_gloc")
      ),
      bslib::nav_panel(
        title = "Dimension Reduction Plot",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              "plot_feature_mode",
              label = shiny::HTML("<b>Feature Mode for DR Plot</b>"),
              choices = c("Small", "Medium", "Big"),
              selected = "Small",
              width = "90%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              "plot_dr_method",
              label = shiny::HTML("<b>DR Method for DR Plot</b>"),
              choices = c(
                "PCA",
                "tSNE (perplexity = 10)",
                "tSNE (perplexity = 30)",
                "tSNE (perplexity = 60)",
                "tSNE (perplexity = 100)",
                "tSNE (perplexity = 300)",
                "UMAP (n_neighbors = 10)",
                "UMAP (n_neighbors = 30)",
                "UMAP (n_neighbors = 60)",
                "UMAP (n_neighbors = 100)",
                "UMAP (n_neighbors = 300)"
              ),
              selected = "PCA",
              width = "90%"
            )
          )
        ),
        shiny::htmlOutput("plot_dr")
      ),
      bslib::nav_panel(
        title = "Variable Scatter Plot",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              "plot_var1",
              label = shiny::HTML("<b>X Variable</b>"),
              choices = c(
                "C_FE", "CI_FE", "N_FE", "O_FE", "NA_FE", "MG_FE", "AL_FE",
                "SI_FE", "S_FE", "K_FE", "CA_FE", "TI_FE", "TIII_FE", "V_FE",
                "CR_FE", "MN_FE", "FE_H", "CO_FE", "NI_FE"
              ),
              selected = "FE_H",
              width = "90%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              "plot_var2",
              label = shiny::HTML("<b>Y Variable</b>"),
              choices = c(
                "C_FE", "CI_FE", "N_FE", "O_FE", "NA_FE", "MG_FE", "AL_FE",
                "SI_FE", "S_FE", "K_FE", "CA_FE", "TI_FE", "TIII_FE", "V_FE",
                "CR_FE", "MN_FE", "FE_H", "CO_FE", "NI_FE"
              ),
              selected = "MG_FE",
              width = "90%"
            )
          )
        ),
        shiny::htmlOutput("plot_vars")
      )
    )
  )

  server <- function(input, output, session) {
    DATA_PATH <- here::here("data")
    RESULTS_PATH <- here::here("results")
    load(file.path(DATA_PATH, "astro_cleaned_data.RData"))
    clust_fit_ls <- readRDS(file.path(RESULTS_PATH, "clustering_fits.rds"))
    dr_fit_ls <- readRDS(file.path(RESULTS_PATH, "dimension_reduction_fits.rds"))

    output$plot_gloc <- shiny::renderUI({
      # get cluster ids
      pipe_key <- sprintf(
        "%s [%s + %s + %s]",
        input$clust_method, input$impute_mode, input$feature_mode, input$dr_method
      )
      k_key <- sprintf("k = %d", input$k)
      cluster_ids <- clust_fit_ls[[pipe_key]]$cluster_ids[[k_key]]

      # plot galactic location
      plt <- dplyr::bind_cols(
        metadata$train |> dplyr::select(GLON, GLAT, GC_NAME),
        cluster_id = as.factor(cluster_ids)
      ) |>
        dplyr::group_by(GC_NAME) |>
        dplyr::mutate(
          GC_NAME_STR = sprintf("%s (n = %d)", GC_NAME, dplyr::n())
        ) |>
        dplyr::ungroup() |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = GLON,
          y = GLAT,
          color = cluster_id,
          label = GC_NAME_STR
        ) +
        ggplot2::geom_jitter(size = 0.5, width = 15, height = 8) +
        ggplot2::labs(
          x = "Galactic Longitude",
          y = "Galactic Latitude",
          color = "Cluster"
        ) +
        ggplot2::theme_minimal()
      plotly::ggplotly(plt, height = 500)
    })

    output$plot_dr <- shiny::renderUI({
      # get cluster ids
      pipe_key <- sprintf(
        "%s [%s + %s + %s]",
        input$clust_method, input$impute_mode, input$feature_mode, input$dr_method
      )
      k_key <- sprintf("k = %d", input$k)
      cluster_ids <- clust_fit_ls[[pipe_key]]$cluster_ids[[k_key]]

      # get dr plot
      dr_feature_key <- stringr::str_to_lower(input$plot_feature_mode)
      dr_key <- input$plot_dr_method
      dr_plt_df <- dr_fit_ls[[dr_feature_key]][[dr_key]]$scores[, 1:2] |>
        setNames(paste0("Component ", 1:2))

      # plot galactic location
      plt <- dplyr::bind_cols(
        dr_plt_df,
        cluster_id = as.factor(cluster_ids)
      ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = `Component 1`,
          y = `Component 2`,
          color = cluster_id
        ) +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(
          color = "Cluster"
        ) +
        ggplot2::theme_minimal()
      plotly::ggplotly(plt, height = 500)
    })

    output$plot_vars <- shiny::renderUI({
      # get cluster ids
      pipe_key <- sprintf(
        "%s [%s + %s + %s]",
        input$clust_method, input$impute_mode, input$feature_mode, input$dr_method
      )
      k_key <- sprintf("k = %d", input$k)
      cluster_ids <- clust_fit_ls[[pipe_key]]$cluster_ids[[k_key]]

      # plot galactic location
      plt <- dplyr::bind_cols(
        data_rf_imputed$train,
        cluster_id = as.factor(cluster_ids)
      ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = !!rlang::sym(input$plot_var1),
          y = !!rlang::sym(input$plot_var2),
          color = cluster_id
        ) +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(
          color = "Cluster"
        ) +
        ggplot2::theme_minimal()
      plotly::ggplotly(plt, height = 500)
    })
  }

  # Create Shiny app ----
  shinyApp(ui = ui, server = server, ...)
}
