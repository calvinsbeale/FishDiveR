# User interface and top-level server for the FishDiveR app (run_app).
# Each pipeline step is a shiny module in its own app_mod_*.R file.

#' App user interface
#' @noRd
app_ui <- function() {
  bslib::page_navbar(
    title = "FishDiveR",
    window_title = "FishDiveR",
    id = "steps",
    theme = bslib::bs_theme(version = 5, primary = "#1B6A8F"),
    # Let pages grow and scroll, so stacked cards in narrow windows don't overlap
    fillable = FALSE,
    bslib::nav_panel(
      "1. Project & tags",
      value = "project",
      mod_project_ui("project"),
      next_button("project")
    ),
    bslib::nav_panel(
      "2. Import",
      value = "import",
      mod_import_ui("import"),
      next_button("import")
    ),
    bslib::nav_panel(
      "3. Wavelets",
      value = "wavelets",
      mod_wavelet_ui("wavelet"),
      next_button("wavelets")
    ),
    bslib::nav_panel(
      "4. Daily stats",
      value = "stats",
      placeholder_panel(
        "Daily statistics",
        "Create daily wavelet statistics and daily and diel depth statistics for each tag.",
        c("create_wavelet_stats()", "create_depth_stats()")
      ),
      next_button("stats")
    ),
    bslib::nav_panel(
      "5. PCA",
      value = "pca",
      placeholder_panel(
        "Principal component analysis",
        "Combine the wavelet statistics of the selected tags, choose the principal components to keep, and view the loadings.",
        c("pca_data()", "pca_results()", "pca_scores()", "combine_data()")
      ),
      next_button("pca")
    ),
    bslib::nav_panel(
      "6. Clustering",
      value = "clustering",
      placeholder_panel(
        "K-means clustering",
        "Choose the number of clusters from the elbow and silhouette plots, then cluster the days of all selected tags.",
        c("select_k()", "k_clustering()")
      ),
      next_button("clustering")
    ),
    bslib::nav_panel(
      "7. Results",
      value = "results",
      placeholder_panel(
        "Results",
        "Plot each tag's depth record coloured by cluster, and example days from each behaviour cluster.",
        c("plot_cluster_TDR()", "plot_clusters()")
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::textOutput("project_label", inline = TRUE))
  )
}

#' Panel for a step that has not been built yet
#' @noRd
placeholder_panel <- function(title, description, functions) {
  bslib::card(
    bslib::card_header(title),
    shiny::p(description),
    shiny::p(
      shiny::em("Coming soon. Until then, use "),
      shiny::code(paste(functions, collapse = ", ")),
      shiny::em(" in R with the project folder as output_folder.")
    )
  )
}

#' App server
#'
#' @param project_folder Optional project folder to open on launch.
#' @noRd
app_server <- function(project_folder = NULL) {
  function(input, output, session) {
    # Shared state across steps: the project folder, the tag table, and a
    # counter bumped whenever a step writes new outputs
    state <- shiny::reactiveValues(
      project = NULL,
      tags = empty_tag_metadata(),
      refresh = 0
    )

    if (!is.null(project_folder)) {
      project <- normalizePath(project_folder, winslash = "/")
      state$project <- project
      state$tags <- read_tag_metadata(file.path(project, "tag_metadata.csv"))
    }

    output$project_label <- shiny::renderText({
      if (is.null(state$project)) "No project folder open" else paste("Project:", state$project)
    })

    # Next buttons move to the following step
    lapply(seq_len(length(app_steps) - 1), function(i) {
      shiny::observeEvent(input[[paste0("next_", app_steps[i])]], {
        bslib::nav_select("steps", app_steps[i + 1])
      })
    })

    mod_project_server("project", state)
    mod_import_server("import", state)
    mod_wavelet_server("wavelet", state)
  }
}
