# App step 6: choose the number of clusters and cluster the days with k-means.

#' Select input of the PCA or clustering results in a project
#' @noRd
results_source_input <- function(id, label) {
  shiny::selectInput(id, label, choices = NULL, width = "100%")
}

#' Keep a results source select input up to date
#' @noRd
update_source_input <- function(session, id, sources, current) {
  shiny::updateSelectInput(
    session, id,
    choices = sources,
    selected = if (isTRUE(current %in% sources)) current else unname(sources[1])
  )
}

#' Cluster colours, as used by k_clustering() and the cluster plots
#' @noRd
cluster_colours <- function(k) {
  colours <- c(
    "#E41A1C", "#377EB8", "#FF7F00", "#984EA3", "#4DAF4A", "#00CED1", "#A65628", "#F781BF",
    "#999999", "#344111", "#8A2BE2", "#FF69B4", "#CD5C5C", "#7FFF00", "#D2691E"
  )
  if (k > length(colours)) {
    colours <- c(colours, colorspace::qualitative_hcl(k, palette = "Set3")[(length(colours) + 1):k])
  }
  colours[seq_len(k)]
}

#' Days per cluster for each tag, as a wide table
#' @noRd
cluster_day_counts <- function(kmeans_result) {
  counts <- table(Tag = kmeans_result$tag_ID, Cluster = kmeans_result$cluster)
  wide <- as.data.frame.matrix(counts)
  names(wide) <- paste("Cluster", names(wide))
  wide <- cbind(`Tag ID` = rownames(wide), wide, Total = rowSums(counts))
  rownames(wide) <- NULL
  wide
}

#' @noRd
mod_cluster_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, xl = c(5, 7)),
    bslib::card(
      bslib::card_header("K-means clustering"),
      shiny::p(
        "Groups the days into clusters of similar vertical movement behaviour, using the principal components",
        "and daily depth statistics from the previous steps."
      ),
      results_source_input(ns("source"), "PCA results to cluster"),
      shiny::uiOutput(ns("data_note")),
      shiny::checkboxInput(
        ns("standardise"),
        shiny::tagList("Standardise the variables ", help_tip("Scales the principal components and depth statistics to the same range before clustering. Recommended.")),
        value = TRUE
      ),
      shiny::numericInput(
        ns("seed"),
        shiny::tagList("Random seed ", help_tip("K-means starts from random cluster centres. A fixed seed gives the same clusters every time you run it with the same settings.")),
        value = 123, min = 0, step = 1, width = "200px"
      ),
      shiny::h6("1. Choose the number of clusters", class = "mt-2"),
      bslib::layout_column_wrap(
        width = "180px",
        fill = FALSE,
        shiny::numericInput(ns("max_k"), "Largest number to try", value = 10, min = 2, step = 1),
        shiny::checkboxInput(
          ns("calc_gap"),
          shiny::tagList("Gap statistic ", help_tip("Adds a third measure for choosing k. It is slow, as it re-runs k-means on 100 simulated data sets.")),
          value = FALSE
        )
      ),
      shiny::div(shiny::actionButton(ns("plot_k"), "Plot elbow and silhouette", class = "btn-primary")),
      shiny::plotOutput(ns("k_plot"), height = "260px", fill = FALSE),
      shiny::p(
        class = "text-muted small",
        "Choose a number near the 'elbow', where more clusters stop reducing the within-cluster sum of squares by much,",
        "and where the silhouette width is high. The red line marks the number of clusters below."
      ),
      shiny::h6("2. Cluster the days"),
      bslib::layout_column_wrap(
        width = "180px",
        fill = FALSE,
        shiny::numericInput(ns("k"), "Number of clusters (k)", value = 4, min = 2, step = 1),
        shiny::numericInput(
          ns("nstart"),
          shiny::tagList("Random starts ", help_tip("K-means is run this many times from different random centres and the best result kept. 50 or more is recommended.")),
          value = 50, min = 1, step = 1
        )
      ),
      shiny::checkboxInput(ns("polygon"), "Shade each cluster in the 3D view", value = TRUE),
      shiny::div(shiny::actionButton(ns("run_kmeans"), "Run k-means", class = "btn-primary")),
      shiny::h6("Log", class = "mt-3"),
      shiny::verbatimTextOutput(ns("log"))
    ),
    bslib::navset_card_underline(
      title = "Clusters",
      bslib::nav_panel(
        "3D view",
        shiny::uiOutput(ns("scene_note")),
        # Fixed height, so the widget doesn't stretch to fill the card
        shiny::div(style = "height: 500px;", rgl::rglwidgetOutput(ns("cluster_3d"), width = "100%", height = "500px")),
        shiny::uiOutput(ns("cluster_legend"))
      ),
      bslib::nav_panel(
        "Cluster variables",
        shiny::uiOutput(ns("variables_note")),
        shiny::imageOutput(ns("variables_image"), height = "auto", fill = FALSE)
      ),
      bslib::nav_panel(
        "Days per cluster",
        shiny::uiOutput(ns("days_note")),
        DT::DTOutput(ns("days_table"), fill = FALSE)
      )
    )
  )
}

#' @noRd
mod_cluster_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cluster_log <- shiny::reactiveVal(character())
    k_plot <- shiny::reactiveVal(NULL)
    scene <- shiny::reactiveVal(NULL)

    sources <- shiny::reactive({
      state$refresh
      shiny::req(state$project)
      kmeans_sources(state$project, state$tags$tag_ID)
    })

    shiny::observe({
      update_source_input(session, "source", sources(), shiny::isolate(input$source))
    })

    kmeans_data <- shiny::reactive({
      state$refresh
      shiny::req(input$source)
      file <- file.path(input$source, "5_k-means", "combined_stats.rds")
      shiny::req(file.exists(file))
      readRDS(file)
    })

    # The latest k-means result saved for this source
    saved_kmeans <- shiny::reactive({
      state$refresh
      shiny::req(input$source)
      file <- file.path(input$source, "5_k-means", "kmeans_result.rds")
      if (file.exists(file)) readRDS(file) else NULL
    })

    output$data_note <- shiny::renderUI({
      if (length(sources()) == 0) {
        return(shiny::p(class = "text-muted", "Run the PCA in step 5 first."))
      }
      data <- kmeans_data()
      n_pcs <- length(grep("^PC[0-9]+$", names(data)))
      n_vars <- sum(vapply(data, is.numeric, logical(1)))
      shiny::p(class = "text-muted small", paste0(
        nrow(data), " days from ", length(unique(data$tag_ID)), " tag(s): ", n_pcs, " principal components and ",
        n_vars - n_pcs, " depth statistics."
      ))
    })

    output$log <- shiny::renderText({
      log <- cluster_log()
      if (length(log) == 0) "No clustering run yet this session." else paste(log, collapse = "\n")
    })

    set_seed <- function() {
      if (isTRUE(input$seed >= 0)) set.seed(round(input$seed))
    }

    ## 1. Elbow and silhouette plots
    shiny::observeEvent(input$plot_k, {
      data <- kmeans_data()
      if (!isTRUE(input$max_k >= 2)) {
        shiny::showNotification("Enter a largest number of clusters of 2 or more.", type = "error")
        return()
      }
      res <- shiny::withProgress(message = "Trying numbers of clusters", value = 0.5, run_logged({
        set_seed()
        select_k(
          data, standardise = isTRUE(input$standardise), Max.k = round(input$max_k),
          v_line = if (isTRUE(input$k >= 1)) round(input$k) else NULL,
          calc_gap = isTRUE(input$calc_gap), plot_gap = isTRUE(input$calc_gap),
          output = TRUE, output_folder = state$project, verbose = TRUE
        )
      }))
      cluster_log(c(paste0("== Choosing k: ", if (res$ok) "done" else "FAILED"), res$log))
      if (res$ok) k_plot(res$value)
    })

    output$k_plot <- shiny::renderPlot(
      {
        shiny::req(k_plot())
        k_plot()
      },
      res = 96
    )

    ## 2. K-means
    shiny::observeEvent(input$run_kmeans, {
      data <- kmeans_data()
      k <- round(input$k)
      if (!isTRUE(k >= 2 && k < nrow(data))) {
        shiny::showNotification(paste0("Choose between 2 and ", nrow(data) - 1, " clusters."), type = "error")
        return()
      }
      has_3d <- all(c("PC1", "PC2", "PC3") %in% names(data))

      res <- shiny::withProgress(message = "Clustering days", value = 0.5, run_logged({
        set_seed()
        # Draw the 3D plot off screen, then show it in the app
        rgl::open3d(useNULL = TRUE)
        tryCatch(
          {
            k_clustering(
              data, standardise = isTRUE(input$standardise), k = k, nstart = round(input$nstart),
              polygon = isTRUE(input$polygon), output = TRUE, output_folder = state$project, verbose = TRUE
            )
            # Replace rgl's legend, drawn as a large background image, with
            # one under the 3D view
            if (has_3d) {
              rgl::bg3d(color = "white")
              rgl::scene3d()
            }
          },
          finally = rgl::close3d()
        )
      }))
      cluster_log(c(paste0("== K-means with k = ", k, ": ", if (res$ok) "done" else "FAILED"), res$log))
      if (res$ok) scene(res$value)
      state$refresh <- state$refresh + 1
    })

    ## Results
    output$scene_note <- shiny::renderUI({
      if (is.null(scene())) {
        shiny::p(class = "text-muted", "Run k-means to see each day plotted on the first three principal components, coloured by cluster. Drag to rotate, scroll to zoom.")
      }
    })

    output$cluster_3d <- rgl::renderRglwidget({
      shiny::req(scene())
      rgl::rglwidget(scene())
    })

    output$cluster_legend <- shiny::renderUI({
      shiny::req(scene())
      km <- saved_kmeans()
      shiny::req(km)
      k <- length(unique(km$cluster))
      shiny::div(
        class = "d-flex flex-wrap gap-3 justify-content-center small",
        lapply(seq_len(k), function(i) {
          shiny::span(
            shiny::span(style = paste0("display:inline-block;width:12px;height:12px;margin-right:4px;background:", cluster_colours(k)[i], ";")),
            paste("Cluster", i)
          )
        })
      )
    })

    variables_file <- shiny::reactive({
      km <- saved_kmeans()
      shiny::req(km)
      k <- length(unique(km$cluster))
      file.path(input$source, "5_k-means", paste0("cluster_variables_k=", k, "_colour.png"))
    })

    output$variables_note <- shiny::renderUI({
      if (is.null(saved_kmeans())) {
        shiny::p(class = "text-muted", "Run k-means to see the mean standardised value of each variable in each cluster.")
      }
    })

    output$variables_image <- shiny::renderImage(
      {
        file <- variables_file()
        shiny::req(file.exists(file))
        list(src = file, contentType = "image/png", width = "100%", alt = "Mean standardised value of each variable by cluster")
      },
      deleteFile = FALSE
    )

    output$days_note <- shiny::renderUI({
      if (is.null(saved_kmeans())) {
        shiny::p(class = "text-muted", "Run k-means to see how many days of each tag are in each cluster.")
      }
    })

    output$days_table <- DT::renderDT({
      km <- saved_kmeans()
      shiny::req(km)
      DT::datatable(cluster_day_counts(km), rownames = FALSE, selection = "none", fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })
  })
}
