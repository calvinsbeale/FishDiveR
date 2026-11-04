# App step 7: plot each tag's depth record coloured by cluster, and example
# days from each cluster.

#' @noRd
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header("Clustering results"),
      bslib::layout_column_wrap(
        width = "300px",
        fill = FALSE,
        results_source_input(ns("source"), "Results to plot"),
        shiny::uiOutput(ns("source_note"))
      )
    ),
    bslib::card(
      bslib::card_header("Depth record by cluster"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::selectInput(ns("tdr_tag"), "Tag", choices = NULL),
          shiny::radioButtons(
            ns("downsample"), "Points to plot",
            choiceNames = list(
              shiny::tagList("Fast preview ", help_tip(paste0(
                "Splits the record into ", format(preview_bins, big.mark = ","), " time slices and plots only the ",
                "shallowest and deepest record in each, so every dive's extremes are kept."
              ))),
              "Every n records",
              "Every n seconds"
            ),
            choiceValues = c("minmax", "n", "s")
          ),
          shiny::conditionalPanel(
            "input.downsample != 'minmax'",
            ns = ns,
            shiny::numericInput(ns("every"), "n", value = 10, min = 1, step = 1)
          ),
          shiny::dateRangeInput(ns("x_lim"), "Dates"),
          bslib::layout_column_wrap(
            width = "120px",
            fill = FALSE,
            shiny::numericInput(ns("y_max"), "Max depth (m)", value = 250, min = 1),
            shiny::numericInput(ns("y_step"), "Depth ticks (m)", value = 50, min = 1)
          ),
          shiny::selectizeInput(ns("date_breaks"), "Date breaks", choices = date_break_choices, selected = "1 day", options = list(create = TRUE)),
          shiny::checkboxInput(ns("legend"), "Show legend", value = TRUE),
          shiny::actionButton(ns("create_tdr"), "Create plot", class = "btn-primary"),
          shiny::actionButton(ns("save_tdr"), "Save PNG")
        ),
        shiny::uiOutput(ns("tdr_note")),
        shiny::plotOutput(ns("tdr_plot"), height = "500px", fill = FALSE)
      )
    ),
    bslib::card(
      bslib::card_header("Example days from each cluster"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::numericInput(
            ns("no_days"),
            shiny::tagList("Days per cluster ", help_tip("The days closest to the centre of each cluster, i.e. the most typical days of that behaviour.")),
            value = 1, min = 1, step = 1
          ),
          shiny::numericInput(ns("days_every"), "Plot every n records", value = 1, min = 1, step = 1),
          bslib::layout_column_wrap(
            width = "120px",
            fill = FALSE,
            shiny::numericInput(ns("days_y_max"), "Max depth (m)", value = 250, min = 1),
            shiny::numericInput(ns("days_y_step"), "Depth ticks (m)", value = 50, min = 1)
          ),
          shiny::checkboxInput(
            ns("diel_shade"),
            shiny::tagList("Shade night ", help_tip("Shades night-time in grey. Needs diel statistics from step 4.")),
            value = FALSE
          ),
          shiny::numericInput(ns("dpi"), "Image resolution (dpi)", value = 150, min = 50, step = 50),
          shiny::actionButton(ns("create_days"), "Create plots", class = "btn-primary"),
          shiny::div(class = "text-muted small", shiny::textOutput(ns("days_time")))
        ),
        shiny::selectInput(ns("days_figure"), "Figure", choices = NULL, width = "100%"),
        shiny::uiOutput(ns("days_note")),
        shiny::imageOutput(ns("days_image"), height = "auto", fill = FALSE),
        shiny::h6("Log", class = "mt-3"),
        shiny::verbatimTextOutput(ns("log"))
      )
    )
  )
}

#' @noRd
mod_results_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    results_log <- shiny::reactiveVal(character())

    # Sources with a saved k-means result
    sources <- shiny::reactive({
      state$refresh
      shiny::req(state$project)
      s <- kmeans_sources(state$project, state$tags$tag_ID)
      s[file.exists(file.path(s, "5_k-means", "kmeans_result.rds"))]
    })

    shiny::observe({
      update_source_input(session, "source", sources(), shiny::isolate(input$source))
    })

    kmeans_result <- shiny::reactive({
      state$refresh
      shiny::req(input$source)
      file <- file.path(input$source, "5_k-means", "kmeans_result.rds")
      shiny::req(file.exists(file))
      readRDS(file)
    })

    k <- shiny::reactive(length(unique(kmeans_result()$cluster)))
    result_tags <- shiny::reactive(unique(as.character(kmeans_result()$tag_ID)))

    output$source_note <- shiny::renderUI({
      if (length(sources()) == 0) {
        return(shiny::p(class = "text-muted", "Run k-means in step 6 first."))
      }
      shiny::p(class = "text-muted mt-4", paste0(
        k(), " clusters of ", length(kmeans_result()$cluster), " days from ", paste(result_tags(), collapse = ", "), "."
      ))
    })

    output$log <- shiny::renderText({
      log <- results_log()
      if (length(log) == 0) "No result plots made yet this session." else paste(log, collapse = "\n")
    })

    ## Depth record by cluster
    shiny::observe({
      tags <- result_tags()
      current <- shiny::isolate(input$tdr_tag)
      shiny::updateSelectInput(session, "tdr_tag", choices = tags, selected = if (isTRUE(current %in% tags)) current else tags[1])
    })

    tag_summary <- function(tag) archive_summary(file.path(state$project, tag, "archive_days.rds"))

    # Reset the plot settings to suit each tag
    shiny::observeEvent(input$tdr_tag, {
      shiny::req(input$tdr_tag %in% result_tags())
      info <- tag_summary(input$tdr_tag)
      axis <- default_depth_axis(info$max_depth)
      shiny::updateNumericInput(session, "y_max", value = axis[["max"]])
      shiny::updateNumericInput(session, "y_step", value = axis[["step"]])
      shiny::updateDateRangeInput(session, "x_lim", start = info$dates[1], end = info$dates[2], min = info$dates[1], max = info$dates[2])
      shiny::updateSelectizeInput(session, "date_breaks", choices = date_break_choices, selected = default_date_breaks(as.numeric(diff(info$dates)) + 1))
    })

    # Example day depth axis to suit the deepest tag
    shiny::observe({
      max_depth <- max(vapply(result_tags(), function(tag) tag_summary(tag)$max_depth, numeric(1)))
      axis <- default_depth_axis(max_depth)
      shiny::updateNumericInput(session, "days_y_max", value = axis[["max"]])
      shiny::updateNumericInput(session, "days_y_step", value = axis[["step"]])
    })

    tdr_settings <- shiny::reactive(list(
      source = input$source,
      tag = input$tdr_tag,
      downsample = input$downsample,
      every = input$every,
      x_lim = input$x_lim,
      y_max = input$y_max,
      y_step = input$y_step,
      date_breaks = input$date_breaks,
      legend = isTRUE(input$legend)
    ))
    plotted_settings <- shiny::reactiveVal(NULL)

    tdr_plot <- shiny::bindEvent(shiny::reactive({
      s <- tdr_settings()
      plotted_settings(s)
      shiny::validate(shiny::need(s$tag, "Run k-means in step 6 first."))
      shiny::validate(shiny::need(
        (s$downsample == "minmax" || isTRUE(s$every > 0)) && isTRUE(s$y_max > 0) && isTRUE(s$y_step > 0) && isTRUE(nzchar(s$date_breaks)),
        "Fill in all the plot settings."
      ))
      dates <- tag_summary(s$tag)$dates
      x_lim <- as.Date(s$x_lim)
      x_lim <- if (length(x_lim) != 2 || anyNA(x_lim) || identical(x_lim, dates)) NULL else as.character(x_lim)

      res <- run_logged(plot_cluster_TDR(
        tag_ID = s$tag,
        data_folder = state$project,
        kmeans_result = kmeans_result(),
        every_nth = if (s$downsample == "n") s$every else 1,
        every_s = if (s$downsample == "s") s$every else 0,
        minmax_bins = if (s$downsample == "minmax") preview_bins else 0,
        X_lim = x_lim,
        Y_lim = c(0, s$y_max, s$y_step),
        date_breaks = s$date_breaks,
        legend = s$legend
      ))
      shiny::validate(shiny::need(res$ok, paste(res$log, collapse = "\n")))
      res$value
    }), input$create_tdr)

    output$tdr_note <- shiny::renderUI({
      plotted <- plotted_settings()
      if (is.null(plotted)) {
        shiny::p(class = "text-muted", "Choose the plot settings, then click Create plot.")
      } else if (!identical(plotted, tdr_settings())) {
        shiny::p(class = "text-warning", "Settings have changed. Click Create plot to update the plot.")
      }
    })

    output$tdr_plot <- shiny::renderPlot(
      {
        shiny::req(input$create_tdr)
        tdr_plot()
      },
      res = 96
    )

    shiny::observeEvent(input$save_tdr, {
      plotted <- plotted_settings()
      if (is.null(plotted)) {
        shiny::showNotification("Create a plot first.", type = "error")
        return()
      }
      file <- file.path(plotted$source, "5_k-means", paste0(plotted$tag, "_TDR_k=", k(), ".png"))
      saved <- tryCatch(
        {
          ggplot2::ggsave(file, plot = tdr_plot(), width = 12, height = 6, dpi = 300)
          TRUE
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
          FALSE
        }
      )
      if (saved) shiny::showNotification(paste("Saved", file))
    })

    ## Example days from each cluster
    days_time <- shiny::reactiveVal(NULL)

    days_folder <- shiny::reactive({
      shiny::req(input$source)
      file.path(input$source, paste0("6_Cluster-plots.K=", k(), if (isTRUE(input$diel_shade)) "_shaded" else ""))
    })

    shiny::observeEvent(input$create_days, {
      if (!isTRUE(input$no_days >= 1) || !isTRUE(input$days_every >= 1) || !isTRUE(input$days_y_max > 0) ||
        !isTRUE(input$days_y_step > 0) || !isTRUE(input$dpi > 0)) {
        shiny::showNotification("Fill in all the plot settings.", type = "error")
        return()
      }
      started <- Sys.time()
      res <- shiny::withProgress(message = "Plotting example days", value = 0.5, run_logged({
        plot_clusters(
          tag_vector = result_tags(),
          data_folder = state$project,
          kmeans_result = kmeans_result(),
          No_days = round(input$no_days),
          every_nth = round(input$days_every),
          Y_lim = c(0, input$days_y_max, input$days_y_step),
          diel_shade = isTRUE(input$diel_shade),
          dpi = input$dpi,
          output = TRUE,
          output_folder = state$project,
          verbose = TRUE
        )
        invisible(NULL)
      }))
      took <- format_duration(as.numeric(difftime(Sys.time(), started, units = "secs")))
      days_time(paste0(if (res$ok) "Last plots took " else "Failed after ", took, if (isTRUE(input$diel_shade)) " (with night shading)." else "."))
      results_log(c(paste0("== Example days: ", if (res$ok) "done" else "FAILED", " (", took, ")"), res$log))
      state$refresh <- state$refresh + 1
    })

    output$days_time <- shiny::renderText(days_time())

    days_files <- shiny::reactive({
      state$refresh
      files <- list.files(days_folder(), pattern = "\\.png$")
      first <- intersect(c("Cluster_facet.png", "Cluster_facet_freeY.png"), files)
      c(first, setdiff(sort(files), first))
    })

    shiny::observe({
      files <- days_files()
      names(files) <- gsub("[._]", " ", sub("\\.png$", "", files))
      current <- shiny::isolate(input$days_figure)
      shiny::updateSelectInput(session, "days_figure", choices = files, selected = if (isTRUE(current %in% files)) current else files[1])
    })

    output$days_note <- shiny::renderUI({
      if (length(days_files()) == 0) {
        shiny::p(class = "text-muted", "Click Create plots to plot the most typical days of each cluster.")
      }
    })

    output$days_image <- shiny::renderImage(
      {
        state$refresh
        shiny::req(input$days_figure)
        file <- file.path(days_folder(), input$days_figure)
        shiny::req(file.exists(file))
        list(src = file, contentType = "image/png", style = "max-width: 100%; max-height: 80vh;", alt = input$days_figure)
      },
      deleteFile = FALSE
    )
  })
}
