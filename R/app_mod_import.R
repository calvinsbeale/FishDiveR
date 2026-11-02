# App step 2: import tag archives and plot each tag's depth record.

# Time bins for the fast preview: about two points per pixel across the plot
preview_bins <- 2000

# Date break choices offered for the depth record plot; others can be typed
date_break_choices <- c("6 hour", "12 hour", "1 day", "2 day", "7 day", "14 day", "1 month", "3 month", "6 month")

#' @noRd
mod_import_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, xl = c(4, 8)),
    bslib::card(
      bslib::card_header("Import tag archives"),
      shiny::p("Crops each archive to full local days between deployment and release, and saves it to the tag's folder."),
      DT::DTOutput(ns("tag_table"), fill = FALSE),
      shiny::div(
        shiny::actionButton(ns("import_selected"), "Import selected", class = "btn-primary"),
        shiny::actionButton(ns("import_all"), "Import all"),
        shiny::actionButton(ns("remove_selected"), "Remove selected")
      ),
      shiny::h6("Log", class = "mt-3"),
      shiny::verbatimTextOutput(ns("log"))
    ),
    bslib::card(
      bslib::card_header("Depth record"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::selectInput(ns("plot_tag"), "Tag", choices = NULL),
          shiny::radioButtons(
            ns("downsample"), "Points to plot",
            choices = c(
              "Fast preview (deepest and shallowest)" = "minmax",
              "Every n records" = "n",
              "Every n seconds" = "s"
            )
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
          shiny::selectizeInput(
            ns("date_breaks"), "Date breaks",
            choices = date_break_choices, selected = "1 day",
            options = list(create = TRUE)
          ),
          shiny::checkboxInput(ns("no_axis_titles"), "Remove axis titles"),
          shiny::actionButton(ns("create_plot"), "Create plot", class = "btn-primary"),
          shiny::actionButton(ns("save_plot"), "Save PNG to tag folder")
        ),
        shiny::uiOutput(ns("plot_note")),
        shiny::plotOutput(ns("tdr_plot"), height = "500px", fill = FALSE)
      )
    )
  )
}

#' @noRd
mod_import_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    import_log <- shiny::reactiveVal(character())
    remove_tags <- remove_tags_server(input, session, state)

    status <- shiny::reactive({
      state$refresh
      shiny::req(state$project)
      tag_status(state$tags, state$project)
    })

    output$tag_table <- DT::renderDT({
      shiny::validate(shiny::need(state$project, "Open a project folder in step 1."))
      shiny::validate(shiny::need(nrow(state$tags) > 0, "Add tags in step 1."))
      display <- data.frame(
        `Tag ID` = status()$tag_ID,
        Imported = ifelse(status()$Imported, "Yes", ""),
        check.names = FALSE
      )
      DT::datatable(display, selection = "multiple", rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })

    output$log <- shiny::renderText({
      log <- import_log()
      if (length(log) == 0) "Nothing imported yet this session." else paste(log, collapse = "\n")
    })

    selected_rows <- function() {
      rows <- input$tag_table_rows_selected
      if (length(rows) == 0) {
        shiny::showNotification("Select one or more tags in the table first.", type = "error")
      }
      rows
    }

    import_tags <- function(tags) {
      log <- character()
      shiny::withProgress(message = "Importing", value = 0, {
        for (i in seq_len(nrow(tags))) {
          tag <- tags[i, ]
          shiny::incProgress(1 / nrow(tags), detail = tag$tag_ID)

          problems <- validate_tag(tag)
          if (length(problems) > 0) {
            log <- c(log, paste0("== ", tag$tag_ID, ": skipped (", paste(problems, collapse = "; "), ")"), "")
            next
          }

          res <- run_logged(import_tag_data(
            tag_ID = tag$tag_ID,
            tag_deploy_UTC = tag$tag_deploy_UTC,
            tag_release_UTC = tag$tag_release_UTC,
            archive = tag$archive,
            date_time_col = tag$date_time_col,
            depth_col = tag$depth_col,
            temp_col = tag$temp_col,
            time_zone = tag$time_zone,
            output = TRUE,
            output_folder = state$project,
            verbose = TRUE
          ))
          log <- c(log, paste0("== ", tag$tag_ID, ": ", if (res$ok) "imported" else "FAILED"), res$log, "")
        }
      })
      import_log(log)
      state$refresh <- state$refresh + 1
    }

    shiny::observeEvent(input$import_selected, {
      rows <- selected_rows()
      if (length(rows) > 0) import_tags(state$tags[rows, , drop = FALSE])
    })

    shiny::observeEvent(input$import_all, {
      shiny::req(state$project, nrow(state$tags) > 0)
      import_tags(state$tags)
    })

    shiny::observeEvent(input$remove_selected, {
      rows <- selected_rows()
      if (length(rows) > 0) remove_tags(state$tags$tag_ID[rows])
    })

    ## Depth record plot
    imported_tags <- shiny::reactive(status()$tag_ID[status()$Imported])

    shiny::observe({
      choices <- imported_tags()
      current <- shiny::isolate(input$plot_tag)
      shiny::updateSelectInput(session, "plot_tag", choices = choices, selected = if (isTRUE(current %in% choices)) current else choices[1])
    })

    archive_info <- shiny::reactive({
      shiny::req(input$plot_tag)
      state$refresh
      archive <- readRDS(file.path(state$project, input$plot_tag, "archive_days.rds"))
      list(max_depth = max(archive$depth), dates = range(archive$date_only))
    })

    # Reset the plot settings to suit each tag
    shiny::observeEvent(archive_info(), {
      info <- archive_info()
      axis <- default_depth_axis(info$max_depth)
      shiny::updateNumericInput(session, "y_max", value = axis[["max"]])
      shiny::updateNumericInput(session, "y_step", value = axis[["step"]])
      shiny::updateDateRangeInput(session, "x_lim", start = info$dates[1], end = info$dates[2], min = info$dates[1], max = info$dates[2])
      breaks <- default_date_breaks(as.numeric(diff(info$dates)) + 1)
      shiny::updateSelectizeInput(session, "date_breaks", choices = union(date_break_choices, input$date_breaks), selected = breaks)
    })

    # Plot settings. The plot is only redrawn when Create plot is clicked, as
    # large archives can take a while to plot
    plot_settings <- shiny::reactive(list(
      tag = input$plot_tag,
      downsample = input$downsample,
      every = input$every,
      x_lim = input$x_lim,
      y_max = input$y_max,
      y_step = input$y_step,
      date_breaks = input$date_breaks,
      no_axis_titles = input$no_axis_titles
    ))
    plotted_settings <- shiny::reactiveVal(NULL)

    run_plot_TDR <- function(settings) {
      dates <- archive_info()$dates
      x_lim <- as.Date(settings$x_lim)
      x_lim <- if (length(x_lim) != 2 || anyNA(x_lim) || identical(x_lim, dates)) NULL else as.character(x_lim)

      run_logged(plot_TDR(
        rds_file = file.path(settings$tag, "archive_days.rds"),
        data_folder = state$project,
        every_nth = if (settings$downsample == "n") settings$every else 1,
        every_s = if (settings$downsample == "s") settings$every else 0,
        minmax_bins = if (settings$downsample == "minmax") preview_bins else 0,
        X_lim = x_lim,
        Y_lim = c(0, settings$y_max, settings$y_step),
        date_breaks = settings$date_breaks
      ))
    }

    tdr_plot <- shiny::bindEvent(shiny::reactive({
      settings <- plot_settings()
      plotted_settings(settings)
      shiny::validate(shiny::need(settings$tag, "Import a tag to plot its depth record."))
      shiny::validate(shiny::need(
        (settings$downsample == "minmax" || isTRUE(settings$every > 0)) &&
          isTRUE(settings$y_max > 0) && isTRUE(settings$y_step > 0) &&
          isTRUE(nzchar(settings$date_breaks)),
        "Fill in all the plot settings."
      ))

      res <- run_plot_TDR(settings)
      shiny::validate(shiny::need(res$ok, paste(res$log, collapse = "\n")))

      plot <- attr(res$value, "TDR_plot")
      if (isTRUE(settings$no_axis_titles)) {
        plot <- plot + ggplot2::theme(axis.title = ggplot2::element_blank())
      }
      plot
    }), input$create_plot)

    output$plot_note <- shiny::renderUI({
      plotted <- plotted_settings()
      if (is.null(plotted)) {
        shiny::p(class = "text-muted", "Choose the plot settings, then click Create plot.")
      } else if (!identical(plotted, plot_settings())) {
        shiny::p(class = "text-warning", "Settings have changed. Click Create plot to update the plot.")
      }
    })

    output$tdr_plot <- shiny::renderPlot(
      {
        shiny::req(input$create_plot)
        tdr_plot()
      },
      res = 96
    )

    # Save the plot as shown, at the plot_TDR() default size
    shiny::observeEvent(input$save_plot, {
      plotted <- plotted_settings()
      if (is.null(plotted)) {
        shiny::showNotification("Create a plot first.", type = "error")
        return()
      }
      file <- file.path(state$project, plotted$tag, "tag_archive.png")
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
  })
}
