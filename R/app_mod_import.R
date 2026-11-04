# App step 2: import tag archives and plot each tag's depth record.

# Time bins for the fast preview: about two points per pixel across the plot
preview_bins <- 2000

# Date break choices offered for the depth record plot; others can be typed
date_break_choices <- c("6 hour", "12 hour", "1 day", "2 day", "7 day", "14 day", "28 day", "1 month", "2 month", "3 month", "6 month")

#' @noRd
mod_import_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, xl = c(4, 8)),
    bslib::card(
      bslib::card_header("Import tag archives"),
      shiny::p("Crops each archive to full local days between deployment and release, and saves it to the tag's folder."),
      shiny::uiOutput(ns("outdated_note")),
      DT::DTOutput(ns("tag_table"), fill = FALSE),
      shiny::div(
        shiny::actionButton(ns("import_selected"), "Import selected", class = "btn-primary"),
        shiny::actionButton(ns("import_all"), "Import all"),
        shiny::actionButton(ns("remove_selected"), "Remove selected")
      ),
      shiny::uiOutput(ns("sampling_details")),
      shiny::h6("Log", class = "mt-3"),
      shiny::verbatimTextOutput(ns("log"))
    ),
    bslib::navset_card_underline(
      bslib::nav_panel(
        "Depth record",
        depth_record_panel(ns)
      ),
      bslib::nav_panel(
        "Time at depth & temperature",
        time_at_panel(ns)
      )
    )
  )
}

#' Sidebar and plot of the depth record tab
#' @noRd
depth_record_panel <- function(ns) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("plot_tag"), "Tag", choices = NULL),
      shiny::radioButtons(
        ns("downsample"), "Points to plot",
        choiceNames = list(
          shiny::tagList(
            "Fast preview ",
            help_tip(paste0(
              "Splits the record into ", format(preview_bins, big.mark = ","), " time slices and plots only the ",
              "shallowest and deepest record in each. Every dive's extremes are kept, and long deployments plot ",
              "in about a second."
            ))
          ),
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
      shiny::selectizeInput(
        ns("date_breaks"), "Date breaks",
        choices = date_break_choices, selected = "1 day",
        options = list(create = TRUE)
      ),
      shiny::checkboxInput(ns("colour_temp"), shiny::tagList(
        "Colour by temperature ",
        help_tip("Colours the depth record from blue (cold) to red (warm). Needs the tag's temperature column, set in step 1.")
      )),
      shiny::checkboxInput(ns("no_axis_titles"), "Remove axis titles"),
      shiny::actionButton(ns("create_plot"), "Create plot", class = "btn-primary"),
      shiny::actionButton(ns("save_plot"), "Save PNG to tag folder")
    ),
    shiny::uiOutput(ns("plot_note")),
    shiny::plotOutput(ns("tdr_plot"), height = "500px", fill = FALSE)
  )
}

#' Sidebar and plot of the time at depth and temperature tab
#' @noRd
time_at_panel <- function(ns) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::selectInput(ns("ta_tag"), "Tag", choices = NULL),
      shiny::radioButtons(ns("ta_variable"), "Time at", choices = c(Depth = "depth", Temperature = "temp")),
      shiny::radioButtons(ns("ta_by"), "For", choices = c("The whole record" = "all", "Each month" = "month", "Each day" = "day")),
      shiny::numericInput(
        ns("ta_width"),
        shiny::tagList(
          shiny::HTML("Range width (m or &deg;C) "),
          help_tip(paste(
            "Width of each depth or temperature range. Leave blank for about 12 ranges of round values covering the",
            "middle 99.8% of the record, with one wider range at each end for the rest."
          ))
        ),
        value = NA, min = 0
      ),
      shiny::checkboxInput(ns("ta_no_axis_titles"), "Remove axis titles"),
      shiny::actionButton(ns("ta_create"), "Create plot", class = "btn-primary"),
      shiny::actionButton(ns("ta_save"), "Save PNG to tag folder")
    ),
    shiny::uiOutput(ns("ta_note")),
    shiny::uiOutput(ns("ta_plot_ui"))
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
        Imported = ifelse(status()$Imported, ifelse(nzchar(status()$Settings_changed), "Outdated", "Yes"), ""),
        Sampling = vapply(seq_len(nrow(status())), function(i) {
          if (status()$Imported[i]) sampling_status(tag_sampling(status()$tag_ID[i])) else ""
        }, character(1)),
        check.names = FALSE
      )
      DT::datatable(display, selection = "multiple", rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })

    # Tags whose settings changed in step 1 since they were imported
    output$outdated_note <- shiny::renderUI({
      outdated <- status()[nzchar(status()$Settings_changed), , drop = FALSE]
      shiny::req(nrow(outdated) > 0)
      shiny::div(
        class = "alert alert-warning",
        shiny::strong("Re-import to use the current settings:"),
        shiny::tags$ul(lapply(seq_len(nrow(outdated)), function(i) {
          shiny::tags$li(paste0(outdated$tag_ID[i], ": ", outdated$Settings_changed[i], " changed since import"))
        }))
      )
    })

    # Sampling report of an imported tag (cached with the archive summary)
    tag_sampling <- function(tag) archive_summary(file.path(state$project, tag, "archive_days.rds"))$sampling

    output$sampling_details <- shiny::renderUI({
      rows <- input$tag_table_rows_selected
      shiny::req(length(rows) == 1, isTRUE(status()$Imported[rows]))
      tag <- status()$tag_ID[rows]
      shiny::tagList(
        shiny::h6(
          paste("Sampling check:", tag),
          help_tip("Checks the imported record for a constant sampling interval. See ?sampling_report for what each item means."),
          class = "mt-3"
        ),
        shiny::tags$pre(paste(utils::capture.output(print(tag_sampling(tag), max_rows = 20)), collapse = "\n"))
      )
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
      list(max_depth = max(archive$depth), dates = range(archive$date_only), has_temp = has_temperature(archive))
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
      colour_temp = input$colour_temp,
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
        date_breaks = settings$date_breaks,
        colour_temp = isTRUE(settings$colour_temp)
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
      shiny::validate(shiny::need(!isTRUE(settings$colour_temp) || archive_info()$has_temp, no_temperature_message))

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

    ## Time at depth and temperature
    shiny::observe({
      choices <- imported_tags()
      current <- shiny::isolate(input$ta_tag)
      shiny::updateSelectInput(session, "ta_tag", choices = choices, selected = if (isTRUE(current %in% choices)) current else choices[1])
    })

    ta_settings <- shiny::reactive(list(
      tag = input$ta_tag,
      variable = input$ta_variable,
      by = input$ta_by,
      width = input$ta_width,
      no_axis_titles = input$ta_no_axis_titles
    ))
    ta_plotted <- shiny::reactiveVal(NULL)

    time_at_plot <- shiny::bindEvent(shiny::reactive({
      s <- ta_settings()
      ta_plotted(s)
      shiny::validate(shiny::need(s$tag, "Import a tag to plot its time at depth or temperature."))
      shiny::validate(shiny::need(is.na(s$width) || isTRUE(s$width > 0), "The range width must be blank or a positive number."))

      archive <- readRDS(file.path(state$project, s$tag, "archive_days.rds"))
      if (s$variable == "temp") shiny::validate(shiny::need(has_temperature(archive), no_temperature_message))
      breaks <- if (is.na(s$width)) NULL else width_breaks(archive[[s$variable]], s$width, s$variable)
      shiny::validate(shiny::need(
        length(breaks) <= 201,
        paste("That range width gives", length(breaks) - 1, "ranges. Use a wider range for at most 200.")
      ))

      res <- run_logged(plot_time_at(archive, variable = s$variable, breaks = breaks, by = s$by))
      shiny::validate(shiny::need(res$ok, paste(res$log, collapse = "\n")))
      plot <- attr(res$value, "time_at_plot")
      if (isTRUE(s$no_axis_titles)) {
        plot <- plot + ggplot2::theme(axis.title = ggplot2::element_blank())
      }
      plot
    }), input$ta_create)

    # Size of the plot as shown and saved: taller for a panel per month
    ta_size <- function(by) if (identical(by, "month")) c(12, 9) else c(8, 6)

    output$ta_note <- shiny::renderUI({
      plotted <- ta_plotted()
      if (is.null(plotted)) {
        shiny::p(class = "text-muted", "Choose the plot settings, then click Create plot.")
      } else if (!identical(plotted, ta_settings())) {
        shiny::p(class = "text-warning", "Settings have changed. Click Create plot to update the plot.")
      }
    })

    output$ta_plot_ui <- shiny::renderUI({
      height <- if (identical(ta_plotted()$by, "month")) "750px" else "500px"
      shiny::plotOutput(session$ns("ta_plot"), height = height, fill = FALSE)
    })

    output$ta_plot <- shiny::renderPlot(
      {
        shiny::req(input$ta_create)
        time_at_plot()
      },
      res = 96
    )

    shiny::observeEvent(input$ta_save, {
      plotted <- ta_plotted()
      if (is.null(plotted)) {
        shiny::showNotification("Create a plot first.", type = "error")
        return()
      }
      file <- file.path(state$project, plotted$tag, time_at_file(plotted$variable, plotted$by))
      size <- ta_size(plotted$by)
      saved <- tryCatch(
        {
          ggplot2::ggsave(file, plot = time_at_plot(), width = size[1], height = size[2], dpi = 300)
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
