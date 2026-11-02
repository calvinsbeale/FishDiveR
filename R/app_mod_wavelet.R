# App step 3: wavelet analysis of each imported tag.

#' @noRd
mod_wavelet_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, xl = c(5, 7)),
    bslib::card(
      bslib::card_header("Run wavelet analysis"),
      shiny::p(
        "Runs the continuous wavelet transform on each tag's depth record, over the range of periods below.",
        "Use the same settings for all tags you will cluster together."
      ),
      bslib::layout_column_wrap(
        width = "140px",
        fill = FALSE,
        shiny::numericInput(ns("lower_period_mins"), "Lower period (mins)", value = 5, min = 0.1),
        shiny::numericInput(ns("upper_period_hours"), "Upper period (hours)", value = 24, min = 1),
        shiny::numericInput(ns("suboctaves"), "Sub-octaves", value = 12, min = 1, step = 1)
      ),
      DT::DTOutput(ns("tag_table"), fill = FALSE),
      shiny::uiOutput(ns("warnings")),
      shiny::div(
        shiny::actionButton(ns("run_selected"), "Run selected", class = "btn-primary"),
        shiny::actionButton(ns("run_all"), "Run all"),
        shiny::actionButton(ns("redraw_selected"), "Redraw figures")
      ),
      shiny::h6("Log", class = "mt-3"),
      shiny::verbatimTextOutput(ns("log"))
    ),
    bslib::card(
      bslib::card_header("Wavelet spectrum"),
      bslib::layout_column_wrap(
        width = "200px",
        fill = FALSE,
        shiny::selectInput(ns("view_tag"), "Tag", choices = NULL),
        shiny::radioButtons(ns("view_plot"), "Show", choices = c("Power spectrum" = "spectrum", "Mean power by period" = "mean_power"), inline = TRUE)
      ),
      shiny::uiOutput(ns("view_note")),
      shiny::imageOutput(ns("wavelet_image"), height = "auto", fill = FALSE)
    )
  )
}

#' @noRd
mod_wavelet_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    wavelet_log <- shiny::reactiveVal(character())
    pending_run <- shiny::reactiveVal(character())
    ram <- total_ram_bytes()

    settings <- shiny::reactive(list(
      lower_period_mins = input$lower_period_mins,
      upper_period_hours = input$upper_period_hours,
      suboctaves = input$suboctaves
    ))

    settings_ok <- function(s) {
      isTRUE(s$lower_period_mins > 0) && isTRUE(s$upper_period_hours > 0) && isTRUE(s$suboctaves >= 1) &&
        isTRUE(s$upper_period_hours * 60 > s$lower_period_mins)
    }

    # One row per imported tag: size, current wavelet settings and estimate
    tag_info <- shiny::reactive({
      state$refresh
      shiny::req(state$project)
      status <- tag_status(state$tags, state$project)
      tags <- status$tag_ID[status$Imported]
      if (length(tags) == 0) {
        return(NULL)
      }

      s <- settings()
      rows <- lapply(tags, function(tag) {
        summary <- archive_summary(file.path(state$project, tag, "archive_days.rds"))
        meta <- if (status$Wavelet[status$tag_ID == tag]) read_wavelet_meta(state$project, tag) else NULL
        data.frame(
          tag_ID = tag,
          n_records = summary$n_records,
          sampling_s = summary$sampling_s,
          bytes = if (settings_ok(s)) wavelet_bytes(summary$n_records, s$lower_period_mins, s$upper_period_hours, s$suboctaves) else NA,
          wavelet = !is.null(meta),
          LP = if (is.null(meta)) NA else meta$LP,
          UP_hours = if (is.null(meta)) NA else meta$UP * 24,
          SO = if (is.null(meta)) NA else meta$SO,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    })

    output$tag_table <- DT::renderDT({
      shiny::validate(shiny::need(state$project, "Open a project folder in step 1."))
      info <- tag_info()
      shiny::validate(shiny::need(!is.null(info), "Import tags in step 2 first."))
      display <- data.frame(
        `Tag ID` = info$tag_ID,
        Records = format(info$n_records, big.mark = ","),
        `Sampling (s)` = info$sampling_s,
        `Est. memory` = ifelse(is.na(info$bytes), "", format_bytes(info$bytes)),
        Wavelet = ifelse(info$wavelet, paste0("Yes (", info$LP, " min to ", info$UP_hours, " h, ", info$SO, " sub-oct.)"), ""),
        check.names = FALSE
      )
      DT::datatable(display, selection = "multiple", rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })

    output$warnings <- shiny::renderUI({
      info <- tag_info()
      s <- settings()
      if (is.null(info)) return(NULL)
      msgs <- character()

      if (!settings_ok(s)) {
        msgs <- c(msgs, "Enter positive periods, with the upper period longer than the lower period.")
      } else {
        too_short <- info$tag_ID[s$lower_period_mins * 60 < info$sampling_s]
        if (length(too_short) > 0) {
          msgs <- c(msgs, paste0("The lower period is shorter than the sampling interval of: ", paste(too_short, collapse = ", "), "."))
        }
        if (!is.null(ram)) {
          too_big <- info$tag_ID[!is.na(info$bytes) & info$bytes > ram / 2]
          if (length(too_big) > 0) {
            msgs <- c(msgs, paste0(
              "Estimated memory for ", paste(too_big, collapse = ", "), " is more than half of this computer's ",
              format_bytes(ram), " and may run out of memory. Try a longer lower period."
            ))
          }
        }
      }

      done <- info[info$wavelet, ]
      if (nrow(unique(done[, c("LP", "UP_hours", "SO")])) > 1) {
        msgs <- c(msgs, "Existing wavelets use different settings. Tags clustered together should use the same settings.")
      }

      if (length(msgs) > 0) {
        shiny::div(class = "alert alert-warning mt-2", lapply(msgs, shiny::p, class = "mb-1"))
      }
    })

    output$log <- shiny::renderText({
      log <- wavelet_log()
      if (length(log) == 0) "No wavelets run yet this session." else paste(log, collapse = "\n")
    })

    # Run new wavelets with the current settings, or with redraw = TRUE load
    # each tag's saved wavelet and redraw its figures with its own settings
    run_wavelets <- function(tag_IDs, redraw = FALSE) {
      log <- character()
      shiny::withProgress(message = if (redraw) "Redrawing wavelet figures" else "Wavelet analysis", value = 0, {
        for (i in seq_along(tag_IDs)) {
          tag <- tag_IDs[i]
          shiny::setProgress(value = (i - 1) / length(tag_IDs), detail = paste0(tag, " (", i, " of ", length(tag_IDs), "). Long records can take a while."))

          s <- settings()
          if (redraw) {
            meta <- read_wavelet_meta(state$project, tag)
            s <- list(lower_period_mins = meta$LP, upper_period_hours = meta$UP * 24, suboctaves = meta$SO)
          }

          started <- Sys.time()
          res <- run_logged({
            archive <- readRDS(file.path(state$project, tag, "archive_days.rds"))
            # Hide WaveletComp's console printout
            utils::capture.output(create_wavelet(
              archive = archive,
              tag_ID = tag,
              load_existing_wavelet = redraw,
              lower_period_mins = s$lower_period_mins,
              upper_period_hours = s$upper_period_hours,
              suboctaves = s$suboctaves,
              output = TRUE,
              output_folder = state$project,
              verbose = TRUE,
              plot_wavelet = TRUE,
              plot_width = 1200,
              plot_height = 500,
              interactive_mode = FALSE
            ))
            invisible(NULL)
          })
          gc()

          minutes <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)
          log <- c(log, paste0("== ", tag, ": ", if (res$ok) "done" else "FAILED", " (", minutes, " min)"), res$log, "")
          wavelet_log(log)
        }
        shiny::setProgress(1)
      })
      state$refresh <- state$refresh + 1
    }

    # Ask before replacing existing wavelets
    start_run <- function(tag_IDs) {
      if (!settings_ok(settings())) {
        shiny::showNotification("Check the wavelet settings first.", type = "error")
        return()
      }
      info <- tag_info()
      existing <- intersect(tag_IDs, info$tag_ID[info$wavelet])
      if (length(existing) == 0) {
        run_wavelets(tag_IDs)
        return()
      }
      pending_run(tag_IDs)
      shiny::showModal(shiny::modalDialog(
        title = "Replace existing wavelets?",
        shiny::p("These tags already have a wavelet: ", shiny::strong(paste(existing, collapse = ", ")), "."),
        shiny::p("Running again replaces them. Daily statistics made from the old wavelets will need running again."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_run"), "Run", class = "btn-primary")
        )
      ))
    }

    shiny::observeEvent(input$confirm_run, {
      shiny::removeModal()
      run_wavelets(pending_run())
    })

    shiny::observeEvent(input$run_selected, {
      rows <- input$tag_table_rows_selected
      if (length(rows) == 0) {
        shiny::showNotification("Select one or more tags in the table first.", type = "error")
        return()
      }
      start_run(tag_info()$tag_ID[rows])
    })

    shiny::observeEvent(input$run_all, {
      info <- tag_info()
      shiny::req(info)
      start_run(info$tag_ID)
    })

    shiny::observeEvent(input$redraw_selected, {
      info <- tag_info()
      rows <- input$tag_table_rows_selected
      tag_IDs <- intersect(info$tag_ID[rows], info$tag_ID[info$wavelet])
      if (length(tag_IDs) == 0) {
        shiny::showNotification("Select one or more tags that already have a wavelet.", type = "error")
        return()
      }
      run_wavelets(tag_IDs, redraw = TRUE)
    })

    ## Spectrum viewer
    wavelet_tags <- shiny::reactive({
      info <- tag_info()
      if (is.null(info)) character() else info$tag_ID[info$wavelet]
    })

    shiny::observe({
      choices <- wavelet_tags()
      current <- shiny::isolate(input$view_tag)
      shiny::updateSelectInput(session, "view_tag", choices = choices, selected = if (isTRUE(current %in% choices)) current else choices[1])
    })

    image_file <- shiny::reactive({
      shiny::req(input$view_tag, input$view_plot)
      file_name <- if (input$view_plot == "spectrum") "_wavelet_spectrum_no.p.png" else "_wavelet_mean_power.png"
      file.path(state$project, input$view_tag, "2_Wavelet_Figures", paste0(input$view_tag, file_name))
    })

    output$view_note <- shiny::renderUI({
      if (length(wavelet_tags()) == 0) {
        shiny::p(class = "text-muted", "Run the wavelet analysis to see each tag's spectrum here.")
      } else if (!file.exists(image_file())) {
        shiny::p(class = "text-muted", "No saved figure found for this tag. Run its wavelet again to create one.")
      }
    })

    output$wavelet_image <- shiny::renderImage(
      {
        state$refresh
        file <- image_file()
        shiny::req(file.exists(file))
        list(src = file, contentType = "image/png", width = "100%", alt = paste("Wavelet figure for", input$view_tag))
      },
      deleteFile = FALSE
    )
  })
}
