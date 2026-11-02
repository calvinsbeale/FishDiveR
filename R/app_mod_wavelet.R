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
      shiny::checkboxInput(ns("thin"), "Thin records before the wavelet (block average)"),
      shiny::conditionalPanel(
        "input.thin",
        ns = ns,
        shiny::numericInput(ns("thin_s"), "Thin to (seconds)", value = 15, min = 1, step = 1, width = "200px")
      ),
      shiny::uiOutput(ns("thin_suggestion")),
      bslib::accordion(
        open = FALSE,
        class = "mb-3",
        bslib::accordion_panel("How thinning works", thinning_explanation())
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
      shiny::uiOutput(ns("thinned_note")),
      shiny::uiOutput(ns("view_note")),
      shiny::imageOutput(ns("wavelet_image"), height = "auto", fill = FALSE)
    )
  )
}

#' Explanation of thinning for the wavelet step
#' @noRd
thinning_explanation <- function() {
  shiny::tagList(
    shiny::p(
      shiny::strong("What it does."),
      "Each block of n seconds is averaged into one record, so thinning a 1 second record to 15 seconds turns every",
      "15 records into one. Only the copy given to the wavelet is thinned: the imported record used for the depth",
      "plots and the daily depth statistics keeps its full resolution."
    ),
    shiny::p(
      shiny::strong("Why use it."),
      "The memory and time a wavelet needs grow with the number of records. Thinning 15 times cuts both roughly",
      "15 times, e.g. 11 GB to under 1 GB. The periods analysed are set by the lower period, upper period and",
      "sub-octaves, so they do not change."
    ),
    shiny::p(
      shiny::strong("Effect on the wavelet."),
      "The shortest cycle a record can show is twice its sampling interval, 30 seconds at 15 second sampling.",
      "Cycles at the lower period are still well described as long as each cycle has enough records: a 5 minute",
      "cycle has 300 records at 1 second and 20 at 15 seconds, and 20 is plenty. What is lost is only movement",
      "faster than about twice the new interval, which is already below the lower period and outside the analysis."
    ),
    shiny::p(
      shiny::strong("Why average rather than keep every nth record."),
      "Keeping only every nth record lets movements faster than the new interval show up as false, slower cycles",
      "(aliasing). Averaging each block removes them first."
    ),
    shiny::p(
      shiny::strong("Suggested interval."),
      paste0(
        "The suggestion keeps at least ", thin_records_per_cycle, " records per cycle of the lower period",
        " (the lower period in seconds divided by ", thin_records_per_cycle, "), rounded down to an interval that divides a day evenly"
      ),
      "and is a multiple of your tags' sampling intervals. Tags already sampled at that interval or longer are not thinned."
    ),
    shiny::p(
      class = "mb-0",
      shiny::strong("Keep tags consistent."),
      "Wavelet statistics from a thinned record are very close to, but not identical to, those from the full record.",
      "Thin all tags you will cluster together to the same interval. This also makes tags recorded at different",
      "rates directly comparable."
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
      suboctaves = input$suboctaves,
      thin_s = if (isTRUE(input$thin)) input$thin_s else NA
    ))

    settings_ok <- function(s) {
      isTRUE(s$lower_period_mins > 0) && isTRUE(s$upper_period_hours > 0) && isTRUE(s$suboctaves >= 1) &&
        isTRUE(s$upper_period_hours * 60 > s$lower_period_mins)
    }

    # Sampling interval the wavelet will use for a tag: thinned if thinning is
    # on and the tag is sampled more finely than the thinning interval
    wavelet_sampling <- function(sampling_s, thin_s) {
      if (isTRUE(thin_s > sampling_s)) thin_s else sampling_s
    }

    # Problems with the thinning interval. Errors stop a run; warnings don't.
    thin_problems <- function(s, info) {
      errors <- character()
      warnings <- character()
      thin_s <- s$thin_s
      if (is.na(thin_s) || is.null(info)) {
        return(list(errors = errors, warnings = warnings))
      }

      if (!isTRUE(thin_s > 0) || thin_s != round(thin_s) || 86400 %% thin_s != 0) {
        errors <- c(errors, "The thinning interval must be a whole number of seconds that divides a day evenly, e.g. 5, 10, 15, 30 or 60.")
        return(list(errors = errors, warnings = warnings))
      }
      not_multiple <- info$tag_ID[info$sampling_s < thin_s & thin_s %% info$sampling_s != 0]
      if (length(not_multiple) > 0) {
        errors <- c(errors, paste0(
          "The thinning interval must be a multiple of the sampling interval of: ", paste(not_multiple, collapse = ", "), "."
        ))
      }
      if (settings_ok(s)) {
        if (thin_s * 2 >= s$lower_period_mins * 60) {
          errors <- c(errors, paste0(
            "Thinning to ", thin_s, " s is too coarse for a ", s$lower_period_mins,
            " minute lower period: the shortest cycle it can show is ", thin_s * 2, " s."
          ))
        } else if (thin_s > s$lower_period_mins * 60 / thin_records_per_cycle) {
          warnings <- c(warnings, paste0(
            "Thinning to ", thin_s, " s leaves fewer than ", thin_records_per_cycle,
            " records per cycle of the lower period. The suggested interval is safer."
          ))
        }
      }
      not_thinned <- info$tag_ID[info$sampling_s >= thin_s]
      if (length(not_thinned) > 0) {
        warnings <- c(warnings, paste0(
          "Already sampled at ", thin_s, " s or longer, so not thinned: ", paste(not_thinned, collapse = ", "), "."
        ))
      }
      list(errors = errors, warnings = warnings)
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
        wave_sampling <- wavelet_sampling(summary$sampling_s, s$thin_s)
        wave_records <- summary$n_records * summary$sampling_s / wave_sampling
        data.frame(
          tag_ID = tag,
          n_records = summary$n_records,
          sampling_s = summary$sampling_s,
          wave_sampling_s = wave_sampling,
          bytes = if (settings_ok(s)) wavelet_bytes(wave_records, s$lower_period_mins, s$upper_period_hours, s$suboctaves) else NA,
          wavelet = !is.null(meta),
          LP = if (is.null(meta)) NA else meta$LP,
          UP_hours = if (is.null(meta)) NA else meta$UP * 24,
          SO = if (is.null(meta)) NA else meta$SO,
          # Wavelets made before the sampling interval was recorded show NA
          wavelet_sampling_s = if (is.null(meta$sampling_s)) NA else meta$sampling_s,
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
        `Sampling (s)` = ifelse(
          info$wave_sampling_s > info$sampling_s,
          paste(info$sampling_s, "to", info$wave_sampling_s),
          info$sampling_s
        ),
        `Est. memory` = ifelse(is.na(info$bytes), "", format_bytes(info$bytes)),
        Wavelet = ifelse(
          info$wavelet,
          paste0(
            "Yes (", info$LP, " min to ", info$UP_hours, " h, ", info$SO, " sub-oct.",
            ifelse(is.na(info$wavelet_sampling_s), "", paste0(", ", info$wavelet_sampling_s, " s")), ")"
          ),
          ""
        ),
        check.names = FALSE
      )
      # Keep the selection when settings change and the table is redrawn
      selection <- list(mode = "multiple", selected = shiny::isolate(input$tag_table_rows_selected))
      DT::datatable(display, selection = selection, rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })

    # Settings problems are always shown; problems with particular tags only
    # for the tags selected in the table
    output$warnings <- shiny::renderUI({
      info <- tag_info()
      s <- settings()
      if (is.null(info)) return(NULL)
      selected <- selected_info()
      if (is.null(selected)) selected <- info[0, ]
      msgs <- character()

      if (!settings_ok(s)) {
        msgs <- c(msgs, "Enter positive periods, with the upper period longer than the lower period.")
      } else {
        too_short <- selected$tag_ID[s$lower_period_mins * 60 < selected$sampling_s]
        if (length(too_short) > 0) {
          msgs <- c(msgs, paste0("The lower period is shorter than the sampling interval of: ", paste(too_short, collapse = ", "), "."))
        }
        if (!is.null(ram)) {
          too_big <- selected$tag_ID[!is.na(selected$bytes) & selected$bytes > ram / 2]
          if (length(too_big) > 0) {
            msgs <- c(msgs, paste0(
              "Estimated memory for ", paste(too_big, collapse = ", "), " is more than half of this computer's ",
              format_bytes(ram), " and may run out of memory. Try thinning the records or a longer lower period."
            ))
          }
        }
      }
      thin <- thin_problems(s, selected)
      msgs <- c(msgs, thin$errors, thin$warnings)

      done <- info[info$wavelet, ]
      if (nrow(unique(done[, c("LP", "UP_hours", "SO", "wavelet_sampling_s")])) > 1) {
        msgs <- c(msgs, "Existing wavelets use different settings or sampling intervals. Tags clustered together should use the same.")
      }

      if (length(msgs) > 0) {
        shiny::div(class = "alert alert-warning mt-2", lapply(msgs, shiny::p, class = "mb-1"))
      }
    })

    output$log <- shiny::renderText({
      log <- wavelet_log()
      if (length(log) == 0) "No wavelets run yet this session." else paste(log, collapse = "\n")
    })

    ## Thinning suggestion, for the tags selected in the table
    selected_info <- shiny::reactive({
      info <- tag_info()
      rows <- input$tag_table_rows_selected
      if (is.null(info) || length(rows) == 0) NULL else info[rows[rows <= nrow(info)], , drop = FALSE]
    })

    suggested_thin <- shiny::reactive({
      selected <- selected_info()
      s <- settings()
      if (is.null(selected) || !settings_ok(s)) return(NA_real_)
      suggest_thin_interval(s$lower_period_mins, selected$sampling_s)
    })

    output$thin_suggestion <- shiny::renderUI({
      selected <- selected_info()
      s <- settings()
      if (is.null(selected)) {
        return(shiny::p(class = "text-muted small", "Select tags in the table below to see a suggested thinning interval."))
      }
      if (!settings_ok(s)) return(NULL)
      suggested <- suggested_thin()
      tag_names <- paste(selected$tag_ID, collapse = ", ")

      if (is.na(suggested)) {
        return(shiny::p(
          class = "text-muted small",
          paste0("No thinning interval suits this lower period and the sampling interval of ", tag_names, ".")
        ))
      }
      if (all(selected$sampling_s >= suggested)) {
        return(shiny::p(
          class = "text-muted small",
          paste0(
            "Thinning isn't needed: ", tag_names, if (nrow(selected) == 1) " is" else " are",
            " already sampled every ", suggested, " s or less often."
          )
        ))
      }

      full_bytes <- wavelet_bytes(selected$n_records, s$lower_period_mins, s$upper_period_hours, s$suboctaves)
      thinned_bytes <- wavelet_bytes(
        selected$n_records * selected$sampling_s / pmax(selected$sampling_s, suggested),
        s$lower_period_mins, s$upper_period_hours, s$suboctaves
      )
      savings <- paste0(
        selected$tag_ID, ": ", format_bytes(full_bytes), " to ", format_bytes(thinned_bytes),
        collapse = "; "
      )

      shiny::div(
        class = "alert alert-info py-2",
        shiny::p(
          class = "mb-1",
          shiny::strong(paste0("Suggested: thin to ", suggested, " s")),
          paste0(
            "for a ", s$lower_period_mins, " minute lower period, keeping ",
            floor(s$lower_period_mins * 60 / suggested), " records per cycle. Estimated memory, ", savings, "."
          )
        ),
        shiny::actionLink(session$ns("use_suggestion"), "Use this suggestion")
      )
    })

    shiny::observeEvent(input$use_suggestion, {
      shiny::updateCheckboxInput(session, "thin", value = TRUE)
      shiny::updateNumericInput(session, "thin_s", value = suggested_thin())
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
            # Thin the copy given to the wavelet; the imported record is unchanged
            if (!redraw) {
              sampling_s <- as.numeric(difftime(archive$date[2], archive$date[1], units = "secs"))
              if (isTRUE(s$thin_s > sampling_s)) {
                archive <- thin_archive(archive, s$thin_s, verbose = TRUE)
              }
            }
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
      info <- tag_info()
      if (!settings_ok(settings())) {
        shiny::showNotification("Check the wavelet settings first.", type = "error")
        return()
      }
      # Check thinning for the tags being run, which may not be the selected ones
      thin_errors <- thin_problems(settings(), info[info$tag_ID %in% tag_IDs, , drop = FALSE])$errors
      if (length(thin_errors) > 0) {
        shiny::showNotification(paste(thin_errors, collapse = " "), type = "error", duration = 10)
        return()
      }
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

    output$thinned_note <- shiny::renderUI({
      info <- tag_info()
      shiny::req(info, input$view_tag)
      row <- info[info$tag_ID == input$view_tag, ]
      if (nrow(row) == 1 && isTRUE(row$wavelet_sampling_s > row$sampling_s)) {
        shiny::p(class = "text-muted", paste("Wavelet thinned to", row$wavelet_sampling_s, "seconds"))
      }
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
