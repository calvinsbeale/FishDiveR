# App step 4: daily wavelet and depth statistics for each tag.

#' @noRd
mod_stats_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, xl = c(5, 7)),
    bslib::card(
      bslib::card_header("Create daily statistics"),
      shiny::p(
        "Summarises each day of each tag: wavelet statistics from the tag's wavelet, and depth statistics from the",
        "full-resolution depth record. Both are used to cluster the days in the next steps."
      ),
      shiny::checkboxInput(
        ns("diel"),
        shiny::tagList(
          "Include diel statistics ",
          help_tip("Diel means day and night. Adds the difference between night and day in depth, vertical speed and time near the surface, standardised by each day's maximum depth.")
        ),
        value = TRUE
      ),
      shiny::conditionalPanel(
        "input.diel",
        ns = ns,
        shiny::radioButtons(
          ns("diel_source"), "Day and night times from",
          choices = c(
            "Each tag's GPS file" = "gps",
            "Fixed times for all tags" = "fixed"
          )
        ),
        shiny::conditionalPanel(
          "input.diel_source == 'gps'",
          ns = ns,
          shiny::selectInput(
            ns("sunset_type"), "Sunrise and sunset type",
            choices = c("Civil" = "civil", "Nautical" = "nautical", "Astronomical" = "astronomical"),
            width = "200px"
          )
        ),
        shiny::conditionalPanel(
          "input.diel_source == 'fixed'",
          ns = ns,
          bslib::layout_column_wrap(
            width = "160px",
            fill = FALSE,
            shiny::textInput(ns("sunrise_time"), "Fixed sunrise (local)", value = "06:00:00", placeholder = "hh:mm:ss"),
            shiny::textInput(ns("sunset_time"), "Fixed sunset (local)", value = "18:00:00", placeholder = "hh:mm:ss")
          )
        )
      ),
      DT::DTOutput(ns("tag_table"), fill = FALSE),
      shiny::uiOutput(ns("warnings")),
      shiny::div(
        shiny::actionButton(ns("run_selected"), "Run selected", class = "btn-primary"),
        shiny::actionButton(ns("run_all"), "Run all")
      ),
      shiny::h6("Log", class = "mt-3"),
      shiny::verbatimTextOutput(ns("log"))
    ),
    bslib::card(
      bslib::card_header("Daily statistics"),
      bslib::layout_column_wrap(
        width = "200px",
        fill = FALSE,
        shiny::selectInput(ns("view_tag"), "Tag", choices = NULL),
        shiny::radioButtons(ns("view_stats"), "Show", choices = c("Depth statistics" = "depth", "Wavelet statistics" = "wave"), inline = TRUE)
      ),
      shiny::uiOutput(ns("view_note")),
      DT::DTOutput(ns("stats_table"), fill = FALSE)
    )
  )
}

#' Paths of a tag's daily statistics files
#' @noRd
stats_files <- function(project_folder, tag_ID) {
  list(
    wave = file.path(project_folder, tag_ID, "3_Stats", paste0(tag_ID, "_waveStats.csv")),
    depth = file.path(project_folder, tag_ID, "3_Stats", paste0(tag_ID, "_depthStats.csv"))
  )
}

#' Number of days (rows) in a statistics file, or NA if it doesn't exist
#' @noRd
stats_days <- function(file) {
  if (!file.exists(file)) return(NA_integer_)
  length(readLines(file)) - 1L
}

#' @noRd
mod_stats_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    stats_log <- shiny::reactiveVal(character())

    is_time <- function(x) isTRUE(grepl("^\\d{2}:\\d{2}:\\d{2}$", x))

    # One row per imported tag
    tag_info <- shiny::reactive({
      state$refresh
      shiny::req(state$project)
      status <- tag_status(state$tags, state$project)
      tags <- state$tags[status$Imported, , drop = FALSE]
      if (nrow(tags) == 0) {
        return(NULL)
      }
      files <- lapply(tags$tag_ID, stats_files, project_folder = state$project)
      data.frame(
        tag_ID = tags$tag_ID,
        wavelet = status$Wavelet[status$Imported],
        has_gps = !is.na(tags$GPS) & nzchar(tags$GPS) & file.exists(ifelse(is.na(tags$GPS), "", tags$GPS)),
        gps = tags$GPS,
        wave_days = vapply(files, function(f) stats_days(f$wave), integer(1)),
        depth_days = vapply(files, function(f) stats_days(f$depth), integer(1)),
        stringsAsFactors = FALSE
      )
    })

    # Where each tag's day and night times will come from
    diel_source <- function(info) {
      if (!isTRUE(input$diel)) return(rep("Not used", nrow(info)))
      if (input$diel_source == "fixed") return(rep("Fixed times", nrow(info)))
      ifelse(info$has_gps, "GPS file", "No GPS file")
    }

    # Problems with the diel settings for some tags, which stop them running
    diel_problems <- function(info) {
      msgs <- character()
      source <- diel_source(info)
      if (any(source == "Fixed times") && !(is_time(input$sunrise_time) && is_time(input$sunset_time))) {
        msgs <- c(msgs, "Enter fixed sunrise and sunset times as hh:mm:ss.")
      }
      no_gps <- info$tag_ID[source == "No GPS file"]
      if (length(no_gps) > 0) {
        msgs <- c(msgs, paste0(
          "No GPS file for ", paste(no_gps, collapse = ", "), ". Add one in step 1, ",
          "choose fixed times for all tags, or turn off diel statistics."
        ))
      }
      msgs
    }

    output$tag_table <- DT::renderDT({
      shiny::validate(shiny::need(state$project, "Open a project folder in step 1."))
      info <- tag_info()
      shiny::validate(shiny::need(!is.null(info), "Import tags in step 2 first."))
      display <- data.frame(
        `Tag ID` = info$tag_ID,
        Wavelet = ifelse(info$wavelet, "Yes", ""),
        `Wavelet stats` = ifelse(is.na(info$wave_days), "", paste(info$wave_days, "days")),
        `Depth stats` = ifelse(is.na(info$depth_days), "", paste(info$depth_days, "days")),
        `Day/night from` = diel_source(info),
        check.names = FALSE
      )
      selection <- list(mode = "multiple", selected = shiny::isolate(input$tag_table_rows_selected))
      DT::datatable(display, selection = selection, rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })

    output$warnings <- shiny::renderUI({
      info <- tag_info()
      if (is.null(info)) return(NULL)
      msgs <- diel_problems(info)
      no_wavelet <- info$tag_ID[!info$wavelet]
      if (length(no_wavelet) > 0) {
        msgs <- c(msgs, paste0(
          "No wavelet yet for ", paste(no_wavelet, collapse = ", "),
          ", so only depth statistics can be made. Run the wavelet in step 3 first."
        ))
      }
      mismatch <- info$tag_ID[!is.na(info$wave_days) & !is.na(info$depth_days) & info$wave_days != info$depth_days]
      if (length(mismatch) > 0) {
        msgs <- c(msgs, paste0(
          "Wavelet and depth statistics cover different numbers of days for ", paste(mismatch, collapse = ", "),
          ". They are matched day by day for clustering, so run both again."
        ))
      }

      if (length(msgs) > 0) {
        shiny::div(class = "alert alert-warning mt-2", lapply(msgs, shiny::p, class = "mb-1"))
      }
    })

    output$log <- shiny::renderText({
      log <- stats_log()
      if (length(log) == 0) "No statistics made yet this session." else paste(log, collapse = "\n")
    })

    run_stats <- function(tag_IDs) {
      info <- tag_info()
      problems <- diel_problems(info[info$tag_ID %in% tag_IDs, , drop = FALSE])
      if (length(problems) > 0) {
        shiny::showNotification(paste(problems, collapse = " "), type = "error", duration = 10)
        return()
      }

      log <- character()
      shiny::withProgress(message = "Daily statistics", value = 0, {
        for (i in seq_along(tag_IDs)) {
          tag <- tag_IDs[i]
          row <- info[info$tag_ID == tag, ]
          shiny::setProgress(value = (i - 1) / length(tag_IDs), detail = paste0(tag, " (", i, " of ", length(tag_IDs), ")"))
          started <- Sys.time()

          wave_res <- if (row$wavelet) {
            run_logged({
              wavelet <- readRDS(file.path(state$project, tag, "1_Wavelets", paste0(tag, "_wavelet.rds")))
              create_wavelet_stats(wavelet, tag_ID = tag, output = TRUE, output_folder = state$project, verbose = TRUE)
              invisible(NULL)
            })
          } else {
            list(ok = FALSE, log = "Skipped wavelet statistics: no wavelet yet.")
          }
          gc()

          use_gps <- isTRUE(input$diel) && diel_source(row) == "GPS file"
          depth_res <- run_logged({
            archive <- readRDS(file.path(state$project, tag, "archive_days.rds"))
            create_depth_stats(
              archive = archive,
              tag_ID = tag,
              diel = isTRUE(input$diel),
              sunrise_time = if (isTRUE(input$diel) && !use_gps) input$sunrise_time else NULL,
              sunset_time = if (isTRUE(input$diel) && !use_gps) input$sunset_time else NULL,
              GPS = if (use_gps) row$gps else FALSE,
              sunset_type = input$sunset_type,
              output = TRUE,
              output_folder = state$project,
              verbose = TRUE
            )
            invisible(NULL)
          })

          minutes <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)
          status <- if (wave_res$ok && depth_res$ok) "done" else if (depth_res$ok) "depth statistics only" else "FAILED"
          log <- c(log, paste0("== ", tag, ": ", status, " (", minutes, " min)"), wave_res$log, depth_res$log, "")
          stats_log(log)
        }
        shiny::setProgress(1)
      })
      state$refresh <- state$refresh + 1
    }

    shiny::observeEvent(input$run_selected, {
      rows <- input$tag_table_rows_selected
      if (length(rows) == 0) {
        shiny::showNotification("Select one or more tags in the table first.", type = "error")
        return()
      }
      run_stats(tag_info()$tag_ID[rows])
    })

    shiny::observeEvent(input$run_all, {
      info <- tag_info()
      shiny::req(info)
      run_stats(info$tag_ID)
    })

    ## Statistics viewer
    stats_tags <- shiny::reactive({
      info <- tag_info()
      if (is.null(info)) character() else info$tag_ID[!is.na(info$wave_days) | !is.na(info$depth_days)]
    })

    shiny::observe({
      choices <- stats_tags()
      current <- shiny::isolate(input$view_tag)
      shiny::updateSelectInput(session, "view_tag", choices = choices, selected = if (isTRUE(current %in% choices)) current else choices[1])
    })

    view_file <- shiny::reactive({
      shiny::req(input$view_tag, input$view_stats)
      stats_files(state$project, input$view_tag)[[input$view_stats]]
    })

    output$view_note <- shiny::renderUI({
      if (length(stats_tags()) == 0) {
        shiny::p(class = "text-muted", "Create daily statistics to see them here, one row per day.")
      } else if (!file.exists(view_file())) {
        shiny::p(class = "text-muted", "These statistics haven't been made for this tag yet.")
      }
    })

    output$stats_table <- DT::renderDT({
      state$refresh
      file <- view_file()
      shiny::req(file.exists(file))
      stats <- utils::read.csv(file, check.names = FALSE)
      stats$tag_ID <- NULL
      numeric_cols <- vapply(stats, is.numeric, logical(1))
      stats[numeric_cols] <- lapply(stats[numeric_cols], signif, digits = 4)
      first <- intersect("date_only", names(stats))
      stats <- stats[, c(first, setdiff(names(stats), first)), drop = FALSE]
      DT::datatable(stats, rownames = FALSE, selection = "none", options = list(scrollX = TRUE, pageLength = 10, dom = "tp"))
    })
  })
}
