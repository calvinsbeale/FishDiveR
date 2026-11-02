# App step 1: choose the project folder and manage the tag table.

#' @noRd
mod_project_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
    shiny::div(
      bslib::card(
        bslib::card_header("Project folder"),
        shiny::p(
          "Results for every tag are saved here: one sub-folder per tag, plus",
          shiny::code("Combined_tags"), "for the multi-tag analyses."
        ),
        shiny::textInput(ns("project_path"), NULL, placeholder = "e.g. C:/Tag data/My project", width = "100%"),
        shiny::div(
          shiny::actionButton(ns("browse_project"), "Browse..."),
          shiny::actionButton(ns("open_project"), "Open", class = "btn-primary")
        )
      ),
      bslib::card(
        bslib::card_header("Add or edit a tag"),
        shiny::textInput(ns("tag_ID"), "Tag ID", width = "100%"),
        path_input(ns("archive"), "Archive csv file"),
        shiny::textInput(ns("tag_deploy_UTC"), "Deploy time (UTC)", placeholder = "yyyy-mm-dd hh:mm:ss", width = "100%"),
        shiny::textInput(ns("tag_release_UTC"), "Release time (UTC)", placeholder = "yyyy-mm-dd hh:mm:ss", width = "100%"),
        shiny::selectizeInput(ns("time_zone"), "Local time zone", choices = OlsonNames(), selected = "UTC", width = "100%"),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::numericInput(ns("date_time_col"), "Date-time column", value = 1, min = 1, step = 1),
          shiny::numericInput(ns("depth_col"), "Depth column", value = 2, min = 1, step = 1),
          shiny::numericInput(ns("temp_col"), "Temp. column", value = NA, min = 1, step = 1)
        ),
        path_input(ns("GPS"), "GPS csv file (optional, for diel statistics)"),
        shiny::div(
          shiny::actionButton(ns("save_tag"), "Save tag", class = "btn-primary"),
          shiny::actionButton(ns("clear_form"), "Clear")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Tags in this project"),
      shiny::p(
        "Select a tag to edit or remove it. The table is saved as",
        shiny::code("tag_metadata.csv"), "in the project folder."
      ),
      DT::DTOutput(ns("tag_table"), fill = FALSE),
      bslib::card_footer(
        shiny::actionButton(ns("edit_tag"), "Edit selected"),
        shiny::actionButton(ns("remove_tag"), "Remove selected"),
        shiny::actionButton(ns("load_table"), "Load tag table csv..."),
        shiny::actionButton(ns("add_example"), "Add example tag")
      )
    )
  )
}

#' Text input for a file path with a Browse button
#' @noRd
path_input <- function(id, label) {
  shiny::div(
    shiny::tags$label(label, `for` = id, class = "form-label"),
    shiny::div(
      class = "input-group mb-3",
      shiny::tags$input(id = id, type = "text", class = "form-control shiny-input-text"),
      shiny::actionButton(paste0(id, "_browse"), "Browse...", class = "btn-outline-secondary")
    )
  )
}

#' @noRd
mod_project_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    if (!is.null(shiny::isolate(state$project))) {
      shiny::updateTextInput(session, "project_path", value = shiny::isolate(state$project))
    }

    notify_error <- function(...) shiny::showNotification(paste0(...), type = "error", duration = 10)
    save_tags <- function(tags) set_tags(state, tags)
    browse <- browse_server(input, session)
    remove_tags <- remove_tags_server(input, session, state)

    need_project <- function() {
      if (is.null(state$project)) {
        notify_error("Open a project folder first.")
        return(FALSE)
      }
      TRUE
    }

    ## Project folder
    shiny::observeEvent(input$browse_project, {
      typed <- trimws(input$project_path)
      start_dir <- if (nzchar(typed) && dir.exists(typed)) typed else state$project
      browse("dir", "Choose the project folder", start_dir, function(path) {
        shiny::updateTextInput(session, "project_path", value = path)
      })
    })

    shiny::observeEvent(input$open_project, {
      path <- trimws(input$project_path)
      if (!nzchar(path)) {
        notify_error("Enter or browse to a project folder.")
        return()
      }
      if (!dir.exists(path)) {
        if (!dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
          notify_error("Could not create folder: ", path)
          return()
        }
        shiny::showNotification("Created new project folder.")
      }
      path <- normalizePath(path, winslash = "/")

      tags <- tryCatch(read_tag_metadata(file.path(path, "tag_metadata.csv")), error = function(e) {
        notify_error("Could not read tag_metadata.csv: ", conditionMessage(e))
        NULL
      })
      if (is.null(tags)) return()

      state$project <- path
      state$tags <- tags
      shiny::updateTextInput(session, "project_path", value = path)
    })

    ## Tag form
    for (field in c("archive", "GPS")) {
      local({
        f <- field
        shiny::observeEvent(input[[paste0(f, "_browse")]], {
          caption <- if (f == "archive") "Choose the archive csv file" else "Choose the GPS csv file"
          browse("file", caption, state$project, function(path) {
            shiny::updateTextInput(session, f, value = path)
          })
        })
      })
    }

    fill_form <- function(tag) {
      for (f in c("tag_ID", "archive", "tag_deploy_UTC", "tag_release_UTC", "GPS")) {
        shiny::updateTextInput(session, f, value = if (is.na(tag[[f]])) "" else tag[[f]])
      }
      shiny::updateSelectizeInput(session, "time_zone", selected = tag$time_zone)
      for (f in c("date_time_col", "depth_col", "temp_col")) {
        shiny::updateNumericInput(session, f, value = tag[[f]])
      }
    }

    form_tag <- function() {
      blank_to_na <- function(x) if (is.null(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
      as_col <- function(x) if (is.null(x) || is.na(x)) NA_integer_ else as.integer(x)
      data.frame(
        tag_ID = trimws(input$tag_ID),
        archive = blank_to_na(input$archive),
        tag_deploy_UTC = blank_to_na(input$tag_deploy_UTC),
        tag_release_UTC = blank_to_na(input$tag_release_UTC),
        date_time_col = as_col(input$date_time_col),
        depth_col = as_col(input$depth_col),
        temp_col = as_col(input$temp_col),
        time_zone = input$time_zone,
        GPS = blank_to_na(input$GPS),
        stringsAsFactors = FALSE
      )
    }

    shiny::observeEvent(input$save_tag, {
      if (!need_project()) return()
      tag <- form_tag()
      problems <- validate_tag(tag)
      if (length(problems) > 0) {
        notify_error("Tag not saved: ", paste(problems, collapse = "; "), ".")
        return()
      }
      save_tags(upsert_tags(state$tags, tag))
      shiny::showNotification(paste0("Saved tag '", tag$tag_ID, "'."))
    })

    shiny::observeEvent(input$clear_form, {
      fill_form(data.frame(
        tag_ID = "", archive = "", tag_deploy_UTC = "", tag_release_UTC = "", GPS = "",
        time_zone = "UTC", date_time_col = 1, depth_col = 2, temp_col = NA
      ))
    })

    ## Tag table
    output$tag_table <- DT::renderDT({
      shiny::validate(shiny::need(state$project, "Open a project folder to see its tags."))
      tags <- state$tags
      shiny::validate(shiny::need(nrow(tags) > 0, "No tags yet. Add one with the form, load a tag table, or add the example tag."))

      problems <- vapply(seq_len(nrow(tags)), function(i) paste(validate_tag(tags[i, ]), collapse = "; "), character(1))
      display <- data.frame(
        `Tag ID` = tags$tag_ID,
        Archive = basename(tags$archive),
        `Deploy (UTC)` = tags$tag_deploy_UTC,
        `Release (UTC)` = tags$tag_release_UTC,
        `Time zone` = tags$time_zone,
        GPS = ifelse(is.na(tags$GPS), "", basename(tags$GPS)),
        Check = ifelse(nzchar(problems), problems, "OK"),
        check.names = FALSE
      )
      DT::datatable(display, selection = "single", rownames = FALSE, fillContainer = FALSE, options = list(dom = "t", pageLength = 1000))
    })

    selected_tag <- function() {
      row <- input$tag_table_rows_selected
      if (length(row) == 0) {
        notify_error("Select a tag in the table first.")
        return(NULL)
      }
      state$tags[row, , drop = FALSE]
    }

    shiny::observeEvent(input$edit_tag, {
      tag <- selected_tag()
      if (!is.null(tag)) fill_form(tag)
    })

    shiny::observeEvent(input$remove_tag, {
      tag <- selected_tag()
      if (!is.null(tag)) remove_tags(tag$tag_ID)
    })

    shiny::observeEvent(input$load_table, {
      if (!need_project()) return()
      browse("file", "Choose the tag table csv", state$project, function(path) {
        new_tags <- tryCatch(read_tag_metadata(path), error = function(e) {
          notify_error(conditionMessage(e))
          NULL
        })
        if (is.null(new_tags)) return()
        save_tags(upsert_tags(state$tags, new_tags))
        shiny::showNotification(paste("Loaded", nrow(new_tags), "tag(s)."))
      })
    })

    shiny::observeEvent(input$add_example, {
      if (!need_project()) return()
      save_tags(upsert_tags(state$tags, example_tag()))
      shiny::showNotification("Added the example tag.")
    })
  })
}
