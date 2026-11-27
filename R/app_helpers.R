# Shiny helpers shared by the app steps (run_app).

# Step values of the app tabs, in workflow order
app_steps <- c("project", "import", "wavelets", "stats", "pca", "clustering", "results")

#' Styles for the app
#'
#' fd-match-left: on wide screens, a card that fills its grid cell without
#' adding height, so it ends level with the column beside it.
#' @noRd
app_css <- function() {
  shiny::tags$style(shiny::HTML("
    @media (min-width: 992px) {
      .fd-match-left { position: relative; flex: 1 1 auto; }
      .fd-match-left > .card { position: absolute; inset: 0; margin-bottom: 0; }
      .fd-left-col > .card:last-child { margin-bottom: 0; }
    }
    .fd-scroll { min-height: 0; overflow-y: auto; }
  "))
}

#' A small "?" that shows an explanation when hovered over
#' @noRd
help_tip <- function(text) {
  bslib::tooltip(
    shiny::span(class = "badge rounded-pill text-bg-secondary", style = "cursor: help;", "?"),
    text
  )
}

#' Text box for a date-time or a time, typed in a fixed format
#'
#' Only the digits are typed: the separators of `pattern` are added as they
#' are typed, pasted text such as "2024/3/5 8:30" is tidied, a date without a
#' time gets 00:00:00, and the box is marked if the value is not a valid date
#' or time.
#'
#' A date-time box shows a calendar as soon as it has the cursor, opening on
#' the years, then the months, then the days. Once the date is chosen or
#' typed, a clock replaces it, to click the hour, then the minute, then the
#' second. A time box shows the clock. Buttons beside the box open them too.
#'
#' The input's value is the text, as from shiny::textInput(), and
#' shiny::updateTextInput() sets it. See inst/app/masked_input.js.
#' @noRd
masked_input <- function(id, label, pattern = c("yyyy-mm-dd hh:mm:ss", "hh:mm:ss"), value = "", width = NULL) {
  pattern <- match.arg(pattern)
  datetime <- pattern == "yyyy-mm-dd hh:mm:ss"

  box <- shiny::tags$input(
    id = id, type = "text", class = "form-control shiny-input-text fd-masked", value = value,
    placeholder = pattern, maxlength = nchar(pattern), inputmode = "numeric",
    autocomplete = "off", spellcheck = "false",
    `data-pattern` = pattern,
    # A date-time needs at least the date, a time at least the hour
    `data-min-length` = if (datetime) 10 else 2
  )
  # The pickers open when the box has the cursor, so tabbing skips the buttons
  picker_button <- function(class, title, icon) {
    shiny::tags$button(
      type = "button", class = paste("btn btn-outline-secondary", class), `data-fd-input` = id,
      tabindex = "-1", title = title, `aria-label` = title, shiny::icon(icon)
    )
  }
  box <- shiny::div(
    class = "input-group",
    box,
    if (datetime) picker_button("fd-calendar", "Choose the date", "calendar"),
    picker_button("fd-clock-button", "Choose the time", "clock")
  )
  if (datetime) {
    # The calendar drops down from this hidden, zero-size input under the box
    anchor <- shiny::tags$input(
      type = "text", class = "fd-calendar-anchor", tabindex = "-1", `aria-hidden` = "true",
      style = "display: block; width: 0; height: 0; padding: 0; margin: 0; border: 0; opacity: 0;"
    )
    box <- shiny::tagList(box, anchor)
  }
  tag <- shiny::div(
    class = "mb-3 fd-masked-group",
    style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
    shiny::tags$label(label, `for` = id, class = "form-label"),
    box,
    shiny::div(
      class = "invalid-feedback",
      if (datetime) "Enter a real date and time, as yyyy-mm-dd hh:mm:ss." else "Enter a real time, as hh:mm:ss."
    )
  )
  htmltools::attachDependencies(tag, masked_input_dependencies(), append = TRUE)
}

#' Scripts for masked_input(): the calendar used by shiny::dateInput(), and the
#' typing rules
#' @noRd
masked_input_dependencies <- function() {
  c(
    htmltools::findDependencies(shiny::dateInput("fd_datepicker", NULL)),
    list(htmltools::htmlDependency(
      "fd-masked-input", as.character(utils::packageVersion("FishDiveR")),
      src = system.file("app", package = "FishDiveR"), script = "masked_input.js"
    ))
  )
}

#' Next button at the bottom right of a step
#' @noRd
next_button <- function(step) {
  shiny::div(
    class = "d-flex justify-content-end mt-3 mb-2",
    shiny::actionButton(paste0("next_", step), shiny::HTML("Next &rarr;"), class = "btn-primary")
  )
}

#' Replace the tag table in state and save it to the project folder
#' @noRd
set_tags <- function(state, tags) {
  state$tags <- tags
  write_tag_metadata(tags, file.path(shiny::isolate(state$project), "tag_metadata.csv"))
}

#' File and folder dialogs for a module
#'
#' The dialog opens in R, not the browser, so a message is shown in the app
#' while it is open. The dialog only opens once the browser has drawn that
#' message. Call once in a module server and use the returned function.
#' @noRd
browse_server <- function(input, session) {
  pending <- NULL

  shiny::observeEvent(input$browse_ready, {
    request <- pending
    pending <<- NULL
    if (is.null(request)) return()
    path <- choose_path(request$type, request$caption, request$start_dir)
    shiny::removeModal()
    if (!is.null(path)) request$on_chosen(path)
  })

  function(type, caption, start_dir = NULL, on_chosen) {
    pending <<- list(type = type, caption = caption, start_dir = start_dir, on_chosen = on_chosen)
    shiny::showModal(shiny::modalDialog(
      shiny::p(shiny::strong(caption), "in the window that has opened."),
      shiny::p("If you can't see it, look for it on your taskbar."),
      title = NULL,
      footer = NULL,
      easyClose = FALSE,
      shiny::tags$script(sprintf(
        "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
        session$ns("browse_ready")
      ))
    ))
  }
}

#' Confirm, then remove tags from the tag table
#'
#' Results already saved in the project folder are kept. Call once in a module
#' server and use the returned function with the tag IDs to remove.
#' @noRd
remove_tags_server <- function(input, session, state) {
  pending <- shiny::reactiveVal(character())

  shiny::observeEvent(input$confirm_remove, {
    tag_IDs <- pending()
    set_tags(state, state$tags[!state$tags$tag_ID %in% tag_IDs, , drop = FALSE])
    shiny::removeModal()
    shiny::showNotification(paste0("Removed ", paste(tag_IDs, collapse = ", "), "."))
  })

  function(tag_IDs) {
    pending(tag_IDs)
    shiny::showModal(shiny::modalDialog(
      title = if (length(tag_IDs) == 1) "Remove tag" else "Remove tags",
      shiny::p("Remove ", shiny::strong(paste(tag_IDs, collapse = ", ")), " from this project?"),
      shiny::p("Results already saved in the project folder are kept, and you can add the tag again later."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_remove"), "Remove", class = "btn-danger")
      )
    ))
  }
}
