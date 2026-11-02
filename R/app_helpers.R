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
