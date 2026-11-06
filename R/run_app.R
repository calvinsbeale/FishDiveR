#' Launch the FishDiveR app
#'
#' `run_app` opens a point-and-click interface to FishDiveR in your web browser.
#' The app runs locally on your computer, so your tag data never leave your
#' machine.
#'
#' @details
#' The app works on a project folder. Tags are listed in a tag table, saved as
#' \code{tag_metadata.csv} in the project folder, and every step saves its
#' results there: one sub-folder per tag, plus \code{Combined_tags} for the
#' multi-tag PCA and k-means clustering. This is the same folder structure the
#' FishDiveR functions use when \code{output_folder} is the project folder, so
#' you can move between the app and R code at any point.
#'
#' The app needs the 'shiny', 'bslib' and 'DT' packages. Install them with
#' \code{install.packages(c("shiny", "bslib", "DT"))}.
#'
#' @name run_app
#'
#' @param project_folder Optional. Path of a project folder to open on launch.
#'   Defaults to NULL, choosing the folder in the app.
#' @param launch.browser Logical. If TRUE, the app opens in your web browser.
#'   Defaults to TRUE.
#' @param stop_on_close Logical. If TRUE, the app stops a few seconds after its
#'   last browser tab is closed, rather than running until R is stopped. Used by
#'   the standalone Windows app. Defaults to FALSE.
#' @param ... Further arguments passed to \code{shiny::runApp()}, e.g. \code{port}.
#'
#' @returns Called for its side effect of running the app. Returns invisibly
#'   when the app is closed.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
run_app <- function(project_folder = NULL, launch.browser = TRUE, stop_on_close = FALSE, ...) {
  needed <- c("shiny", "bslib", "DT")
  missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      "The FishDiveR app needs the following package(s): ", paste(missing_pkgs, collapse = ", "),
      "\nInstall with: install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }
  if (!is.null(project_folder) && !dir.exists(project_folder)) {
    stop("project_folder '", project_folder, "' does not exist.", call. = FALSE)
  }

  server <- app_server(project_folder)
  if (isTRUE(stop_on_close)) server <- stop_when_closed(server)
  app <- shiny::shinyApp(ui = app_ui(), server = server)
  invisible(shiny::runApp(app, launch.browser = launch.browser, ...))
}

#' Stop the app once no browser tab has had it open for `delay` seconds
#'
#' Wraps an app server so that closing the app's last tab or window stops the
#' app. The delay lets a reloaded page reconnect without the app stopping.
#' @noRd
stop_when_closed <- function(server, delay = 5) {
  # Evaluate now: run_app() reassigns its 'server' to this wrapper
  force(server)
  open_sessions <- 0
  function(input, output, session) {
    open_sessions <<- open_sessions + 1
    session$onSessionEnded(function() {
      open_sessions <<- open_sessions - 1
      later::later(function() if (open_sessions == 0) shiny::stopApp(), delay)
    })
    server(input, output, session)
  }
}
