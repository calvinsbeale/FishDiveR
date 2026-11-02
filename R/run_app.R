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
run_app <- function(project_folder = NULL, launch.browser = TRUE, ...) {
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

  app <- shiny::shinyApp(ui = app_ui(), server = app_server(project_folder))
  invisible(shiny::runApp(app, launch.browser = launch.browser, ...))
}
