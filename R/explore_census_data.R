#' Launch the Census ACS Variable Explorer
#'
#' This function launches an interactive Shiny application to explore,
#' filter, and download Census ACS variables.
#'
#' @import shiny
#' @export
#'
#' @examples
#' \dontrun{
#' DETRLMI::explore_census_data()
#' }
explore_census_data <- function() {
  # Locate the app inside the installed package
  app_dir <- system.file("shiny", "census_explorer", package = "DETRLMI")

  # Error handling just in case the folder goes missing
  if (app_dir == "") {
    stop("Could not find the 'census_explorer' app directory. Try re-installing the `DETRLMI` package.", call. = FALSE)
  }

  # Launch the app
  shiny::runApp(app_dir, display.mode = "normal")
}
