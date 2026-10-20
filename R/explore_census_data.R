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

#' Launch the Census LEHD RAC File Explorer
#'
#' This function launches an interactive Shiny application to explore,
#' filter, and download data from the LEHD Residence Area Characteristics (RAC) file.
#'
#' @import shiny
#' @param regional Logical. If FALSE (default), runs the Shiny app for a single state.  If TRUE, allows the selection of multiple states.  NOTE: running multiple states will significantly increase processing times.
#' @export
#'
#' @examples
#' \dontrun{
#' DETRLMI::explore_rac()
#' }
explore_rac <- function(regional = FALSE) {
  # Locate the app inside the installed package
  if(regional){
    app_dir <- system.file("shiny", "rac_explorer_multistate", package = "DETRLMI")
    message("Using Multistate RAC Explorer app.")
  } else {
    app_dir <- system.file("shiny", "rac_explorer", package = "DETRLMI")
    message("Using single state RAC Explorer app. Set parameter `regional = TRUE`` to use multi-state explorer.")
  }

  # Error handling just in case the folder goes missing
  if (app_dir == "") {
    stop("Could not find the 'rac_explorer' app directory. Try re-installing the `DETRLMI` package.", call. = FALSE)
  }

  # Launch the app
  shiny::runApp(app_dir, display.mode = "normal")
}
