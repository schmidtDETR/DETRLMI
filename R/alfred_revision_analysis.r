
#' Get Data on Series Revisions from ALFRED
#'
#' This function pulls data for a specified series from ALFRED, the archival version of the St. Louis Federal Reserve Economic Database, FRED.  It retrieves the data, then summarizes that data with the initial observation, the final observation, and the change between the two.
#'
#' @param series_id Character string (required).  This is the ALFRED series ID that will be returned.
#'
#' @return A tibble with observations for each date in the series, showing the initial, final, change, percent change, number of items, and number of unique values across all observations for that date.
#'
#' @examples
#' \dontrun{
#'
#' # Retrieving data for Nevada Total Nonfarm employment, seasonally adjusted.
#' nv_jobs <- alfred_revision_analysis("NVNA")
#'
#' # Revision analysis for Nevada's Unemployment Rate
#' nv_ur <- alfred_revision_analysis("NVUR")
#' }
#'
#' @importFrom dplyr group_by summarize
#' @importFrom alfred get_alfred_series
#' @export
alfred_revision_analysis <- function(series_id) {

  data_vintages <- alfred::get_alfred_series(series_id = series_id)

  data_releases <- data_vintages |>
    dplyr::group_by(date) |>
    dplyr::summarize(
      initial = .data[[series_id]][1],
      final = .data[[series_id]][length(.data[[series_id]])],
      change = final-initial,
      pct_change = change/initial,
      items = length(.data[[series_id]]),
      versions = length(unique(.data[[series_id]])),
      .groups = "drop"
    )

  return(data_releases)
}
