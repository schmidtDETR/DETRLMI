#' Get the Most Recent BLS QCEW Industry Data (with caching and validation)
#'
#' Retrieves the most recent available **Quarterly Census of Employment and Wages (QCEW)** CSV file.
#' Looks back two to three quarters (BLS data typically lags) and downloads the first available file.
#'
#' @param industry_code Character string of the industry code (default `"10"` = Total all ownerships).
#' @param quarters_to_check Numeric vector of quarters to look back (default `c(2,3)`).
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{A `data.table` containing the QCEW data for the most recent available quarter.}
#'   \item{check_quarter}{The quarter of the data returned.}
#'   \item{check_year}{The year of the data returned.}
#' }
#' @export
get_latest_qcew_data <- function(industry_code = "10", quarters_to_check = c(2,3)) {
  current_date <- Sys.Date()
  current_year <- lubridate::year(current_date)
  current_quarter <- lubridate::quarter(current_date)

  cache_dir <- get_cache_dir()

  for (offset in quarters_to_check) {
    check_quarter <- current_quarter - offset
    if (check_quarter <= 0) {
      check_quarter <- check_quarter + 4
      check_year <- current_year - 1
    } else {
      check_year <- current_year
    }

    url <- paste0("https://data.bls.gov/cew/data/api/", check_year, "/", check_quarter, "/industry/", industry_code, ".csv")
    destfile <- file.path(cache_dir, paste0("bls_", check_year, "_Q", check_quarter, ".csv"))

    # Remove directory if it exists at file path
    if (dir.exists(destfile)) unlink(destfile, recursive = TRUE)

    # HEAD check
    head_resp <- httr::HEAD(url)
    if (httr::status_code(head_resp) != 200) next

    download_if_new(url, destfile, check = "size")

    if (file.exists(destfile)) {
      message("Returning data for ", check_year, " Q", check_quarter)
      return(list(
        data = data.table::fread(destfile),
        check_quarter = check_quarter,
        check_year = check_year
      ))
    }
  }

  stop("No recent QCEW data found — the BLS may not have released recent quarters yet.")
}
