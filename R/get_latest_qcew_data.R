#' Get the Most Recent BLS QCEW Industry Data
#'
#' This function checks recent quarters (by default, two previous quarters)
#' of BLS Quarterly Census of Employment and Wages (QCEW) data for industry code 10,
#' and downloads the most recent available CSV file. The function uses
#' [download_if_new()] to cache data locally and avoid unnecessary re-downloads.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{A `data.table` containing the BLS data for the latest available quarter.}
#'   \item{check_quarter}{The quarter of the data returned.}
#'   \item{check_year}{The year of the data returned.}
#' }
#'
#' @importFrom data.table fread
#' @importFrom httr GET status_code
#' @export
get_latest_qcew_data <- function() {
  current_date <- Sys.Date()
  current_year <- lubridate::year(current_date)
  current_quarter <- lubridate::quarter(current_date)

  # Check up to two previous quarters
  quarters_to_check <- c(0, 1, 2)

  for (offset in quarters_to_check) {
    check_quarter <- current_quarter - offset
    check_year <- current_year

    if (check_quarter <= 0) {
      check_quarter <- check_quarter + 4
      check_year <- current_year - 1
    }

    url <- paste0("https://data.bls.gov/cew/data/api/", check_year, "/", check_quarter, "/industry/10.csv")
    destfile <- file.path(get_cache_dir("bls"), paste0("bls_", check_year, "_Q", check_quarter, ".csv"))

    # Use helper to download (if newer)
    download_if_new(url, destfile)

    # If the file now exists, read it and return
    if (file.exists(destfile)) {
      return(list(
        data = data.table::fread(destfile),
        check_quarter = check_quarter,
        check_year = check_year
      ))
    }
  }

  stop("No recent BLS data found.")
}
