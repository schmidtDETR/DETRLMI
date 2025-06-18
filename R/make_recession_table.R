#' This function retrieves US recession data from the Federal Reserve Economic Data (FRED)
#' database and creates a table showing the peak and trough dates of recessions.
#' The function uses the USREC series which indicates recession periods.
#'
#' @param api_key Character string. FRED API key for accessing the data. If not provided,
#'   the function will look for the key in the `FRED_API_KEY` environment variable.
#'   You can obtain an API key from https://fred.stlouisfed.org/docs/api/api_key.html
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{peak}{Date of recession peak (start of recession)}
#'     \item{trough}{Date of recession trough (end of recession)}
#'     \item{periodyear}{Year of the recession peak}
#'   }
#'
#' @details The function processes the USREC series data to identify recession periods
#'   and their corresponding peak and trough dates. It handles the current date by
#'   adding a synthetic data point to ensure proper calculation of the most recent
#'   recession period if applicable.
#'
#' @examples
#' \dontrun{
#' # Using API key from environment variable
#' recession_data <- make_recession_table()
#'
#' # Using API key as argument
#' recession_data <- make_recession_table(api_key = "your_api_key_here")
#'
#' # View the results
#' print(recession_data)
#' }
#'
#' @importFrom dplyr mutate filter select rename
#' @importFrom lubridate ymd floor_date year %m+%
#' @importFrom data.table data.table rleid
#'
#' @seealso \code{\link{getFRED}} for the underlying data retrieval function
#' @seealso \url{https://fred.stlouisfed.org/} to access FRED directly
#'
#' @export
make_recession_table <- function(api_key){
  # Check for API key: use argument if provided, else check environment
  if (missing(api_key) || is.null(api_key) || api_key == "") {
    api_key <- Sys.getenv("FRED_API_KEY")
    if (identical(api_key, "") || is.null(api_key)) {
      stop("API key not provided. Supply it as `api_key` argument or set the `FRED_API_KEY` environment variable. API key can be obtained from https://fred.stlouisfed.org/docs/api/api_key.html")
    }
  }

  cur_date <- as.Date(floor_date(Sys.Date(), unit = "months"))
  df.recession <- getFRED(series_id = "USREC") |>
    mutate(value = as.numeric(value),
           date = ymd(date))

  df.recession <- data.table(df.recession)[,trend:=(value)][, counter := seq_len(.N), by=rleid(trend)][,trend:=ifelse(value>0,"recession","no recession")] |>
    filter(counter==1) |>
    select(date,trend)

  current_trend <- if_else(df.recession[nrow(df.recession),2] == "recession", "no recession", "recession")
  add_current <- data.frame(cur_date, current_trend)
  names(add_current) <- c("date", "trend")

  df.recession <- rbind(df.recession, add_current) |>
    mutate(date=date%m+% months(-1), peak=lag(date)) |>
    filter(trend=="no recession") |>
    select(peak,date) |>
    rename(trough=date) |>
    mutate(periodyear=year(peak))

  return(df.recession)
}
