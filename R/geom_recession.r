
#' Add recession shading to ggplot2 charts
#'
#' This function adds shaded rectangular areas to indicate US recession periods
#' on time series charts. It automatically fetches recession data from FRED or
#' accepts user-provided recession data.
#'
#' @param data A data frame containing recession data with peak, trough, and period
#'   columns. If `NULL` (default), recession data will be automatically fetched
#'   from FRED using `make_recession_table()`.
#' @param api_key Character string. FRED API key for accessing recession data when
#'   `data = NULL`. If not provided, the function will look for the key in the
#'   `FRED_API_KEY` environment variable.
#' @param start_year Numeric. The earliest year for which to show recessions.
#'   This parameter is required and filters the recession data to only include
#'   recessions that began in or after this year.
#' @param end_year Numeric. Optional. The latest year for which to show recessions.
#'   If provided, filters recession data to only include recessions that began
#'   in or before this year.
#' @param peak_col Character string. Name of the column containing recession peak
#'   (start) dates. Default is "peak".
#' @param trough_col Character string. Name of the column containing recession trough
#'   (end) dates. Default is "trough".
#' @param period_col Character string. Name of the column containing the recession
#'   period year. Default is "periodyear".
#' @param fill Character string. Fill color for recession shading. Default is "gray".
#' @param alpha Numeric. Transparency level for recession shading, between 0 and 1.
#'   Default is 0.3.
#'
#' @return A ggplot2 layer (geom_rect) that can be added to a ggplot object.
#'
#' @details The function creates rectangular shaded areas spanning from ymin = -Inf
#'   to ymax = Inf, automatically adapting to any y-axis scale. Recession periods
#'   are filtered based on the `start_year` parameter to show only relevant recessions
#'   for the time period being plotted.
#'
#'   When `data = NULL`, the function calls `make_recession_table()` to fetch current
#'   recession data from the Federal Reserve Economic Data (FRED) database.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Basic usage with automatic data fetching
#' economic_data %>%
#'   ggplot(aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_recession(start_year = 2000)
#'
#' # With custom styling
#' economic_data %>%
#'   ggplot(aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_recession(start_year = 2007, fill = "red", alpha = 0.1)
#'
#' # Using date range from your data
#' min_year <- year(min(economic_data$date))
#' economic_data %>%
#'   ggplot(aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_recession(start_year = min_year)
#'
#' # With both start and end years
#' economic_data %>%
#'   ggplot(aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_recession(start_year = 2000, end_year = 2020)
#'
#' # Using your own recession data
#' recession_data <- make_recession_table()
#' economic_data %>%
#'   ggplot(aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_recession(data = recession_data, start_year = 2007)
#'
#' # Works with any type of plot
#' economic_data %>%
#'   filter(date > "2007-07-31") %>%
#'   ggplot() +
#'   geom_recession(start_year = 2007) +
#'   geom_col(aes(x = date, y = change)) +
#'   scale_y_continuous(labels = scales::percent)
#' }
#'
#' @seealso \code{\link{make_recession_table}} for fetching recession data
#' @seealso \code{\link[ggplot2]{geom_rect}} for the underlying geom
#'
#' @importFrom ggplot2 geom_rect aes sym geom_blank
#' @importFrom lubridate year
#' @export
geom_recession <- function(data = NULL,
                           api_key = NULL,
                           start_year = NULL,
                           end_year = NULL,
                           peak_col = "peak",
                           trough_col = "trough",
                           period_col = "periodyear",
                           fill = "gray",
                           alpha = 0.3) {

  # If no data provided, automatically fetch recession data
  if (is.null(data)) {
    message("Fetching recession data from FRED...")
    data <- make_recession_table(api_key = api_key)
  }

  # Require start_year parameter
  if (is.null(start_year)) {
    stop("start_year parameter is required. Please specify the earliest year to show recessions.")
  }

  # Filter by year range
  if (!is.null(start_year)) {
    data <- data[data[[period_col]] >= start_year, ]
  }
  if (!is.null(end_year)) {
    data <- data[data[[period_col]] <= end_year, ]
  }

  # Return empty layer if no recessions in range
  if (nrow(data) == 0) {
    return(geom_blank())
  }

  # Simple geom_rect approach
  geom_rect(
    data = data,
    aes(xmin = !!sym(peak_col), xmax = !!sym(trough_col), ymin = -Inf, ymax = Inf),
    fill = fill,
    alpha = alpha,
    inherit.aes = FALSE
  )
}

#' Get minimum year from a date column
#'
#' A convenience function to extract the minimum year from a date column,
#' useful for determining the `start_year` parameter for `geom_recession()`.
#'
#' @param date_column A vector of Date objects.
#'
#' @return Numeric. The year of the minimum date in the column.
#'
#' @examples
#' \dontrun{
#' # Get the earliest year from your data
#' min_year <- get_min_year(economic_data$date)
#'
#' # Use it with geom_recession
#' economic_data %>%
#'   ggplot(aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_recession(start_year = min_year)
#' }
#'
#' @seealso \code{\link{geom_recession}}
#'
#' @importFrom lubridate year
#' @export
get_min_year <- function(date_column) {
  year(min(date_column, na.rm = TRUE))
}
