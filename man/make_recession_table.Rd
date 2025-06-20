% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_recession_table.R
\name{make_recession_table}
\alias{make_recession_table}
\title{This function retrieves US recession data from the Federal Reserve Economic Data (FRED)
database and creates a table showing the peak and trough dates of recessions.
The function uses the USREC series which indicates recession periods.}
\usage{
make_recession_table(api_key)
}
\arguments{
\item{api_key}{Character string. FRED API key for accessing the data. If not provided,
the function will look for the key in the `FRED_API_KEY` environment variable.
You can obtain an API key from https://fred.stlouisfed.org/docs/api/api_key.html}
}
\value{
A data frame with the following columns:
  \describe{
    \item{peak}{Date of recession peak (start of recession)}
    \item{trough}{Date of recession trough (end of recession)}
    \item{periodyear}{Year of the recession peak}
  }
}
\description{
This function retrieves US recession data from the Federal Reserve Economic Data (FRED)
database and creates a table showing the peak and trough dates of recessions.
The function uses the USREC series which indicates recession periods.
}
\details{
The function processes the USREC series data to identify recession periods
  and their corresponding peak and trough dates. It handles the current date by
  adding a synthetic data point to ensure proper calculation of the most recent
  recession period if applicable.
}
\examples{
\dontrun{
# Using API key from environment variable
recession_data <- make_recession_table()

# Using API key as argument
recession_data <- make_recession_table(api_key = "your_api_key_here")

# View the results
print(recession_data)
}

}
\seealso{
\code{\link{getFRED}} for the underlying data retrieval function

\url{https://fred.stlouisfed.org/} to access FRED directly
}
