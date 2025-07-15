#' Streamline connection to WID Database
#'
#' Wrap the con/query/disconnect workflow to query a SQL database into a function that allows use of a single function.
#'
#' @param query Character. The SQL query that you with to execute.
#' @param ODBC Character. The name of an ODBC connection on your local machine that the script will use.
#'
#' @return The results of the SQL query, run with as.is=TRUE.
#'
#' @details
#' The function uses RODBC to run a query (required input) against a named ODBC connection (default "WID_DB").
#' Alternative ODBC connection name may be provided with the ODBC argument.
#'
#' @importFrom RODBC odbcConnect sqlQuery odbcClose
#'
#' @examples
#' \dontrun{
#' ces_query <- run_query("SELECT * FROM dbo.ces")
#'}
#' @export
#'

run_query <- function(query, ODBC = "WID_DB"){

  con <- odbcConnect(ODBC)
  query_result <- sqlQuery(con, as.is = TRUE, query)
  odbcClose(con)

  return(query_result)

}



