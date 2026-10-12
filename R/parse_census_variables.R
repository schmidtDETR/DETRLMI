#' Load and Parse Census Variables
#'
#' @description Retrieves variables for a specific year and census dataset, then cleans
#' and expands the `label` column into hierarchical columns for easier filtering and analysis.
#'
#' @param year A numeric value or string specifying the vintage of the census dataset.
#' @param dataset A string specifying the dataset to load (e.g., "acs5", "acs1").
#'
#' @return A tibble of census variables with cleaned, level-separated labels.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch and parse variables for the 2021 ACS 5-year dataset
#' parsed_vars <- parsed_variables(year = 2021, dataset = "acs5")
#' }
parsed_variables <- function(year, dataset) {

  # Fetch variables from tidycensus
  out <- tidycensus::load_variables(year = year, dataset = dataset)

  # Clean and tokenize labels
  out <- out |>
    dplyr::mutate(
      table = stringr::str_remove(.data$name, "_.*"),
      year = year,
      dataset = dataset,
      label_clean = trimws(.data$label, which = "both"),
      label_clean = stringr::str_remove_all(.data$label_clean, ":"),
      label_clean = stringr::str_remove_all(.data$label_clean, "--"),
      label_tokens = stringr::str_split(.data$label_clean, "!!"),
      label_tokens = purrr::map(.data$label_tokens, \(x) {
        x |> stringr::str_trim() |> purrr::discard(\(y) y == "")
      }),
      label_depth = purrr::map_int(.data$label_tokens, length)
    ) |>
    dplyr::rename("variable" = "name")

  max_depth <- max(out$label_depth, na.rm = TRUE)

  # Create dynamic label level columns based on max depth
  if (is.finite(max_depth) && max_depth > 0) {
    for (i in seq_len(max_depth)) {
      out[[paste0("label_level_", i - 1)]] <- purrr::map_chr(
        out$label_tokens,
        \(x) if (length(x) >= i) x[[i]] else NA_character_
      )
    }
  }

  # Drop intermediate parsing columns
  out <- out |>
    dplyr::select(
      -.data$label_tokens,
      -.data$label_depth,
      -.data$label_clean,
      -.data$label_level_0
    )

  return(out)
}
