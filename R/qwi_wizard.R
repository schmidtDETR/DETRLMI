#' @importFrom rlang %||%
NULL

########################
### Lookup Functions ###
########################

#' QWI Indicator Dictionary
#'
#' Returns a named character vector mapping Census QWI indicator codes
#' to human-readable descriptions.
#'
#' @return A named character vector of indicator codes and descriptions.
#' @export
#' @examples
#' dict <- qwi_dict_indicators()
#' dict["Emp"]
qwi_dict_indicators <- function() {
  c(
    "Emp" = "Beginning-of-Quarter Employment",
    "EmpEnd" = "End-of-Quarter Employment",
    "EmpS" = "Full-Quarter Employment (Stable)",
    "EmpTotal" = "Employment - Reference Quarter",
    "EmpSpv" = "Full-Quarter Employment in the Previous Quarter",
    "HirA" = "Hires (All Accessions)",
    "HirN" = "New Hires",
    "HirR" = "Recall Hires",
    "Sep" = "Separations (All)",
    "HirAEnd" = "End-of-Quarter Hires",
    "SepBeg" = "Beginning-of-Quarter Separations",
    "HirAEndRepl" = "Replacement Hires",
    "HirAEndR" = "End-of-Quarter Hiring Rate",
    "SepBegR" = "Beginning-of-Quarter Separation Rate",
    "SepS" = "Separations (Flows out of Full-Quarter Employment)",
    "SepSnx" = "Separations in the Next Quarter (Flows out of Full-Quarter Employment)",
    "TurnOvrS" = "Turnover (Stable)",
    "FrmJbGn" = "Firm Job Gains (Job Creation)",
    "FrmJbLs" = "Firm Job Loss (Job Destruction)",
    "FrmJbC" = "Firm Job Change (Net Change)",
    "FrmJbGnS" = "Firm Job Gains (Stable)",
    "FrmJbLsS" = "Firm Job Loss (Stable)",
    "FrmJbCS" = "Firm Job Change (Stable; Net Change)",
    "EarnS" = "Average Monthly Earnings (Full-Quarter Employment)",
    "EarnBeg" = "Average Monthly Earnings (Beginning-of-Quarter Employment)",
    "EarnHirAS" = "Average Monthly Earnings (All Hires into Full-Quarter Employment)",
    "EarnHirNS" = "Average Monthly Earnings (New Hires into Full-Quarter Employment)",
    "EarnSepS" = "Average Monthly Earnings (Flows out of Full-Quarter Employment)",
    "Payroll" = "Total Quarterly Payroll"
  )
}

#' QWI NAICS Industry Dictionary
#'
#' Returns a named character vector mapping 2-digit NAICS sector codes
#' to industry titles.
#'
#' @return A named character vector of NAICS sector codes and titles.
#' @export
#' @examples
#' qwi_dict_industries()
qwi_dict_industries <- function() {
  c(
    "11" = "Agriculture, Forestry, Fishing and Hunting",
    "21" = "Mining, Quarrying, and Oil and Gas Extraction",
    "22" = "Utilities",
    "23" = "Construction",
    "31-33" = "Manufacturing",
    "42" = "Wholesale Trade",
    "44-45" = "Retail Trade",
    "48-49" = "Transportation and Warehousing",
    "51" = "Information",
    "52" = "Finance and Insurance",
    "53" = "Real Estate and Rental and Leasing",
    "54" = "Professional, Scientific, and Technical Services",
    "55" = "Management of Companies and Enterprises",
    "56" = "Administrative and Support and Waste Management",
    "61" = "Educational Services",
    "62" = "Health Care and Social Assistance",
    "71" = "Arts, Entertainment, and Recreation",
    "72" = "Accommodation and Food Services",
    "81" = "Other Services (except Public Administration)",
    "92" = "Public Administration"
  )
}

#################################
### Function to Retrieve Data ###
#################################

#' Fetch QWI Data Chunk
#'
#' Queries the U.S. Census Bureau LEHD Quarterly Workforce Indicators (QWI) API
#' for specified indicators, geographic levels, and years.
#'
#' @param endpoint Character. The demographic crosstab endpoint: `"sa"` (Sex by Age),
#'   `"se"` (Sex by Education), or `"rh"` (Race by Ethnicity).
#' @param get_vars Character vector of indicators and demographic variables to retrieve.
#' @param geography Character. Geographic hierarchy: `"state"`, `"county"`, `"cbsa"`,
#'   or `"wia"`. Default is `"county"`.
#' @param state_fips Two-digit state FIPS code as a string (e.g., `"06"` for California).
#' @param years Numeric vector of years (e.g., `2020:2022`).
#' @param industry Optional NAICS industry code (e.g., `"31-33"`). Default is `NULL`.
#' @param key Census API Key. Defaults to the `CENSUS_API_KEY` environment variable.
#' @param label_data Logical. If `TRUE` (default), appends human-readable `*_label`
#'   columns for demographic codes.
#'
#' @return A data frame containing the requested QWI observations with numeric
#'   indicators converted to numeric types.
#' @export
get_qwi_chunk <- function(endpoint = "sa",
                          get_vars = c("Emp", "sex", "agegrp"),
                          geography = "county",
                          state_fips,
                          years,
                          industry = NULL,
                          key = Sys.getenv("CENSUS_API_KEY"),
                          label_data = TRUE) {

  base_url <- sprintf("https://api.census.gov/data/timeseries/qwi/%s", endpoint)

  if (geography == "state") {
    geo_for <- paste0("state:", state_fips)
    geo_in <- NULL
  } else if (geography == "county") {
    geo_for <- "county:*"
    geo_in <- paste0("state:", state_fips)
  } else if (geography == "cbsa") {
    geo_for <- "metropolitan statistical area/micropolitan statistical area:*"
    geo_in <- paste0("state:", state_fips)
  } else if (geography == "wia") {
    geo_for <- "workforce investment area:*"
    geo_in <- paste0("state:", state_fips)
  } else {
    cli::cli_abort("Unsupported geography: {.val {geography}}. Use 'state', 'county', 'cbsa', or 'wia'.")
  }

  # Dictionaries for labeling demographic variables
  qwi_labels <- list(
    sex = c("0" = "All Sexes", "1" = "Male", "2" = "Female"),
    agegrp = c("A00" = "All Ages", "A01" = "14-18", "A02" = "19-21", "A03" = "22-24",
               "A04" = "25-34", "A05" = "35-44", "A06" = "45-54", "A07" = "55-64",  "A08" = "65 and older"),
    education = c("E0" = "All Education Categories",
                  "E1" = "Less than high school",
                  "E2" = "High school or equivalent, no college",
                  "E3" = "Some college or Associate degree",
                  "E4" = "Bachelor's degree or advanced degree",
                  "E5" = "Educational attainment not available"),
    race = c("A0" = "All Races", "A1" = "White Alone", "A2" = "Black or African American Alone",
             "A3" = "American Indian or Alaska Native Alone", "A4" = "Asian Alone",
             "A5" = "Native Hawaiian or Other Pacific Islander Alone", "A7" = "Two or More Race Groups"),
    ethnicity = c("A0" = "All Ethnicities", "A1" = "Not Hispanic or Latino", "A2" = "Hispanic or Latino")
  )


  results <- purrr::map_dfr(years, function(yr) {

    query_params <- list(
      get = paste(get_vars, collapse = ","),
      `for` = geo_for,
      time = as.character(yr),
      key = key
    )
    if (!is.null(geo_in)) {
      query_params$`in` <- geo_in
    }

    if (!is.null(industry)) {
      query_params$industry <- industry
    }

    req <- httr2::request(base_url) |>
      httr2::req_url_query(!!!query_params) |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2)

    resp <- tryCatch({
      httr2::req_perform(req)
    }, error = function(e) {
      if (inherits(e, "httr2_error")) {
        last_resp <- httr2::last_response()
        api_msg <- if (!is.null(last_resp)) httr2::resp_body_string(last_resp) else conditionMessage(e)
        cli::cli_abort(c(
          "x" = "Census API rejected the request for year {yr}.",
          "i" = "API Message: {.val {api_msg}}"
        ))
      }
      stop(e)
    })

    if (httr2::resp_status(resp) == 204) {
      cli::cli_alert_warning("No data found for FIPS {state_fips} in {yr}")
      return(data.frame())
    }

    # Parse JSON response
    raw_json <- jsonlite::fromJSON(httr2::resp_body_string(resp))
    df <- as.data.frame(raw_json[-1, , drop = FALSE], stringsAsFactors = FALSE)
    names(df) <- raw_json[1, ]

    # Apply human-readable labels as new columns
    if (label_data) {
      for (var in names(qwi_labels)) {
        if (var %in% names(df)) {
          new_col_name <- paste0(var, "_label")
          matched <- qwi_labels[[var]][df[[var]]]
          df[[new_col_name]] <- ifelse(is.na(matched), df[[var]], unname(matched))
        }
      }
    }

    # Convert indicators to numeric
    label_cols <- paste0(names(qwi_labels), "_label")
    skip_cols <- c(names(qwi_labels), label_cols, "time", "year", "quarter",
                   "state", "county", "metropolitan statistical area/micropolitan statistical area",
                   "workforce investment area", "industry")

    num_cols <- setdiff(names(df), skip_cols)
    df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(as.character(x)))

    Sys.sleep(0.5)

    return(df)
  })

  return(results)
}

#################################
### Wizard and Script Builder ###
#################################

#' Generate R Script for QWI Extraction
#'
#' Constructs a standalone, reproducible R script string configured to download
#' specified QWI data using `get_qwi_chunk()`.
#'
#' @param endpoint A list containing endpoint metadata (`id`, `title`, and `vars`).
#' @param years Numeric vector of years (e.g., `2020:2022`). Default is `2022`.
#' @param geography Geography level (e.g., `"state"`, `"county"`, `"cbsa"`, `"wia"`). Default is `"county"`.
#' @param state_fips Two-digit state FIPS code as a string (e.g., `"06"`). Default is `"06"`.
#' @param indicators Character vector of QWI indicator codes. Default is `c("Emp")`.
#' @param industry Optional NAICS industry code (e.g., `"31-33"`). Default is `NULL`.
#' @param var_name Character variable name assigned in the generated script. Default is `"qwi_data"`.
#'
#' @return A character string containing the generated R script.
#' @export
generate_qwi_script <- function(endpoint,
                                years = 2022,
                                geography = "county",
                                state_fips = "06",
                                indicators = c("Emp"),
                                industry = NULL,
                                var_name = "qwi_data") {

  years_str <- if (length(years) > 1) {
    paste0(min(years), ":", max(years))
  } else {
    as.character(years)
  }

  inds_str <- paste(paste0('"', indicators, '"'), collapse = ", ")
  demo_vars_str <- paste(paste0('"', endpoint$vars, '"'), collapse = ", ")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ind_arg_str <- if (!is.null(industry)) paste0("  industry = \"", industry, "\",\n") else ""

  ind_dict <- qwi_dict_indicators()
  selected_labels <- ind_dict[indicators]

  cat_lines <- paste0('"', indicators, ' - ', selected_labels, '\\n"')
  indicator_print_block <- paste0(
    "cat(\"\\nReturning the following indicators:\\n\",\n    ",
    paste(cat_lines, collapse = ",\n    "),
    ", \"\\n\")\n"
  )

  script <- paste0(
    "# =========================================================================\n",
    "# QWI Data Script: ", endpoint$title, "\n",
    "# Generated: ", timestamp, "\n",
    "# Endpoint ID: ", endpoint$id, "\n",
    "# Demographic Crosstabs: ", paste(endpoint$vars, collapse = ", "), "\n",
    "# For more information: https://www.census.gov/data/developers/data-sets/qwi.html\n",
    "# =========================================================================\n\n",
    "# 1. Load required libraries\n",
    "# library(qwiwizard)\n",
    "library(dplyr)\n\n",
    "# 2. Census API Key Check\n",
    "if (Sys.getenv(\"CENSUS_API_KEY\") == \"\") {\n",
    "  message(\"Note: CENSUS_API_KEY environment variable is not set. Requests may be rate-limited.\")\n",
    "}\n\n",
    "# 3. Define Variables\n",
    "indicators <- c(", inds_str, ")\n",
    "demo_vars <- c(", demo_vars_str, ")\n",
    "get_vars <- c(indicators, demo_vars)\n\n",
    "# 4. Fetch QWI Data\n",
    var_name, " <- get_qwi_chunk(\n",
    "  endpoint = \"", endpoint$id, "\",\n",
    "  get_vars = get_vars,\n",
    "  geography = \"", geography, "\",\n",
    "  state_fips = \"", state_fips, "\",\n",
    "  years = ", years_str, ",\n",
    ind_arg_str,
    "  label_data = TRUE\n",
    ")\n\n",
    "# 5. Inspect returned data\n",
    indicator_print_block,
    "glimpse(", var_name, ")\n"
  )

  script
}

#' Interactive QWI Script Wizard
#'
#' Launches an interactive console assistant that guides you through building
#' a QWI API query and generates a ready-to-run R script.
#'
#' @param output_dir Optional destination folder where the script will be saved.
#' @param filename Optional destination file name (e.g., `"my_qwi_extract.R"`).
#'
#' @return Invisibly returns the normalized file path of the generated script.
#' @export
qwi_wizard <- function(output_dir = NULL, filename = NULL) {
  if (!interactive()) {
    cli::cli_abort("{.fn qwi_wizard} requires an interactive R session.")
  }

  cli::cli_h1("LEHD QWI Script Wizard")
  cli::cli_text("Configure your QWI data query and generate a reproducible R script.")
  cli::cli_rule()

  # Step 1: Endpoint Selection
  selected_endpoint <- prompt_qwi_endpoint()
  if (is.null(selected_endpoint)) return(invisible(NULL))

  cli::cli_alert_success("Selected Endpoint: {.strong {selected_endpoint$title}}")
  cli::cli_text("{.field Crosstabs}: {paste(selected_endpoint$vars, collapse = ', ')}")
  cli::cli_rule()

  # Step 2: Query Parameters
  params <- prompt_qwi_parameters()

  # Step 3: Destination Selection
  dest <- prompt_qwi_destination(
    endpoint_id = selected_endpoint$id,
    year = min(params$years),
    output_dir = output_dir,
    filename = filename
  )

  # Step 4: Generate Script
  script_content <- generate_qwi_script(
    endpoint = selected_endpoint,
    years = params$years,
    geography = params$geography,
    state_fips = params$state_fips,
    indicators = params$indicators,
    industry = params$industry,
    var_name = paste0("qwi_", selected_endpoint$id)
  )

  # Ensure target directory exists
  dir_to_create <- dirname(dest$path)
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }

  writeLines(script_content, con = dest$path)

  cli::cli_rule()
  cli::cli_alert_success("Created R script: {.file {dest$path}}")

  # Clipboard integration via Suggested clipr
  if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
    tryCatch({
      clipr::write_clip(script_content)
      cli::cli_alert_info("Code copied to system clipboard.")
    }, error = function(e) NULL)
  }

  # Open in editor
  tryCatch({
    utils::file.edit(dest$path)
  }, error = function(e) {
    cli::cli_alert_info("Open file with: {.code file.edit(\"{dest$path}\")}")
  })

  cli::cli_h2("Generated Code Preview")
  cli::cli_code(script_content, language = "r")

  invisible(dest$path)
}

# -----------------------------------------------------------------------------
# Internal Prompt Helpers
# -----------------------------------------------------------------------------

#' Prompt for QWI Endpoint Selection
#' @noRd
prompt_qwi_endpoint <- function() {
  cli::cli_h2("Step 1: Select QWI Crosstab Endpoint")

  endpoints <- list(
    list(id = "sa", title = "Sex by Age (SA)", vars = c("sex", "agegrp")),
    list(id = "se", title = "Sex by Education (SE)", vars = c("sex", "education")),
    list(id = "rh", title = "Race by Ethnicity (RH)", vars = c("race", "ethnicity"))
  )

  menu_options <- vapply(endpoints, function(x) x$title, character(1))
  choice <- utils::menu(menu_options, title = "Select Demographic Crosstab:")

  if (choice == 0) {
    cli::cli_alert_danger("Wizard cancelled.")
    return(NULL)
  }

  endpoints[[choice]]
}

#' Prompt for QWI Parameters
#' @noRd
prompt_qwi_parameters <- function() {
  cli::cli_h2("Step 2: Configure Download Parameters")

  # 1. Geography
  geos <- c("state" = "State", "county" = "County", "cbsa" = "Metro/Micro Area", "wia" = "Workforce Investment Area")
  geo_idx <- utils::menu(unname(geos), title = "Select Geography Level:")
  if (geo_idx == 0) stop("Wizard cancelled.", call. = FALSE)
  geography <- names(geos)[geo_idx]

  # 2. State FIPS
  state_input <- readline(prompt = "Enter 2-digit State FIPS code (e.g., '06' for CA): ")
  state_fips <- sprintf("%02d", as.numeric(trimws(state_input)))
  if (is.na(state_fips) || state_fips == "NA") state_fips <- "00"

  # 3. Years
  year_input <- readline(prompt = "Enter year(s) to fetch (e.g., '2022' or '2019:2022'): ")
  years <- tryCatch({
    parsed <- eval(parse(text = year_input))
    if (!is.numeric(parsed)) 2022 else parsed
  }, error = function(e) 2022)

  # 4. Indicators (Grouped vs Manual)
  raw_dict <- qwi_dict_indicators()
  all_inds <- names(raw_dict)

  group_emp <- grep("^Emp", all_inds, value = TRUE)
  group_earn <- grep("^Earn", all_inds, value = TRUE)
  group_turn <- grep("^TurnOvr|^Sep|^Hir", all_inds, value = TRUE)
  group_firm <- grep("^Frm", all_inds, value = TRUE)

  cli::cli_h3("Indicator Selection")
  ind_menu <- c(
    "All Indicators (Warning: May trigger cardinality errors on county-level queries)",
    "All Employment Indicators (Emp...)",
    "All Earnings Indicators (Earn...)",
    "All Turnover & Flow Indicators (TurnOvr..., Hir..., Sep...)",
    "All Firm-based Indicators (Frm...)",
    "Select Manually"
  )

  ind_choice <- utils::menu(ind_menu, title = "How would you like to select indicators?")

  if (ind_choice == 1) {
    indicators <- all_inds
  } else if (ind_choice == 2) {
    indicators <- group_emp
  } else if (ind_choice == 3) {
    indicators <- group_earn
  } else if (ind_choice == 4) {
    indicators <- group_turn
  } else if (ind_choice == 5) {
    indicators <- group_firm
  } else if (ind_choice == 6) {
    menu_names <- paste0(unname(raw_dict), " (", names(raw_dict), ")")
    ind_dict <- stats::setNames(names(raw_dict), menu_names)

    cli::cli_text("Select one or more indicators (0 to cancel/skip):")
    selected_names <- utils::select.list(names(ind_dict), multiple = TRUE, graphics = FALSE)

    if (length(selected_names) == 0) {
      cli::cli_alert_info("No selection made. Defaulting to Emp and HirA.")
      indicators <- c("Emp", "HirA")
    } else {
      indicators <- unname(ind_dict[selected_names])
    }
  } else {
    stop("Wizard cancelled.", call. = FALSE)
  }

  # 5. Industry Filter
  cli::cli_h3("Industry Filter")
  ind_opts <- c("All Industries (No Filter)", "Select a specific NAICS Sector", "Enter custom NAICS code")
  ind_sel <- utils::menu(ind_opts, title = "Would you like to filter by Industry?")

  industry_code <- NULL
  if (ind_sel == 2) {
    raw_ind <- qwi_dict_industries()
    ind_menu_names <- paste0(unname(raw_ind), " (", names(raw_ind), ")")
    ind_dict_map <- stats::setNames(names(raw_ind), ind_menu_names)

    cli::cli_text("Select a NAICS Sector:")
    selected_ind_name <- utils::select.list(names(ind_dict_map), multiple = FALSE, graphics = FALSE)
    if (selected_ind_name != "") {
      industry_code <- unname(ind_dict_map[selected_ind_name])
    }
  } else if (ind_sel == 3) {
    custom_ind <- readline("Enter NAICS code (e.g., '311' for Food Mfg): ")
    if (nzchar(trimws(custom_ind))) {
      industry_code <- trimws(custom_ind)
    }
  }

  list(
    geography = geography,
    state_fips = state_fips,
    years = years,
    indicators = indicators,
    industry = industry_code
  )
}

#' Prompt for Destination Selection
#' @noRd
prompt_qwi_destination <- function(endpoint_id, year, output_dir = NULL, filename = NULL) {
  if (!is.null(filename) && nzchar(trimws(filename))) {
    fname <- trimws(filename)
    if (!grepl("\\.[Rr]$", fname)) fname <- paste0(fname, ".R")
    target_dir <- output_dir %||% "."
    return(list(is_temp = FALSE, path = file.path(target_dir, fname)))
  }

  default_fname <- paste0("get_qwi_", endpoint_id, "_", year, ".R")

  cli::cli_h2("Step 3: Choose Output Destination")
  dest_options <- c(
    "Temporary scratchpad file (Quick copy & paste, opens in editor)",
    "Save file to directory"
  )
  dest_choice <- utils::menu(dest_options, title = "Where would you like to create the script?")

  if (dest_choice == 1) {
    tmp <- tempfile(pattern = paste0("qwi_", endpoint_id, "_"), fileext = ".R")
    return(list(is_temp = TRUE, path = tmp))
  }

  fname_input <- readline(prompt = paste0("Filename [default: ", default_fname, "]: "))
  fname_trimmed <- trimws(fname_input)
  final_fname <- if (nzchar(fname_trimmed)) {
    if (!grepl("\\.[Rr]$", fname_trimmed)) paste0(fname_trimmed, ".R") else fname_trimmed
  } else {
    default_fname
  }

  target_dir <- output_dir %||% "."
  list(is_temp = FALSE, path = file.path(target_dir, final_fname))
}
