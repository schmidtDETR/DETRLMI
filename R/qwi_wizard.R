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
qwi_dict_indicators <- function() {
  c(
    "Emp"         = "Beginning-of-Quarter Employment",
    "EmpEnd"      = "End-of-Quarter Employment",
    "EmpS"        = "Full-Quarter Employment (Stable)",
    "EmpTotal"    = "Employment - Reference Quarter",
    "EmpSpv"      = "Full-Quarter Employment in the Previous Quarter",
    "HirA"        = "Hires (All Accessions)",
    "HirN"        = "New Hires",
    "HirR"        = "Recall Hires",
    "Sep"         = "Separations (All)",
    "HirAEnd"     = "End-of-Quarter Hires",
    "SepBeg"      = "Beginning-of-Quarter Separations",
    "HirAEndRepl" = "Replacement Hires",
    "HirAEndR"    = "End-of-Quarter Hiring Rate",
    "SepBegR"     = "Beginning-of-Quarter Separation Rate",
    "SepS"        = "Separations (Flows out of Full-Quarter Employment)",
    "SepSnx"      = "Separations in the Next Quarter (Flows out of Full-Quarter Employment)",
    "TurnOvrS"    = "Turnover (Stable)",
    "FrmJbGn"     = "Firm Job Gains (Job Creation)",
    "FrmJbLs"     = "Firm Job Loss (Job Destruction)",
    "FrmJbC"      = "Firm Job Change (Net Change)",
    "FrmJbGnS"    = "Firm Job Gains (Stable)",
    "FrmJbLsS"    = "Firm Job Loss (Stable)",
    "FrmJbCS"     = "Firm Job Change (Stable; Net Change)",
    "EarnS"       = "Average Monthly Earnings (Full-Quarter Employment)",
    "EarnBeg"     = "Average Monthly Earnings (Beginning-of-Quarter Employment)",
    "EarnHirAS"   = "Average Monthly Earnings (All Hires into Full-Quarter Employment)",
    "EarnHirNS"   = "Average Monthly Earnings (New Hires into Full-Quarter Employment)",
    "EarnSepS"    = "Average Monthly Earnings (Flows out of Full-Quarter Employment)",
    "Payroll"     = "Total Quarterly Payroll"
  )
}

#' QWI NAICS Industry Dictionary
#'
#' Returns a named character vector mapping 2-digit NAICS sector codes
#' to industry titles.
#'
#' @return A named character vector of NAICS sector codes and titles.
#' @export
qwi_dict_industries <- function() {
  c(
    "11"    = "Agriculture, Forestry, Fishing and Hunting",
    "21"    = "Mining, Quarrying, and Oil and Gas Extraction",
    "22"    = "Utilities",
    "23"    = "Construction",
    "31-33" = "Manufacturing",
    "42"    = "Wholesale Trade",
    "44-45" = "Retail Trade",
    "48-49" = "Transportation and Warehousing",
    "51"    = "Information",
    "52"    = "Finance and Insurance",
    "53"    = "Real Estate and Rental and Leasing",
    "54"    = "Professional, Scientific, and Technical Services",
    "55"    = "Management of Companies and Enterprises",
    "56"    = "Administrative and Support and Waste Management",
    "61"    = "Educational Services",
    "62"    = "Health Care and Social Assistance",
    "71"    = "Arts, Entertainment, and Recreation",
    "72"    = "Accommodation and Food Services",
    "81"    = "Other Services (except Public Administration)",
    "92"    = "Public Administration"
  )
}

#' State Postal Abbreviation and FIPS Code Mapping
#' @noRd
qwi_state_lookup <- function() {
  c(
    "AL" = "01", "AK" = "02", "AZ" = "04", "AR" = "05", "CA" = "06",
    "CO" = "08", "CT" = "09", "DE" = "10", "DC" = "11", "FL" = "12",
    "GA" = "13", "HI" = "15", "ID" = "16", "IL" = "17", "IN" = "18",
    "IA" = "19", "KS" = "20", "KY" = "21", "LA" = "22", "ME" = "23",
    "MD" = "24", "MA" = "25", "MI" = "26", "MN" = "27", "MS" = "28",
    "MO" = "29", "MT" = "30", "NE" = "31", "NV" = "32", "NH" = "33",
    "NJ" = "34", "NM" = "35", "NY" = "36", "NC" = "37", "ND" = "38",
    "OH" = "39", "OK" = "40", "OR" = "41", "PA" = "42", "RI" = "44",
    "SC" = "45", "SD" = "46", "TN" = "47", "TX" = "48", "UT" = "49",
    "VT" = "50", "VA" = "51", "WA" = "53", "WV" = "54", "WI" = "55",
    "WY" = "56", "PR" = "72", "US" = "00"
  )
}

#' Standardize State Input (Supports '00', 'US', FIPS, or Postal Codes)
#' @noRd
qwi_normalize_state <- function(input) {
  if (is.null(input) || !nzchar(trimws(as.character(input)))) {
    return("00")
  }
  clean <- toupper(trimws(as.character(input)))
  dict <- qwi_state_lookup()

  # Treat '00' and 'US' as National scope
  if (clean %in% c("00", "US", "NATIONAL")) {
    return("00")
  }

  if (clean %in% names(dict)) {
    return(unname(dict[clean]))
  }

  if (grepl("^\\d{1,2}$", clean)) {
    fips_padded <- sprintf("%02d", as.numeric(clean))
    if (fips_padded %in% unname(dict) || fips_padded == "00") {
      return(fips_padded)
    }
  }

  cli::cli_alert_warning("Unrecognized state/FIPS identifier: {.val {input}}. Defaulting to '00' (National).")
  "00"
}

#' Convert FIPS or "00" to 2-Letter Abbreviation
#' @noRd
qwi_fips_to_abbr <- function(fips) {
  clean <- toupper(trimws(as.character(fips)))
  if (clean == "00") return("US")

  dict <- qwi_state_lookup()
  rev_dict <- stats::setNames(names(dict), unname(dict))
  rev_dict[[clean]] %||% clean
}

#################################
### Function to Retrieve Data ###
#################################

#' Fetch National QWI Flat File from LEHD
#' @noRd
get_qwi_us_bulk <- function(endpoint = "sa",
                            get_vars = c("Emp", "sex", "agegrp"),
                            years = 2021,
                            industry = NULL,
                            label_data = TRUE,
                            use_cache = TRUE) {

  file_name <- sprintf("qwi_us_%s_f_gn_ns_op_u.csv.gz", endpoint)
  cached_path <- file.path(tempdir(), file_name)

  if (!use_cache || !file.exists(cached_path)) {
    url <- sprintf("https://lehd.ces.census.gov/data/qwi/latest_release/us/%s", file_name)
    cli::cli_alert_info("Reading National QWI series from LEHD repository...")
    utils::download.file(url, destfile = cached_path, mode = "wb", quiet = TRUE)
  }

  df <- readr::read_csv(cached_path, show_col_types = FALSE)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # Harmonize core identifier column names to lowercase
  names(df) <- tolower(names(df))

  # Filter years without data-masking ambiguity
  if (!is.null(years)) {
    target_years <- as.numeric(years)
    df <- df[as.numeric(df$year) %in% target_years, , drop = FALSE]
  }

  # Filter industry without data-masking ambiguity
  if (!is.null(industry)) {
    target_inds <- trimws(as.character(industry))
    df <- df[trimws(as.character(df$industry)) %in% target_inds, , drop = FALSE]
  }

  # Add standardized geographic and time fields
  df$region <- "US"
  df$geography <- "us"
  df$state <- "00"
  if (!"time" %in% names(df) && all(c("year", "quarter") %in% names(df))) {
    df$time <- paste0(df$year, "-Q", df$quarter)
  }

  # Case-insensitive harmonization for requested indicators (e.g., 'emp' -> 'Emp')
  df_names_lower <- tolower(names(df))
  for (v in get_vars) {
    idx <- which(df_names_lower == tolower(v))
    if (length(idx) > 0 && names(df)[idx[1]] != v) {
      names(df)[idx[1]] <- v
    }
  }

  # Apply demographic labels
  qwi_labels <- list(
    sex = c("0" = "All Sexes", "1" = "Male", "2" = "Female"),
    agegrp = c("A00" = "All Ages", "A01" = "14-18", "A02" = "19-21", "A03" = "22-24",
               "A04" = "25-34", "A05" = "35-44", "A06" = "45-54", "A07" = "55-64", "A08" = "65 and older"),
    education = c("E0" = "All Education Categories", "E1" = "Less than high school",
                  "E2" = "High school or equivalent, no college", "E3" = "Some college or Associate degree",
                  "E4" = "Bachelor's degree or advanced degree", "E5" = "Educational attainment not available"),
    race = c("A0" = "All Races", "A1" = "White Alone", "A2" = "Black or African American Alone",
             "A3" = "American Indian or Alaska Native Alone", "A4" = "Asian Alone",
             "A5" = "Native Hawaiian or Other Pacific Islander Alone", "A7" = "Two or More Race Groups"),
    ethnicity = c("A0" = "All Ethnicities", "A1" = "Not Hispanic or Latino", "A2" = "Hispanic or Latino")
  )

  if (label_data) {
    for (var in names(qwi_labels)) {
      if (var %in% names(df)) {
        new_col_name <- paste0(var, "_label")
        matched <- qwi_labels[[var]][as.character(df[[var]])]
        df[[new_col_name]] <- ifelse(is.na(matched), as.character(df[[var]]), unname(matched))
      }
    }
  }

  label_cols <- paste0(names(qwi_labels), "_label")
  keep_cols <- unique(c(
    "region", "state", "geography", "time", "year", "quarter", "industry",
    intersect(names(df), c(get_vars, label_cols))
  ))

  df <- df |> dplyr::select(dplyr::any_of(keep_cols))

  # Explicitly cast metadata columns to character and variables to numeric
  skip_cols <- c(
    names(qwi_labels), label_cols, "time", "year", "quarter",
    "state", "county", "metropolitan statistical area/micropolitan statistical area",
    "workforce investment area", "industry", "region", "geography"
  )

  skip_in_df <- intersect(names(df), skip_cols)
  df[skip_in_df] <- lapply(df[skip_in_df], as.character)

  num_cols <- setdiff(names(df), skip_cols)
  df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(as.character(x))))

  return(df)
}

#' Fetch QWI Data Chunk (Hybrid: API for Sub-National, LEHD Repository for US)
#'
#' @param endpoint Character. Demographic crosstab: `"sa"`, `"se"`, or `"rh"`.
#' @param get_vars Character vector of indicators and demographic variables.
#' @param geography Character. Geographic level: `"us"`, `"state"`, `"county"`, `"cbsa"`, or `"wia"`.
#' @param state_fips Two-digit state FIPS code, 2-letter abbreviation (e.g. `"NV"`), or `"00"`/`"US"`.
#' @param years Numeric vector of years (e.g., `2020:2022`).
#' @param industry Optional NAICS industry code (e.g., `"31-33"`). Default is `NULL`.
#' @param key Census API Key. Defaults to `CENSUS_API_KEY` environment variable.
#' @param label_data Logical. If `TRUE` (default), appends readable demographic labels.
#'
#' @return A data frame containing the requested QWI observations.
#' @export
get_qwi_chunk <- function(endpoint = "sa",
                          get_vars = c("Emp", "sex", "agegrp"),
                          geography = "state",
                          state_fips = "00",
                          years = 2021,
                          industry = NULL,
                          key = Sys.getenv("CENSUS_API_KEY"),
                          label_data = TRUE) {

  norm_state <- qwi_normalize_state(state_fips)

  # Internal branch: if '00' or geography is 'us', resolve via LEHD national bulk stream
  if (norm_state == "00" || geography %in% c("us", "national")) {
    return(get_qwi_us_bulk(
      endpoint = endpoint,
      get_vars = get_vars,
      years = years,
      industry = industry,
      label_data = label_data
    ))
  }

  # Route State / Sub-state queries to Census Data API
  if (geography == "state") {
    geo_for <- paste0("state:", norm_state)
    geo_in <- NULL
  } else if (geography == "county") {
    geo_for <- "county:*"
    geo_in <- paste0("state:", norm_state)
  } else if (geography == "cbsa") {
    geo_for <- "metropolitan statistical area/micropolitan statistical area:*"
    geo_in <- paste0("state:", norm_state)
  } else if (geography == "wia") {
    geo_for <- "workforce investment area:*"
    geo_in <- paste0("state:", norm_state)
  } else {
    cli::cli_abort("Unsupported geography: {.val {geography}}.")
  }

  base_url <- sprintf("https://api.census.gov/data/timeseries/qwi/%s", endpoint)

  qwi_labels <- list(
    sex = c("0" = "All Sexes", "1" = "Male", "2" = "Female"),
    agegrp = c("A00" = "All Ages", "A01" = "14-18", "A02" = "19-21", "A03" = "22-24",
               "A04" = "25-34", "A05" = "35-44", "A06" = "45-54", "A07" = "55-64", "A08" = "65 and older"),
    education = c("E0" = "All Education Categories", "E1" = "Less than high school",
                  "E2" = "High school or equivalent, no college", "E3" = "Some college or Associate degree",
                  "E4" = "Bachelor's degree or advanced degree", "E5" = "Educational attainment not available"),
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
    if (!is.null(geo_in)) query_params$`in` <- geo_in
    if (!is.null(industry)) query_params$industry <- industry

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
      cli::cli_alert_warning("No data found for geography '{geography}' / region '{norm_state}' in {yr}")
      return(data.frame())
    }

    raw_json <- jsonlite::fromJSON(httr2::resp_body_string(resp))
    df <- as.data.frame(raw_json[-1, , drop = FALSE], stringsAsFactors = FALSE)
    names(df) <- raw_json[1, ]

    # Standardize geographic identifier
    df$region <- qwi_fips_to_abbr(norm_state)

    if (label_data) {
      for (var in names(qwi_labels)) {
        if (var %in% names(df)) {
          new_col_name <- paste0(var, "_label")
          matched <- qwi_labels[[var]][df[[var]]]
          df[[new_col_name]] <- ifelse(is.na(matched), df[[var]], unname(matched))
        }
      }
    }

    label_cols <- paste0(names(qwi_labels), "_label")
    skip_cols <- c(
      names(qwi_labels), label_cols, "time", "year", "quarter",
      "state", "county", "metropolitan statistical area/micropolitan statistical area",
      "workforce investment area", "industry", "region"
    )

    skip_in_df <- intersect(names(df), skip_cols)
    df[skip_in_df] <- lapply(df[skip_in_df], as.character)

    num_cols <- setdiff(names(df), skip_cols)
    df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(as.character(x))))

    Sys.sleep(0.5)
    return(df)
  })

  return(results)
}

#################################
### Cross-Sectional Functions ###
#################################

#' Batch Fetch QWI for Multi-Area and Multi-Industry Comparisons
#'
#' Retrieves and stacks Quarterly Workforce Indicators (QWI) data across multiple
#' geographic areas and NAICS industry codes to facilitate cross-sectional analysis.
#' Supports national benchmarks, states, metropolitan areas (CBSA), and counties.
#'
#' @param areas Character vector specifying geographic entities to compare.
#'   Supports national, state, metro/micro area (CBSA), and county levels:
#'   \itemize{
#'     \item National: \code{"US"} or \code{"00"}
#'     \item State: Two-letter postal code or two-digit FIPS (e.g., \code{"NV"}, \code{"CA"}, \code{"32"})
#'     \item Metro/Micro Area (CBSA): \code{"cbsa:STATE:CODE"} (e.g., \code{"cbsa:NV:29820"} for Las Vegas, \code{"cbsa:NV:39900"} for Reno)
#'     \item County: \code{"county:STATE:FIPS"} (e.g., \code{"county:NV:003"} for Clark County, \code{"county:NV:031"} for Washoe County)
#'   }
#' @param industries Character vector of NAICS industry codes (e.g., \code{c("23", "72", "31-33")}).
#'   Supports 2-digit sectors, as well as 3-digit and 4-digit subsectors for state and sub-state
#'   queries (e.g., \code{"621"}, \code{"7225"}). Note: National benchmarks (\code{"US"} / \code{"00"})
#'   are only tabulated at the 2-digit sector level.
#' @param years Numeric vector of years (e.g., \code{2020:2022}).
#' @param indicators Character vector of QWI indicator codes to retrieve. Default is \code{c("Emp", "EarnS")}.
#'   Valid indicator codes include:
#'   \itemize{
#'     \item \code{"Emp"}: Beginning-of-Quarter Employment
#'     \item \code{"EmpEnd"}: End-of-Quarter Employment
#'     \item \code{"EmpS"}: Full-Quarter Employment (Stable)
#'     \item \code{"EmpTotal"}: Employment - Reference Quarter
#'     \item \code{"EmpSpv"}: Full-Quarter Employment in the Previous Quarter
#'     \item \code{"HirA"}: Hires (All Accessions)
#'     \item \code{"HirN"}: New Hires
#'     \item \code{"HirR"}: Recall Hires
#'     \item \code{"Sep"}: Separations (All)
#'     \item \code{"HirAEnd"}: End-of-Quarter Hires
#'     \item \code{"SepBeg"}: Beginning-of-Quarter Separations
#'     \item \code{"HirAEndRepl"}: Replacement Hires
#'     \item \code{"HirAEndR"}: End-of-Quarter Hiring Rate
#'     \item \code{"SepBegR"}: Beginning-of-Quarter Separation Rate
#'     \item \code{"SepS"}: Separations (Flows out of Full-Quarter Employment)
#'     \item \code{"SepSnx"}: Separations in the Next Quarter (Flows out of Full-Quarter Employment)
#'     \item \code{"TurnOvrS"}: Turnover (Stable)
#'     \item \code{"FrmJbGn"}: Firm Job Gains (Job Creation)
#'     \item \code{"FrmJbLs"}: Firm Job Loss (Job Destruction)
#'     \item \code{"FrmJbC"}: Firm Job Change (Net Change)
#'     \item \code{"FrmJbGnS"}: Firm Job Gains (Stable)
#'     \item \code{"FrmJbLsS"}: Firm Job Loss (Stable)
#'     \item \code{"FrmJbCS"}: Firm Job Change (Stable; Net Change)
#'     \item \code{"EarnS"}: Average Monthly Earnings (Full-Quarter Employment)
#'     \item \code{"EarnBeg"}: Average Monthly Earnings (Beginning-of-Quarter Employment)
#'     \item \code{"EarnHirAS"}: Average Monthly Earnings (All Hires into Full-Quarter Employment)
#'     \item \code{"EarnHirNS"}: Average Monthly Earnings (New Hires into Full-Quarter Employment)
#'     \item \code{"EarnSepS"}: Average Monthly Earnings (Flows out of Full-Quarter Employment)
#'     \item \code{"Payroll"}: Total Quarterly Payroll
#'   }
#' @param endpoint Demographic crosstab endpoint. Default is \code{"sa"}.
#'   Supported options include:
#'   \itemize{
#'     \item \code{"sa"}: Sex by Age
#'     \item \code{"se"}: Sex by Education
#'     \item \code{"rh"}: Race by Ethnicity
#'   }
#' @param geo_file Optional local path to \code{label_geography.csv}. If \code{NULL}, fetches and caches from LEHD.
#'
#' @return A stacked, tidy tibble with standardized \code{region}, \code{industry_code},
#'   and \code{industry_name} columns.
#' @export
#' @examples
#' \dontrun{
#' # State vs. National comparison across two sectors
#' df_state_us <- fetch_qwi_comparison(
#'   areas = c("NV", "US"),
#'   industries = c("23", "72"),
#'   years = 2021:2022,
#'   indicators = c("Emp", "EarnS")
#' )
#'
#' # Multi-tier comparison: MSAs, State total, and National baseline
#' df_regional <- fetch_qwi_comparison(
#'   areas = c("cbsa:NV:39900", "cbsa:NV:29820", "NV", "00"),
#'   industries = c("72"),
#'   years = 2022,
#'   indicators = c("Emp", "TurnOvrS")
#' )
#' }
fetch_qwi_comparison <- function(areas = c("cbsa:NV:39900", "cbsa:NV:29820", "NV", "00"),
                                 industries = c("23", "72"),
                                 years = 2020:2022,
                                 indicators = c("Emp", "EarnS"),
                                 endpoint = "sa",
                                 geo_file = NULL) {

  demo_vars <- list(sa = c("sex", "agegrp"), se = c("sex", "education"), rh = c("race", "ethnicity"))[[endpoint]]
  get_vars <- c(indicators, demo_vars)
  ind_dict <- qwi_dict_industries()
  geo_dict <- qwi_dict_geography(file_path = geo_file)

  query_grid <- tidyr::expand_grid(
    area = areas,
    industry = industries
  )

  cli::cli_alert_info("Fetching {nrow(query_grid)} batches across {length(years)} year(s)...")

  purrr::pmap_dfr(query_grid, function(area, industry) {
    area_parts <- strsplit(area, ":")[[1]]

    # Parse geographic hierarchy
    if (length(area_parts) == 3) {
      geo_type   <- tolower(area_parts[1])  # 'cbsa' or 'county'
      state_id   <- area_parts[2]          # 'NV'
      sub_code   <- area_parts[3]          # '29820' or '003'
      state_fips <- qwi_normalize_state(state_id)
    } else {
      norm_val   <- qwi_normalize_state(area)
      geo_type   <- if (norm_val == "00") "us" else "state"
      state_id   <- area
      state_fips <- norm_val
      sub_code   <- NULL
    }

    df <- get_qwi_chunk(
      endpoint = endpoint,
      get_vars = get_vars,
      geography = geo_type,
      state_fips = state_id,
      years = years,
      industry = industry,
      label_data = TRUE
    )

    if (nrow(df) > 0) {
      # Filter to the requested sub-state entity and apply LEHD labels
      if (geo_type == "cbsa" && !is.null(sub_code)) {
        cbsa_col <- "metropolitan statistical area/micropolitan statistical area"
        if (cbsa_col %in% names(df)) {
          df <- df[df[[cbsa_col]] == sprintf("%05d", as.numeric(sub_code)), , drop = FALSE]
          df$region <- qwi_lookup_cbsa(cbsa_code = sub_code, state_fips = state_fips, geo_df = geo_dict)
        }
      } else if (geo_type == "county" && !is.null(sub_code)) {
        if ("county" %in% names(df)) {
          clean_county <- sprintf("%03d", as.numeric(sub_code))
          df <- df[df$county == clean_county, , drop = FALSE]
          df$region <- qwi_lookup_county(county_fips = clean_county, state_fips = state_fips, geo_df = geo_dict)
        }
      }

      # Attach industry labels
      ind_title <- unname(ind_dict[as.character(industry)])
      if (is.na(ind_title) || !nzchar(ind_title)) {
        ind_title <- paste("NAICS", industry)
      }
      df$industry_code <- as.character(industry)
      df$industry_name <- as.character(ind_title)
    }

    df
  })
}

#################################
### Wizard and Script Builder ###
#################################

#' Generate R Script for QWI Extraction
#' @export
generate_qwi_script <- function(endpoint,
                                years = 2022,
                                geography = "county",
                                state_fips = "06",
                                indicators = c("Emp"),
                                industry = NULL,
                                var_name = "qwi_data") {

  norm_state <- qwi_normalize_state(state_fips)

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
    "# Geographic Scope: ", toupper(geography), " (Region: ", norm_state, ")\n",
    "# Endpoint ID: ", endpoint$id, "\n",
    "# Demographic Crosstabs: ", paste(endpoint$vars, collapse = ", "), "\n",
    "# =========================================================================\n\n",
    "library(dplyr)\n\n",
    "if (Sys.getenv(\"CENSUS_API_KEY\") == \"\") {\n",
    "  message(\"Note: CENSUS_API_KEY environment variable is not set. Requests may be rate-limited.\")\n",
    "}\n\n",
    "indicators <- c(", inds_str, ")\n",
    "demo_vars <- c(", demo_vars_str, ")\n",
    "get_vars <- c(indicators, demo_vars)\n\n",
    var_name, " <- get_qwi_chunk(\n",
    "  endpoint = \"", endpoint$id, "\",\n",
    "  get_vars = get_vars,\n",
    "  geography = \"", geography, "\",\n",
    "  state_fips = \"", norm_state, "\",\n",
    "  years = ", years_str, ",\n",
    ind_arg_str,
    "  label_data = TRUE\n",
    ")\n\n",
    indicator_print_block,
    "glimpse(", var_name, ")\n"
  )

  script
}

#' Interactive QWI Script Wizard
#' @export
qwi_wizard <- function(output_dir = NULL, filename = NULL) {
  if (!interactive()) {
    cli::cli_abort("{.fn qwi_wizard} requires an interactive R session.")
  }

  cli::cli_h1("LEHD QWI Script Wizard")
  cli::cli_text("Configure your QWI data query and generate a reproducible R script.")
  cli::cli_rule()

  selected_endpoint <- prompt_qwi_endpoint()
  if (is.null(selected_endpoint)) return(invisible(NULL))

  cli::cli_alert_success("Selected Endpoint: {.strong {selected_endpoint$title}}")
  cli::cli_text("{.field Crosstabs}: {paste(selected_endpoint$vars, collapse = ', ')}")
  cli::cli_rule()

  params <- prompt_qwi_parameters()

  dest <- prompt_qwi_destination(
    endpoint_id = selected_endpoint$id,
    year = min(params$years),
    output_dir = output_dir,
    filename = filename
  )

  script_content <- generate_qwi_script(
    endpoint = selected_endpoint,
    years = params$years,
    geography = params$geography,
    state_fips = params$state_fips,
    indicators = params$indicators,
    industry = params$industry,
    var_name = paste0("qwi_", selected_endpoint$id)
  )

  dir_to_create <- dirname(dest$path)
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }

  writeLines(script_content, con = dest$path)

  cli::cli_rule()
  cli::cli_alert_success("Created R script: {.file {dest$path}}")

  if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
    tryCatch({
      clipr::write_clip(script_content)
      cli::cli_alert_info("Code copied to system clipboard.")
    }, error = function(e) NULL)
  }

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

prompt_qwi_parameters <- function() {
  cli::cli_h2("Step 2: Configure Download Parameters")

  # 1. Geography Scope Selection
  geos <- c(
    "us"     = "US National",
    "state"  = "State",
    "county" = "County",
    "cbsa"   = "Metro/Micro Area",
    "wia"    = "Workforce Investment Area"
  )
  geo_idx <- utils::menu(unname(geos), title = "Select Geography Level:")
  if (geo_idx == 0) stop("Wizard cancelled.", call. = FALSE)
  geography <- names(geos)[geo_idx]

  # 2. State / Region Resolution
  if (geography == "us") {
    state_fips <- "00"
    cli::cli_alert_info("Geographic scope set to national: {.val 00 (US)}")
  } else {
    state_input <- readline(prompt = "Enter State FIPS code or 2-letter postal abbreviation (e.g., '32' or 'NV'): ")
    state_fips <- qwi_normalize_state(state_input)
    cli::cli_alert_success("Resolved state: {.strong {qwi_fips_to_abbr(state_fips)}} (FIPS: {state_fips})")
  }

  # 3. Years
  year_input <- readline(prompt = "Enter year(s) to fetch (e.g., '2022' or '2019:2022'): ")
  years <- tryCatch({
    parsed <- eval(parse(text = year_input))
    if (!is.numeric(parsed)) 2022 else parsed
  }, error = function(e) 2022)

  # 4. Indicators
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


# Internal cache environment for LEHD geography lookup
.qwi_cache <- new.env(parent = emptyenv())

#' Load LEHD Geography Labels Dictionary
#'
#' Reads and caches the official LEHD geography labels table from a local
#' file or the official Census LEHD schema URL.
#'
#' @param file_path Optional path to a local `label_geography.csv` file.
#'   If `NULL`, downloads and caches from the LEHD repository.
#' @return A data frame containing `geography`, `label`, and `geo_level`.
#' @noRd
qwi_dict_geography <- function(file_path = NULL) {
  if (exists("geo_table", envir = .qwi_cache)) {
    return(get("geo_table", envir = .qwi_cache))
  }

  if (!is.null(file_path) && file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  } else {
    url <- "https://lehd.ces.census.gov/data/schema/latest/label_geography.csv"
    cached_file <- file.path(tempdir(), "label_geography.csv")
    if (!file.exists(cached_file)) {
      cli::cli_alert_info("Downloading LEHD geography dictionary...")
      utils::download.file(url, destfile = cached_file, quiet = TRUE)
    }
    df <- readr::read_csv(
      cached_file,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  }

  assign("geo_table", df, envir = .qwi_cache)
  df
}

#' Lookup CBSA / MSA Name from LEHD Table
#' @noRd
qwi_lookup_cbsa <- function(cbsa_code, state_fips = NULL, geo_df = qwi_dict_geography()) {
  clean_cbsa <- sprintf("%05d", as.numeric(trimws(as.character(cbsa_code))))

  # 1. Try 7-digit state-specific portion first (geo_level == 'M')
  if (!is.null(state_fips)) {
    clean_state <- sprintf("%02d", as.numeric(trimws(as.character(state_fips))))
    m_key <- paste0(clean_state, clean_cbsa)
    m_row <- geo_df[geo_df$geo_level == "M" & geo_df$geography == m_key, , drop = FALSE]
    if (nrow(m_row) > 0) return(m_row$label[1])
  }

  # 2. Fall back to 5-digit CBSA title (geo_level == 'B')
  b_row <- geo_df[geo_df$geo_level == "B" & geo_df$geography == clean_cbsa, , drop = FALSE]
  if (nrow(b_row) > 0) return(b_row$label[1])

  paste("CBSA", clean_cbsa)
}

#' Lookup County Name from LEHD Table
#' @noRd
qwi_lookup_county <- function(county_fips, state_fips, geo_df = qwi_dict_geography()) {
  clean_state  <- sprintf("%02d", as.numeric(trimws(as.character(state_fips))))
  clean_county <- sprintf("%03d", as.numeric(trimws(as.character(county_fips))))
  c_key        <- paste0(clean_state, clean_county)

  c_row <- geo_df[geo_df$geo_level == "C" & geo_df$geography == c_key, , drop = FALSE]
  if (nrow(c_row) > 0) return(c_row$label[1])

  paste0(qwi_fips_to_abbr(clean_state), " County ", clean_county)
}
