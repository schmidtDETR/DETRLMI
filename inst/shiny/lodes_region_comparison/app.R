# ==============================================================================
# LODES & ACS Regional Workforce Comparator
#
# Interactive R Shiny application for building custom geographic regions,
# comparing LEHD LODES8 (RAC, WAC, OD) and ACS 5-Year (Table S2301) workforce
# profiles, generating charts, and exporting CSV/PDF reports.
# ==============================================================================

library(shiny)
library(bslib)
library(mapgl)
library(tigris)
library(tidycensus)
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(scales)
library(DT)
library(htmltools)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(jsonlite)

options(tigris_use_cache = TRUE)
options(timeout = 300)
sf::sf_use_s2(FALSE)

# ------------------------------------------------------------------------------
# Cache, LaTeX-Safe Build Directory & Static Dictionaries
# ------------------------------------------------------------------------------
get_data_cache_dir <- function() {
  dir_path <- tools::R_user_dir("lodes_region_compare_cache", which = "cache")
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  dir_path
}
cache_dir <- get_data_cache_dir()

# Returns a temporary build directory free of spaces or Windows '~' short-path
# characters that can cause pdflatex compilation errors.
get_latex_safe_build_dir <- function() {
  candidates <- c(
    normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
    normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  )
  safe_base <- candidates[!grepl("[~ ]", candidates)][1]
  if (is.na(safe_base) || !nzchar(safe_base)) {
    safe_base <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  }
  build_dir <- file.path(safe_base, paste0("lodes_pdf_", as.integer(Sys.time()), "_", sample.int(9999L, 1L)))
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(build_dir, winslash = "/", mustWork = TRUE)
}

region_palette <- c(
  "#2563eb", # Blue
  "#d97706", # Amber
  "#059669", # Emerald
  "#7c3aed", # Violet
  "#db2777", # Pink
  "#0891b2", # Cyan
  "#4f46e5", # Indigo
  "#65a30d"  # Lime
)

state_choices <- stats::setNames(
  c(state.abb, "DC", "PR"),
  c(state.name, "District of Columbia", "Puerto Rico")
)

segment_choices <- c(
  "All Workers"                                    = "S000",
  "Age: 29 or Younger"                             = "SA01",
  "Age: 30 to 54"                                  = "SA02",
  "Age: 55 and Older"                              = "SA03",
  "Earnings: $1,250/month or Less"                 = "SE01",
  "Earnings: $1,251 to $3,333/month"               = "SE02",
  "Earnings: Average (Greater than $3,333/month)"  = "SE03",
  "Industry: Goods Producing"                      = "SI01",
  "Industry: Trade, Transportation & Utilities"    = "SI02",
  "Industry: All Other Services"                   = "SI03"
)

job_type_choices <- c(
  "All Jobs"             = "JT00",
  "Primary Jobs"         = "JT01",
  "All Private Jobs"     = "JT02",
  "Private Primary Jobs" = "JT03",
  "All Federal Jobs"     = "JT04",
  "Federal Primary Jobs" = "JT05"
)

metric_catalog <- tibble::tribble(
  ~category,            ~var,               ~label,
  "Totals",             "total_jobs",       "Total Segment Jobs / Workers",
  "Age",                "age_under_30",     "Under 30",
  "Age",                "age_30_to_54",     "30 to 54",
  "Age",                "age_over_55",      "55 and Older",
  "Earnings",           "earn_low",         "Very Low ($1,250 or less/mo)",
  "Earnings",           "earn_mid",         "Low ($1,250 to $3,333/mo)",
  "Earnings",           "earn_high",        "Average (> $3,333/mo)",
  "Education",          "edu_less_hs",      "Less than High School",
  "Education",          "edu_hs",           "High School or Equivalent",
  "Education",          "edu_some_college", "Some College or Associate",
  "Education",          "edu_bachelors",    "Bachelor's Degree or Higher",
  "Race",               "race_white",       "White Alone",
  "Race",               "race_black",       "Black or African American",
  "Race",               "race_native",      "American Indian or Alaska Native",
  "Race",               "race_asian",       "Asian Alone",
  "Race",               "race_pacific",     "Native Hawaiian or Pacific Islander",
  "Race",               "race_two_plus",    "Two or More Races",
  "Ethnicity",          "eth_hispanic",     "Hispanic or Latino",
  "Ethnicity",          "eth_not_hispanic", "Not Hispanic or Latino",
  "Sex",                "sex_male",         "Male",
  "Sex",                "sex_female",       "Female",
  "Broad Industry",     "ind_goods",        "Goods Producing",
  "Broad Industry",     "ind_trade",        "Trade & Transportation",
  "Broad Industry",     "ind_service",      "Services",
  "2-Digit NAICS",      "cns01",            "NAICS 11: Agriculture, Forestry, Fishing",
  "2-Digit NAICS",      "cns02",            "NAICS 21: Mining, Oil & Gas Extraction",
  "2-Digit NAICS",      "cns03",            "NAICS 22: Utilities",
  "2-Digit NAICS",      "cns04",            "NAICS 23: Construction",
  "2-Digit NAICS",      "cns05",            "NAICS 31-33: Manufacturing",
  "2-Digit NAICS",      "cns06",            "NAICS 42: Wholesale Trade",
  "2-Digit NAICS",      "cns07",            "NAICS 44-45: Retail Trade",
  "2-Digit NAICS",      "cns08",            "NAICS 48-49: Transportation & Warehousing",
  "2-Digit NAICS",      "cns09",            "NAICS 51: Information",
  "2-Digit NAICS",      "cns10",            "NAICS 52: Finance & Insurance",
  "2-Digit NAICS",      "cns11",            "NAICS 53: Real Estate & Rental/Leasing",
  "2-Digit NAICS",      "cns12",            "NAICS 54: Professional, Scientific & Tech",
  "2-Digit NAICS",      "cns13",            "NAICS 55: Management of Companies",
  "2-Digit NAICS",      "cns14",            "NAICS 56: Administrative & Waste Mgmt",
  "2-Digit NAICS",      "cns15",            "NAICS 61: Educational Services",
  "2-Digit NAICS",      "cns16",            "NAICS 62: Health Care & Social Assistance",
  "2-Digit NAICS",      "cns17",            "NAICS 71: Arts, Entertainment & Recreation",
  "2-Digit NAICS",      "cns18",            "NAICS 72: Accommodation & Food Services",
  "2-Digit NAICS",      "cns19",            "NAICS 81: Other Services (ex. Public Admin)",
  "2-Digit NAICS",      "cns20",            "NAICS 92: Public Administration"
)

# ------------------------------------------------------------------------------
# Spatial, ACS (tidycensus) & LODES Helpers
# ------------------------------------------------------------------------------
dissolve_quietly <- function(sf_obj) {
  base::suppressMessages(base::suppressWarnings(
    sf_obj |>
      sf::st_transform(5070) |>
      sf::st_make_valid() |>
      sf::st_buffer(0) |>
      sf::st_union() |>
      sf::st_transform(4326)
  ))
}

difference_quietly <- function(geom_a, geom_b) {
  base::suppressMessages(base::suppressWarnings(
    sf::st_difference(
      sf::st_transform(geom_a, 5070),
      sf::st_transform(geom_b, 5070)
    ) |>
      sf::st_transform(4326)
  ))
}

union_quietly <- function(geom_a, geom_b) {
  base::suppressMessages(base::suppressWarnings(
    sf::st_union(
      sf::st_transform(geom_a, 5070),
      sf::st_transform(geom_b, 5070)
    ) |>
      sf::st_transform(4326)
  ))
}

fetch_geo_robustly <- function(geom_type, state_id) {
  call_tigris <- function(use_cb, target_year = NULL) {
    args <- list(cb = use_cb, progress_bar = FALSE)
    if (!is.null(target_year)) args$year <- target_year
    if (geom_type == "ZCTAs" && is.null(target_year)) args$year <- 2020

    state_level_geoms <- c(
      "Counties", "Tracts", "Block Groups", "PUMAs",
      "County Subdivisions", "Census Places", "School Districts",
      "Lower State Legislative Districts", "Upper State Legislative Districts",
      "Voting Districts"
    )
    if (geom_type %in% state_level_geoms) args$state <- state_id
    if (geom_type == "Lower State Legislative Districts") args$house <- "lower"
    if (geom_type == "Upper State Legislative Districts") args$house <- "upper"

    raw_shape <- switch(
      geom_type,
      "Full State" = {
        st_shp <- base::do.call(tigris::states, args)
        st_shp[st_shp$STUSPS == state_id, ]
      },
      "Counties"                          = base::do.call(tigris::counties, args),
      "Tracts"                            = base::do.call(tigris::tracts, args),
      "Block Groups"                      = base::do.call(tigris::block_groups, args),
      "PUMAs"                             = base::do.call(tigris::pumas, args),
      "ZCTAs"                             = base::do.call(tigris::zctas, args),
      "Core-based Areas"                  = base::do.call(tigris::core_based_statistical_areas, args),
      "County Subdivisions"               = base::do.call(tigris::county_subdivisions, args),
      "Census Places"                     = base::do.call(tigris::places, args),
      "School Districts"                  = base::do.call(tigris::school_districts, args),
      "Lower State Legislative Districts" = base::do.call(tigris::state_legislative_districts, args),
      "Upper State Legislative Districts" = base::do.call(tigris::state_legislative_districts, args),
      "Urban Areas"                       = base::do.call(tigris::urban_areas, args),
      "Voting Districts"                  = base::do.call(tigris::voting_districts, args)
    )

    national_geoms <- c("ZCTAs", "Core-based Areas", "Urban Areas")
    if (geom_type %in% national_geoms) {
      state_args <- list(cb = use_cb, progress_bar = FALSE)
      if (!is.null(target_year)) state_args$year <- target_year
      state_boundary <- base::do.call(tigris::states, state_args)
      state_boundary <- state_boundary[state_boundary$STUSPS == state_id, ]
      state_boundary <- sf::st_transform(state_boundary, sf::st_crs(raw_shape))

      intersects <- base::suppressMessages(sf::st_intersects(raw_shape, state_boundary, sparse = FALSE)[, 1])
      touches    <- base::suppressMessages(sf::st_touches(raw_shape, state_boundary, sparse = FALSE)[, 1])
      raw_shape  <- raw_shape[intersects & !touches, ]
    }
    raw_shape
  }

  base::tryCatch(call_tigris(TRUE), error = function(e1) {
    base::tryCatch(call_tigris(TRUE, 2020), error = function(e2) {
      base::tryCatch(call_tigris(FALSE), error = function(e3) {
        call_tigris(FALSE, 2020)
      })
    })
  })
}

build_standardized_layer <- function(geom_type, state_id) {
  raw_data <- fetch_geo_robustly(geom_type, state_id)
  possible_ids <- c("GEOID", "GEOID20", "GEOID10", "ZCTA5CE20", "ZCTA5CE10")
  id_col <- base::intersect(possible_ids, names(raw_data))[1]
  if (is.na(id_col)) id_col <- grep("^GEOID|^ZCTA", names(raw_data), value = TRUE)[1]

  name_col <- base::intersect(c("NAMELSAD", "NAME", "NAMELSAD20", "NAME20"), names(raw_data))[1]

  raw_data |>
    dplyr::mutate(
      MAP_GEOID  = as.character(.data[[id_col]]),
      AREA_LABEL = if (!is.na(name_col)) paste0(.data[[name_col]], " (", MAP_GEOID, ")") else MAP_GEOID,
      UNIQUE_KEY = paste(state_id, geom_type, MAP_GEOID, sep = "::"),
      SRC_STATE  = as.character(state_id),
      SRC_GEOM   = as.character(geom_type)
    ) |>
    dplyr::select(UNIQUE_KEY, MAP_GEOID, AREA_LABEL, SRC_STATE, SRC_GEOM) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
}

# Fetch full ACS 5-Year Subject Table S2301 (Headline + All Demographic Subsets)
fetch_acs_s2301_regions <- function(regs, regions_poly, reg_meta, acs_year, exclude_overlap) {
  yr_int <- as.integer(acs_year)

  # Load S2301 variable metadata dynamically so all demographic rows (001..035) are labeled
  s2301_meta <- tidycensus::load_variables(yr_int, "acs5/subject", cache = TRUE) |>
    dplyr::filter(grepl("^S2301_C01_", name)) |>
    dplyr::mutate(
      row_id    = sub("^S2301_C01_", "", name),
      clean_lbl = sub("^Estimate!!Total!!", "", label)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      parts          = list(strsplit(clean_lbl, "!!", fixed = TRUE)[[1]]),
      Category       = dplyr::case_when(
        length(parts) == 1 ~ "Headline Total (16+)",
        length(parts) == 2 ~ "Working Age Summary",
        TRUE               ~ tools::toTitleCase(tolower(parts[2]))
      ),
      Characteristic = dplyr::case_when(
        length(parts) <= 2 ~ parts[length(parts)],
        TRUE               ~ paste(parts[3:length(parts)], collapse = " - ")
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(row_id, Category, Characteristic)

  all_states <- unique(unlist(lapply(regs, function(r) r$sf$SRC_STATE)))
  all_geoms  <- unique(unlist(lapply(regs, function(r) r$sf$SRC_GEOM)))

  use_direct_admin <- (!exclude_overlap) && all(all_geoms %in% c("Full State", "Counties"))

  if (use_direct_admin) {
    state_acs <- NULL
    county_acs <- NULL

    if ("Full State" %in% all_geoms) {
      state_acs <- base::suppressMessages(
        tidycensus::get_acs(
          geography   = "state",
          table       = "S2301",
          year        = yr_int,
          survey      = "acs5",
          output      = "wide",
          cache_table = TRUE
        )
      ) |>
        dplyr::mutate(MAP_GEOID = as.character(GEOID), SRC_GEOM = "Full State")
    }

    if ("Counties" %in% all_geoms) {
      cty_states <- unique(unlist(lapply(regs, function(r) {
        r$sf$SRC_STATE[r$sf$SRC_GEOM == "Counties"]
      })))
      county_acs <- base::suppressMessages(
        tidycensus::get_acs(
          geography   = "county",
          state       = cty_states,
          table       = "S2301",
          year        = yr_int,
          survey      = "acs5",
          output      = "wide",
          cache_table = TRUE
        )
      ) |>
        dplyr::mutate(MAP_GEOID = as.character(GEOID), SRC_GEOM = "Counties")
    }

    admin_lookup <- dplyr::bind_rows(state_acs, county_acs)

    res_rows <- lapply(seq_along(regs), function(idx) {
      r <- regs[[idx]]
      rname <- if (nzchar(trimws(r$name))) trimws(r$name) else r$id
      sub_areas <- r$sf |>
        sf::st_drop_geometry() |>
        dplyr::select(MAP_GEOID, SRC_GEOM) |>
        dplyr::left_join(admin_lookup, by = c("MAP_GEOID", "SRC_GEOM"))

      is_single <- (nrow(sub_areas) == 1)

      lapply(seq_len(nrow(s2301_meta)), function(m_idx) {
        rid_code <- s2301_meta$row_id[m_idx]
        c_pop    <- paste0("S2301_C01_", rid_code, "E")
        c_pop_m  <- paste0("S2301_C01_", rid_code, "M")
        c_lfpr   <- paste0("S2301_C02_", rid_code, "E")
        c_lfpr_m <- paste0("S2301_C02_", rid_code, "M")
        c_emp    <- paste0("S2301_C03_", rid_code, "E")
        c_emp_m  <- paste0("S2301_C03_", rid_code, "M")
        c_unemp  <- paste0("S2301_C04_", rid_code, "E")
        c_unemp_m <- paste0("S2301_C04_", rid_code, "M")

        if (!all(c(c_pop, c_lfpr, c_emp, c_unemp) %in% names(sub_areas))) return(NULL)

        pop_v   <- as.numeric(sub_areas[[c_pop]])
        lfpr_v  <- as.numeric(sub_areas[[c_lfpr]])
        emp_v   <- as.numeric(sub_areas[[c_emp]])
        unemp_v <- as.numeric(sub_areas[[c_unemp]])

        tot_pop   <- sum(pop_v, na.rm = TRUE)
        est_lf    <- sum(pop_v * (lfpr_v / 100), na.rm = TRUE)
        est_emp   <- sum(pop_v * (emp_v / 100), na.rm = TRUE)
        est_unemp <- sum((pop_v * (lfpr_v / 100)) * (unemp_v / 100), na.rm = TRUE)

        tibble::tibble(
          region_id          = r$id,
          region_name        = rname,
          region_color       = r$color,
          row_id             = rid_code,
          Category           = s2301_meta$Category[m_idx],
          Characteristic     = s2301_meta$Characteristic[m_idx],
          population         = round(tot_pop),
          population_moe     = if (is_single && c_pop_m %in% names(sub_areas)) round(as.numeric(sub_areas[[c_pop_m]][1])) else NA_real_,
          est_labor_force    = round(est_lf),
          lfpr_pct           = if (tot_pop > 0) (est_lf / tot_pop) * 100 else NA_real_,
          lfpr_moe           = if (is_single && c_lfpr_m %in% names(sub_areas)) as.numeric(sub_areas[[c_lfpr_m]][1]) else NA_real_,
          emp_pop_ratio_pct  = if (tot_pop > 0) (est_emp / tot_pop) * 100 else NA_real_,
          emp_pop_ratio_moe  = if (is_single && c_emp_m %in% names(sub_areas)) as.numeric(sub_areas[[c_emp_m]][1]) else NA_real_,
          unemp_rate_pct     = if (est_lf > 0) (est_unemp / est_lf) * 100 else NA_real_,
          unemp_rate_moe     = if (is_single && c_unemp_m %in% names(sub_areas)) as.numeric(sub_areas[[c_unemp_m]][1]) else NA_real_,
          n_units            = nrow(sub_areas),
          aggregation_method = if (is_single) "Direct ACS S2301" else "Pop-Weighted Multi-Area"
        )
      }) |> dplyr::bind_rows()
    }) |> dplyr::bind_rows()

    return(res_rows)
  }

  # Sub-county / mixed geographies / overlap-excluded regions: aggregate from Census tracts
  tract_acs_raw <- base::suppressMessages(
    tidycensus::get_acs(
      geography   = "tract",
      state       = all_states,
      table       = "S2301",
      year        = yr_int,
      survey      = "acs5",
      output      = "wide",
      cache_table = TRUE
    )
  ) |>
    dplyr::mutate(GEOID = as.character(GEOID))

  tract_shapes <- lapply(all_states, function(st) {
    tr <- fetch_geo_robustly("Tracts", st)
    id_col <- base::intersect(c("GEOID", "GEOID20", "GEOID10"), names(tr))[1]
    tr |>
      dplyr::transmute(GEOID = as.character(.data[[id_col]])) |>
      sf::st_transform(4326) |>
      sf::st_make_valid()
  }) |> dplyr::bind_rows()

  tract_pts <- base::suppressMessages(base::suppressWarnings(
    tract_shapes |>
      sf::st_transform(5070) |>
      sf::st_point_on_surface() |>
      sf::st_transform(4326)
  ))

  joined_tracts <- base::suppressMessages(
    sf::st_join(tract_pts, regions_poly, join = sf::st_within, left = FALSE)
  ) |>
    sf::st_drop_geometry() |>
    dplyr::select(GEOID, region_id) |>
    dplyr::left_join(tract_acs_raw, by = "GEOID")

  lapply(seq_len(nrow(reg_meta)), function(r_idx) {
    rid   <- reg_meta$region_id[r_idx]
    rname <- reg_meta$region_name[r_idx]
    rcol  <- reg_meta$region_color[r_idx]
    sub_t <- joined_tracts |> dplyr::filter(region_id == rid)

    lapply(seq_len(nrow(s2301_meta)), function(m_idx) {
      rid_code <- s2301_meta$row_id[m_idx]
      c_pop    <- paste0("S2301_C01_", rid_code, "E")
      c_lfpr   <- paste0("S2301_C02_", rid_code, "E")
      c_emp    <- paste0("S2301_C03_", rid_code, "E")
      c_unemp  <- paste0("S2301_C04_", rid_code, "E")

      if (!all(c(c_pop, c_lfpr, c_emp, c_unemp) %in% names(sub_t))) return(NULL)

      pop_v   <- as.numeric(sub_t[[c_pop]])
      lfpr_v  <- as.numeric(sub_t[[c_lfpr]])
      emp_v   <- as.numeric(sub_t[[c_emp]])
      unemp_v <- as.numeric(sub_t[[c_unemp]])

      tot_pop   <- sum(pop_v, na.rm = TRUE)
      est_lf    <- sum(pop_v * (lfpr_v / 100), na.rm = TRUE)
      est_emp   <- sum(pop_v * (emp_v / 100), na.rm = TRUE)
      est_unemp <- sum((pop_v * (lfpr_v / 100)) * (unemp_v / 100), na.rm = TRUE)

      tibble::tibble(
        region_id          = rid,
        region_name        = rname,
        region_color       = rcol,
        row_id             = rid_code,
        Category           = s2301_meta$Category[m_idx],
        Characteristic     = s2301_meta$Characteristic[m_idx],
        population         = round(tot_pop),
        population_moe     = NA_real_,
        est_labor_force    = round(est_lf),
        lfpr_pct           = if (tot_pop > 0) (est_lf / tot_pop) * 100 else NA_real_,
        lfpr_moe           = NA_real_,
        emp_pop_ratio_pct  = if (tot_pop > 0) (est_emp / tot_pop) * 100 else NA_real_,
        emp_pop_ratio_moe  = NA_real_,
        unemp_rate_pct     = if (est_lf > 0) (est_unemp / est_lf) * 100 else NA_real_,
        unemp_rate_moe     = NA_real_,
        n_units            = nrow(sub_t),
        aggregation_method = "Tract-Aggregated S2301"
      )
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()
}

summarize_lodes_blocks <- function(df, geocode_col) {
  df |>
    dplyr::rename(tabblk2020 = !!dplyr::sym(geocode_col)) |>
    dplyr::group_by(tabblk2020) |>
    dplyr::summarise(
      total_jobs        = sum(C000, na.rm = TRUE),
      age_under_30      = sum(CA01, na.rm = TRUE),
      age_30_to_54      = sum(CA02, na.rm = TRUE),
      age_over_55       = sum(CA03, na.rm = TRUE),
      earn_low          = sum(CE01, na.rm = TRUE),
      earn_mid          = sum(CE02, na.rm = TRUE),
      earn_high         = sum(CE03, na.rm = TRUE),
      ind_goods         = sum(CNS01 + CNS02 + CNS03 + CNS04 + CNS05, na.rm = TRUE),
      ind_trade         = sum(CNS06 + CNS07 + CNS08, na.rm = TRUE),
      ind_service       = sum(CNS09 + CNS10 + CNS11 + CNS12 + CNS13 + CNS14 +
                                CNS15 + CNS16 + CNS17 + CNS18 + CNS19 + CNS20, na.rm = TRUE),
      cns01             = sum(CNS01, na.rm = TRUE),
      cns02             = sum(CNS02, na.rm = TRUE),
      cns03             = sum(CNS03, na.rm = TRUE),
      cns04             = sum(CNS04, na.rm = TRUE),
      cns05             = sum(CNS05, na.rm = TRUE),
      cns06             = sum(CNS06, na.rm = TRUE),
      cns07             = sum(CNS07, na.rm = TRUE),
      cns08             = sum(CNS08, na.rm = TRUE),
      cns09             = sum(CNS09, na.rm = TRUE),
      cns10             = sum(CNS10, na.rm = TRUE),
      cns11             = sum(CNS11, na.rm = TRUE),
      cns12             = sum(CNS12, na.rm = TRUE),
      cns13             = sum(CNS13, na.rm = TRUE),
      cns14             = sum(CNS14, na.rm = TRUE),
      cns15             = sum(CNS15, na.rm = TRUE),
      cns16             = sum(CNS16, na.rm = TRUE),
      cns17             = sum(CNS17, na.rm = TRUE),
      cns18             = sum(CNS18, na.rm = TRUE),
      cns19             = sum(CNS19, na.rm = TRUE),
      cns20             = sum(CNS20, na.rm = TRUE),
      race_white        = sum(CR01, na.rm = TRUE),
      race_black        = sum(CR02, na.rm = TRUE),
      race_native       = sum(CR03, na.rm = TRUE),
      race_asian        = sum(CR04, na.rm = TRUE),
      race_pacific      = sum(CR05, na.rm = TRUE),
      race_two_plus     = sum(CR07, na.rm = TRUE),
      eth_not_hispanic  = sum(CT01, na.rm = TRUE),
      eth_hispanic      = sum(CT02, na.rm = TRUE),
      edu_less_hs       = sum(CD01, na.rm = TRUE),
      edu_hs            = sum(CD02, na.rm = TRUE),
      edu_some_college  = sum(CD03, na.rm = TRUE),
      edu_bachelors     = sum(CD04, na.rm = TRUE),
      sex_male          = sum(CS01, na.rm = TRUE),
      sex_female        = sum(CS02, na.rm = TRUE),
      .groups = "drop"
    )
}

build_region_popup <- function(reg_name, wac_row, rac_row, segment_label) {
  fmt_pair <- function(row, col) {
    val <- row[[col]]
    tot <- row[["total_jobs"]]
    pct <- if (!is.null(tot) && tot > 0) scales::percent(val / tot, accuracy = 0.1) else "0.0%"
    paste0(scales::comma(val), " <i>(", pct, ")</i>")
  }

  fmt_ratio <- function(col) {
    w_val <- wac_row[[col]]
    r_val <- rac_row[[col]]
    if (!is.null(r_val) && length(r_val) > 0 && !is.na(r_val) && r_val > 0) {
      sprintf("%.2f", w_val / r_val)
    } else {
      "—"
    }
  }

  overall_ratio <- fmt_ratio("total_jobs")

  paste0(
    "<div style='font-family: system-ui, -apple-system, sans-serif; min-width: 630px; max-height: 560px; overflow-y: auto; line-height: 1.4;'>",
    "  <div style='font-size: 1.35em; font-weight: 700; color: #1e3a8a;'>", reg_name, "</div>",
    "  <div style='font-size: 0.95em; color: #4b5563; margin-bottom: 6px;'><b>Segment:</b> ", segment_label, "</div>",
    "  <div style='display: flex; gap: 12px; background: #f8fafc; padding: 8px 10px; border-radius: 6px; margin-bottom: 8px;'>",
    "    <div style='flex: 1;'><b>Workplace Jobs (WAC):</b> <span style='color:#0284c7; font-weight:700;'>", scales::comma(wac_row$total_jobs), "</span></div>",
    "    <div style='flex: 1;'><b>Resident Workers (RAC):</b> <span style='color:#059669; font-weight:700;'>", scales::comma(rac_row$total_jobs), "</span></div>",
    "    <div style='flex: 0.9; text-align: right;'><b>Jobs / Workers:</b> <span style='color:#1e3a8a; font-weight:700;'>", overall_ratio, "</span></div>",
    "  </div>",
    "  <table style='width:100%; border-collapse: collapse; font-size: 0.9em;'>",
    "    <thead>",
    "      <tr style='border-bottom: 2px solid #cbd5e1;'>",
    "        <th style='text-align:left; padding: 4px 4px;'>Metric</th>",
    "        <th style='text-align:right; padding: 4px 6px;'>Workplace (WAC)</th>",
    "        <th style='text-align:right; padding: 4px 6px;'>Residence (RAC)</th>",
    "        <th style='text-align:right; padding: 4px 4px; color:#1e3a8a;'>Jobs / Workers</th>",
    "      </tr>",
    "    </thead>",
    "    <tbody>",
    "      <tr><td style='padding: 2px 4px;'><b>Age: Under 30</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "age_under_30"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "age_under_30"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("age_under_30"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Age: 30 to 54</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "age_30_to_54"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "age_30_to_54"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("age_30_to_54"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Age: 55+</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "age_over_55"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "age_over_55"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("age_over_55"), "</td></tr>",
    "      <tr style='border-top:1px solid #e2e8f0;'><td style='padding: 2px 4px;'><b>Earnings: &le; $1,250/mo</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "earn_low"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "earn_low"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("earn_low"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Earnings: $1,250-$3,333/mo</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "earn_mid"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "earn_mid"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("earn_mid"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Earnings: &gt; $3,333/mo</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "earn_high"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "earn_high"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("earn_high"), "</td></tr>",
    "      <tr style='border-top:1px solid #e2e8f0;'><td style='padding: 2px 4px;'><b>Edu: Less than HS</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "edu_less_hs"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "edu_less_hs"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("edu_less_hs"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Edu: High School</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "edu_hs"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "edu_hs"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("edu_hs"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Edu: Some College</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "edu_some_college"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "edu_some_college"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("edu_some_college"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Edu: Bachelor's+</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "edu_bachelors"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "edu_bachelors"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("edu_bachelors"), "</td></tr>",
    "      <tr style='border-top:1px solid #e2e8f0;'><td style='padding: 2px 4px;'><b>Industry: Goods</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "ind_goods"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "ind_goods"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("ind_goods"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Industry: Trade/Transp.</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "ind_trade"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "ind_trade"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("ind_trade"), "</td></tr>",
    "      <tr><td style='padding: 2px 4px;'><b>Industry: Services</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "ind_service"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "ind_service"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("ind_service"), "</td></tr>",
    "      <tr style='border-top:1px solid #e2e8f0;'><td style='padding: 2px 4px;'><b>Sex: Male / Female</b></td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(wac_row, "sex_male"), " / ", fmt_pair(wac_row, "sex_female"), "</td><td style='text-align:right; padding: 2px 6px;'>", fmt_pair(rac_row, "sex_male"), " / ", fmt_pair(rac_row, "sex_female"), "</td><td style='text-align:right; padding: 2px 4px; font-weight:600; color:#1e3a8a;'>", fmt_ratio("sex_male"), " / ", fmt_ratio("sex_female"), "</td></tr>",
    "    </tbody>",
    "  </table>",
    "</div>"
  )
}

clean_pdf_label <- function(raw_lbl) {
  lbl <- sub("^[^:]+:\\s*", "", raw_lbl)
  lbl <- sub("\\s*\\([^)]+\\)$", "", lbl)
  lbl <- sub(" or Equivalent$", "", lbl)
  lbl <- sub(" or Associate$", "/Assoc.", lbl)
  lbl <- sub(" Degree or Higher$", "+", lbl)
  lbl <- sub(" or Latino$", "", lbl)
  lbl <- sub(" Alone$", "", lbl)
  lbl
}

summarize_top_groups <- function(totals_df, meta) {
  demo_cats <- c("Age", "Earnings", "Education", "Race", "Ethnicity", "Sex", "Broad Industry")

  lapply(seq_len(nrow(meta)), function(i) {
    rid   <- meta$region_id[i]
    rname <- meta$region_name[i]
    row   <- totals_df |> dplyr::filter(region_id == rid)
    tot   <- row$total_jobs[1]

    demo_rows <- lapply(demo_cats, function(cat_name) {
      sub_cat <- metric_catalog |> dplyr::filter(category == cat_name)
      vals <- vapply(sub_cat$var, function(v) row[[v]][1], numeric(1))

      if (cat_name == "Ethnicity") {
        sel_idx <- which(sub_cat$var == "eth_hispanic")
      } else {
        pos_idx <- which(vals > 0)
        if (length(pos_idx) == 0) {
          return(tibble::tibble(Region = rname, Category = cat_name, Highlight = "—"))
        }
        ord <- pos_idx[order(vals[pos_idx], decreasing = TRUE)]
        if (cat_name %in% c("Age", "Education", "Race")) {
          sel_idx <- ord[1:min(2, length(ord))]
        } else if (cat_name %in% c("Earnings", "Broad Industry", "Sex")) {
          sel_idx <- ord
        } else {
          sel_idx <- ord[1]
        }
      }

      item_strs <- vapply(sel_idx, function(idx) {
        val <- vals[idx]
        lbl <- clean_pdf_label(sub_cat$label[idx])
        pct <- if (tot > 0) scales::percent(val / tot, accuracy = 0.1) else "0.0%"
        paste0(lbl, " (", scales::comma(val), " | ", pct, ")")
      }, character(1))

      tibble::tibble(
        Region    = rname,
        Category  = cat_name,
        Highlight = paste(item_strs, collapse = "LINEBREAKHERE")
      )
    }) |> dplyr::bind_rows()

    naics_cat <- metric_catalog |> dplyr::filter(category == "2-Digit NAICS")
    n_vals <- vapply(naics_cat$var, function(v) row[[v]][1], numeric(1))
    pos_naics <- which(n_vals > 0)

    if (length(pos_naics) == 0) {
      naics_row <- tibble::tibble(Region = rname, Category = "Top NAICS Codes", Highlight = "—")
    } else {
      top3_idx <- pos_naics[order(n_vals[pos_naics], decreasing = TRUE)][1:min(3, length(pos_naics))]
      top3_strs <- vapply(top3_idx, function(idx) {
        val <- n_vals[idx]
        pct <- if (tot > 0) scales::percent(val / tot, accuracy = 0.1) else "0.0%"
        naics_code_only <- sub(":.*$", "", naics_cat$label[idx])
        paste0(naics_code_only, " (", scales::comma(val), " | ", pct, ")")
      }, character(1))

      naics_row <- tibble::tibble(
        Region    = rname,
        Category  = "Top NAICS Codes",
        Highlight = paste(top3_strs, collapse = "LINEBREAKHERE")
      )
    }

    dplyr::bind_rows(demo_rows, naics_row)
  }) |> dplyr::bind_rows()
}

# ------------------------------------------------------------------------------
# User Interface
# ------------------------------------------------------------------------------
ui <- bslib::page_sidebar(
  title = "LODES & ACS Regional Workforce Comparator",
  sidebar = bslib::sidebar(
    width = "410px",
    padding = c("10px", "14px"),
    gap = "8px",

    shiny::tags$head(shiny::tags$style(shiny::HTML("
      .maplibregl-popup { max-width: none !important; z-index: 1000 !important; }
      .maplibregl-popup-content {
        padding: 12px 16px !important;
        border-radius: 8px !important;
        box-shadow: 0 4px 16px rgba(0,0,0,0.2) !important;
      }
      .bslib-nav-item .tab-content, .tab-pane { height: 100% !important; }
      .region-card {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 6px;
        padding: 6px 8px;
        margin-bottom: 6px;
      }
      .compact-file-input .form-group { margin-bottom: 0 !important; }
      .compact-file-input .shiny-input-container { margin-bottom: 0 !important; }
      .chart-controls-bar .form-group { margin-bottom: 0 !important; }
      .chart-controls-bar .shiny-input-container { margin-bottom: 0 !important; }
      table.dataTable thead th.group-header-counts {
        background-color: #eff6ff !important;
        color: #1e3a8a !important;
        text-align: center !important;
        border-bottom: 2px solid #93c5fd !important;
      }
      table.dataTable thead th.group-header-shares {
        background-color: #ecfdf5 !important;
        color: #065f46 !important;
        text-align: center !important;
        border-bottom: 2px solid #6ee7b7 !important;
      }
      table.dataTable th.section-divider-left,
      table.dataTable td.section-divider-left {
        border-left: 2px solid #94a3b8 !important;
      }
      table.dataTable tbody td.share-col-tint {
        background-color: rgba(236, 253, 245, 0.45);
      }
      table.dataTable tbody tr:nth-child(odd) td.share-col-tint {
        background-color: rgba(209, 250, 229, 0.35);
      }
      table.dataTable thead th.share-col-tint {
        background-color: #f0fdf4 !important;
      }
      table.dataTable thead th.dt-right,
      table.dataTable tbody td.dt-right {
        text-align: right !important;
      }
    "))),

    # Step 1: Map Geography Selection
    shiny::h5("1. Select Areas on Map", style = "color: #1e3a8a; font-weight: 700; margin-bottom: 2px;"),
    shiny::p(
      class = "text-muted",
      style = "font-size: 0.82em; margin-bottom: 4px;",
      "Click polygons or use the polygon draw tool (top-right). Switch states or geographies anytime; staged selections persist until assigned."
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),
      shiny::selectInput("state", "State:", choices = state_choices, selected = "NV"),
      shiny::selectInput("geometry", "Geography:", choices = c(
        "Full State", "Counties", "Tracts", "Block Groups", "PUMAs", "ZCTAs",
        "Core-based Areas", "County Subdivisions", "Census Places",
        "School Districts", "Lower State Legislative Districts",
        "Upper State Legislative Districts", "Urban Areas", "Voting Districts"
      ), selected = "Counties")
    ),

    shiny::div(
      style = "background: #f1f5f9; padding: 8px; border-radius: 6px; font-size: 0.88em;",
      shiny::div(style = "font-weight: 600; margin-bottom: 4px;", shiny::textOutput("staged_summary")),
      shiny::div(
        style = "display: flex; gap: 6px; align-items: center;",
        shiny::div(
          style = "flex: 1;",
          shiny::selectInput("target_region", label = NULL, choices = c("Custom Region 1" = "R1", "Custom Region 2" = "R2"), width = "100%")
        ),
        shiny::actionButton("assign_staged", "Add Areas", class = "btn-primary btn-sm"),
        shiny::actionButton("clear_staged", "Clear", class = "btn-outline-secondary btn-sm")
      )
    ),

    shiny::hr(style = "margin: 6px 0;"),

    # Step 2: Manage Custom Regions & JSON Definitions
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; gap: 4px; flex-wrap: wrap;",
      shiny::h5("2. Custom Regions", style = "color: #1e3a8a; font-weight: 700; margin: 0;"),
      shiny::div(
        style = "display: flex; gap: 4px;",
        shiny::actionButton("update_names_btn", "Update Names", class = "btn-outline-secondary btn-sm", style = "padding: 2px 6px; font-size: 0.8em;"),
        shiny::actionButton("add_region_btn", "+ Add", class = "btn-outline-primary btn-sm", style = "padding: 2px 6px; font-size: 0.8em;"),
        shiny::actionButton("reset_all_regions_btn", "Reset All", class = "btn-outline-danger btn-sm", style = "padding: 2px 6px; font-size: 0.8em;", title = "Remove all regions, names, and selections and start over")
      )
    ),

    shiny::div(
      style = "background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 6px; padding: 6px 8px; font-size: 0.82em;",
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center; gap: 6px;",
        shiny::span(style = "font-weight: 600; color: #334155;", "Region Definitions:"),
        shiny::downloadButton("export_regions_btn", "Export JSON", class = "btn-outline-secondary btn-sm", style = "padding: 2px 8px; font-size: 0.9em;")
      ),
      shiny::div(
        class = "compact-file-input",
        style = "margin-top: 5px;",
        shiny::fileInput(
          "import_regions_file",
          label = NULL,
          accept = c(".json", "application/json"),
          buttonLabel = "Import JSON...",
          placeholder = "Load saved regions file",
          width = "100%"
        )
      )
    ),

    shiny::div(
      style = "max-height: 210px; overflow-y: auto; padding-right: 2px;",
      shiny::uiOutput("dynamic_regions_ui")
    ),

    shiny::checkboxInput(
      "exclude_higher_overlap",
      "Exclude higher-ranked regions' overlap from lower regions (e.g., R1 from R2, R1–R2 from R3)",
      value = FALSE
    ),

    shiny::hr(style = "margin: 6px 0;"),

    # Step 3: Data Parameters & Run
    shiny::h5("3. Data Parameters & Compare", style = "color: #1e3a8a; font-weight: 700; margin-bottom: 2px;"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::selectInput("year", "LODES Year:", choices = seq(2024, 2002), selected = 2023),
      shiny::selectInput("acs_year", "ACS 5-Yr End Year:", choices = seq(2024, 2010), selected = 2024)
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      shiny::selectInput("job_type", "Job Type:", choices = job_type_choices, selected = "JT00"),
      shiny::selectInput("segment", "LODES Segment:", choices = segment_choices, selected = "S000")
    ),

    shiny::actionButton("run_comparison", "Fetch LODES & ACS Data", class = "btn-success w-100 fw-bold")
  ),

  bslib::navset_card_tab(
    id = "main_tabs",
    bslib::nav_panel(
      title = "1. Region Selector Map",
      value = "selector_tab",
      fillable = TRUE,
      mapgl::maplibreOutput("selector_map", height = "calc(100vh - 135px)", width = "100%")
    ),
    bslib::nav_panel(
      title = "2. Comparison Summary Report",
      value = "report_tab",
      shiny::uiOutput("report_ui")
    ),
    bslib::nav_panel(
      title = "3. ACS Labor Market Profile (S2301)",
      value = "acs_tab",
      shiny::uiOutput("acs_ui")
    ),
    bslib::nav_panel(
      title = "4. Regional Comparison Charts",
      value = "charts_tab",
      shiny::uiOutput("charts_ui")
    ),
    bslib::nav_panel(
      title = "5. Regional Profile Map (RAC & WAC)",
      value = "profile_map_tab",
      fillable = TRUE,
      mapgl::maplibreOutput("profile_map", height = "calc(100vh - 135px)", width = "100%")
    ),
    bslib::nav_panel(
      title = "6. OD Commuting Flows",
      value = "od_tab",
      shiny::uiOutput("od_ui")
    )
  )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  staged_sf          <- shiny::reactiveVal(NULL)
  comparison_results <- shiny::reactiveVal(NULL)
  next_reg_num       <- shiny::reactiveVal(3)

  make_default_regions <- function() {
    list(
      R1 = list(id = "R1", name = "Custom Region 1", color = region_palette[1], sf = NULL),
      R2 = list(id = "R2", name = "Custom Region 2", color = region_palette[2], sf = NULL)
    )
  }

  regions_store <- shiny::reactiveVal(make_default_regions())

  sync_region_names <- function(regs) {
    for (rid in names(regs)) {
      inp_val <- shiny::isolate(input[[paste0("name_", rid)]])
      if (!is.null(inp_val) && nzchar(trimws(inp_val))) {
        regs[[rid]]$name <- trimws(inp_val)
      }
    }
    regs
  }

  # ----------------------------------------------------------------------------
  # Master Reset All Regions, Names, Colors & Selections
  # ----------------------------------------------------------------------------
  shiny::observeEvent(input$reset_all_regions_btn, {
    regions_store(make_default_regions())
    next_reg_num(3)
    staged_sf(NULL)
    comparison_results(NULL)
    mapgl::clear_drawn_features(mapgl::maplibre_proxy("selector_map"))
    shiny::updateCheckboxInput(session, "exclude_higher_overlap", value = FALSE)
    shiny::updateSelectInput(
      session,
      "target_region",
      choices = c("Custom Region 1" = "R1", "Custom Region 2" = "R2"),
      selected = "R1"
    )
    shiny::showNotification(
      "All regions, names, and colors have been reset to default.",
      type = "message",
      duration = 3
    )
  })

  # ----------------------------------------------------------------------------
  # Export / Import Region Definitions (JSON)
  # ----------------------------------------------------------------------------
  output$export_regions_btn <- shiny::downloadHandler(
    filename = function() {
      paste0("lodes_region_definitions_", Sys.Date(), ".json")
    },
    content = function(file) {
      regs <- sync_region_names(regions_store())
      regions_store(regs)

      payload <- list(
        version = 1,
        exported_at = as.character(Sys.time()),
        exclude_higher_overlap = isTRUE(input$exclude_higher_overlap),
        regions = unname(lapply(regs, function(r) {
          areas_tbl <- if (is.null(r$sf) || nrow(r$sf) == 0) {
            tibble::tibble(
              UNIQUE_KEY = character(),
              MAP_GEOID  = character(),
              AREA_LABEL = character(),
              SRC_STATE  = character(),
              SRC_GEOM   = character()
            )
          } else {
            r$sf |>
              sf::st_drop_geometry() |>
              dplyr::select(UNIQUE_KEY, MAP_GEOID, AREA_LABEL, SRC_STATE, SRC_GEOM)
          }
          list(
            id    = r$id,
            name  = r$name,
            color = r$color,
            areas = areas_tbl
          )
        }))
      )

      base::writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE), file)
    }
  )

  shiny::observeEvent(input$import_regions_file, {
    file_info <- input$import_regions_file
    shiny::req(file_info$datapath)

    parsed <- base::tryCatch(
      jsonlite::fromJSON(file_info$datapath, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed) || is.null(parsed$regions) || length(parsed$regions) < 2) {
      shiny::showNotification("Invalid region definitions file. Please select a valid exported JSON file.", type = "error", duration = 6)
      return()
    }

    shiny::withProgress(message = "Importing Region Definitions...", value = 0.1, {
      all_area_rows <- lapply(parsed$regions, function(reg_item) {
        if (length(reg_item$areas) == 0) return(NULL)
        dplyr::bind_rows(lapply(reg_item$areas, tibble::as_tibble))
      })
      all_areas_df <- dplyr::bind_rows(all_area_rows)

      layer_lookup <- list()
      if (nrow(all_areas_df) > 0) {
        unique_pairs <- all_areas_df |> dplyr::distinct(SRC_STATE, SRC_GEOM)
        for (k in seq_len(nrow(unique_pairs))) {
          st_k <- unique_pairs$SRC_STATE[k]
          gm_k <- unique_pairs$SRC_GEOM[k]
          shiny::incProgress(0.7 / nrow(unique_pairs), detail = paste("Fetching", gm_k, "for", st_k))
          layer_lookup[[paste(st_k, gm_k, sep = "::")]] <- build_standardized_layer(gm_k, st_k)
        }
      }

      new_regs <- list()
      max_r_idx <- 0

      for (idx in seq_along(parsed$regions)) {
        reg_item <- parsed$regions[[idx]]
        rid <- if (!is.null(reg_item$id)) as.character(reg_item$id) else paste0("R", idx)
        num_part <- base::suppressWarnings(as.integer(sub("^R", "", rid)))
        if (!is.na(num_part) && num_part > max_r_idx) max_r_idx <- num_part

        col_val <- if (!is.null(reg_item$color)) {
          as.character(reg_item$color)
        } else {
          region_palette[((idx - 1) %% length(region_palette)) + 1]
        }

        reg_sf <- NULL
        if (length(reg_item$areas) > 0) {
          reg_areas_df <- dplyr::bind_rows(lapply(reg_item$areas, tibble::as_tibble)) |>
            dplyr::mutate(PAIR_KEY = paste(SRC_STATE, SRC_GEOM, sep = "::"))

          sf_pieces <- lapply(split(reg_areas_df, reg_areas_df$PAIR_KEY), function(sub_df) {
            base_lyr <- layer_lookup[[sub_df$PAIR_KEY[1]]]
            if (is.null(base_lyr)) return(NULL)
            base_lyr |> dplyr::filter(UNIQUE_KEY %in% sub_df$UNIQUE_KEY)
          })
          sf_pieces <- base::Filter(Negate(is.null), sf_pieces)
          if (length(sf_pieces) > 0) {
            reg_sf <- base::do.call(rbind, sf_pieces)
          }
        }

        new_regs[[rid]] <- list(
          id    = rid,
          name  = as.character(reg_item$name),
          color = col_val,
          sf    = reg_sf
        )
      }

      regions_store(new_regs)
      next_reg_num(max(max_r_idx + 1, length(new_regs) + 1))
      staged_sf(NULL)

      if (!is.null(parsed$exclude_higher_overlap)) {
        shiny::updateCheckboxInput(session, "exclude_higher_overlap", value = isTRUE(parsed$exclude_higher_overlap))
      }

      imported_sfs <- base::Filter(function(x) !is.null(x) && nrow(x) > 0, lapply(new_regs, function(r) r$sf))
      if (length(imported_sfs) > 0) {
        combined_imp <- base::do.call(rbind, imported_sfs)
        shiny::updateSelectInput(session, "state", selected = combined_imp$SRC_STATE[1])
        shiny::updateSelectInput(session, "geometry", selected = combined_imp$SRC_GEOM[1])
        bbox <- as.numeric(sf::st_bbox(combined_imp))
        mapgl::maplibre_proxy("selector_map") |> mapgl::fit_bounds(bbox, padding = 30)
      }

      shiny::showNotification(
        paste0("Imported ", length(new_regs), " custom regions successfully."),
        type = "message",
        duration = 4
      )
    })
  })

  # ----------------------------------------------------------------------------
  # Dynamic Region Management
  # ----------------------------------------------------------------------------
  shiny::observeEvent(input$add_region_btn, {
    regs <- sync_region_names(regions_store())
    num  <- next_reg_num()
    new_id <- paste0("R", num)

    used_colors <- vapply(regs, function(r) r$color, character(1))
    avail_colors <- setdiff(region_palette, used_colors)
    new_color <- if (length(avail_colors) > 0) {
      avail_colors[1]
    } else {
      region_palette[((length(regs)) %% length(region_palette)) + 1]
    }

    regs[[new_id]] <- list(
      id    = new_id,
      name  = paste0("Custom Region ", num),
      color = new_color,
      sf    = NULL
    )
    regions_store(regs)
    next_reg_num(num + 1)
    shiny::updateSelectInput(session, "target_region", selected = new_id)
  })

  shiny::observeEvent(input$update_names_btn, {
    regs <- sync_region_names(regions_store())
    regions_store(regs)
    shiny::showNotification("Region names updated.", type = "message", duration = 2)
  })

  shiny::observeEvent(regions_store(), {
    regs <- regions_store()
    choices <- stats::setNames(
      names(regs),
      vapply(regs, function(r) if (nzchar(trimws(r$name))) trimws(r$name) else r$id, character(1))
    )
    cur_sel <- shiny::isolate(input$target_region)
    if (is.null(cur_sel) || !cur_sel %in% names(regs)) {
      cur_sel <- names(regs)[1]
    }
    shiny::updateSelectInput(session, "target_region", choices = choices, selected = cur_sel)
  })

  output$dynamic_regions_ui <- shiny::renderUI({
    regs <- regions_store()
    can_delete <- length(regs) > 2

    lapply(seq_along(regs), function(idx) {
      r <- regs[[idx]]
      rid <- r$id
      n_areas <- if (is.null(r$sf)) 0 else nrow(r$sf)
      states_str <- if (n_areas > 0) paste0(" across ", paste(unique(r$sf$SRC_STATE), collapse = ", ")) else ""

      shiny::div(
        class = "region-card",
        style = paste0("border-left: 5px solid ", r$color, ";"),
        shiny::div(
          style = "display: flex; gap: 6px; align-items: center; margin-bottom: 3px;",
          shiny::textInput(
            inputId = paste0("name_", rid),
            label = NULL,
            value = r$name,
            width = "100%"
          ),
          if (can_delete) {
            shiny::actionButton(
              inputId = paste0("del_", rid),
              label = "✕",
              class = "btn-outline-danger btn-sm",
              style = "padding: 2px 7px; margin-bottom: 14px;",
              title = "Delete Region"
            )
          }
        ),
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; font-size: 0.82em; color: #475569; margin-top: -10px;",
          shiny::span(paste0("Priority #", idx, " • ", n_areas, " area(s)", states_str)),
          if (n_areas > 0) {
            shiny::div(
              style = "display: flex; gap: 8px; align-items: center;",
              shiny::actionLink(
                inputId = paste0("stage_", rid),
                label = "Edit Areas",
                style = "color: #2563eb; font-weight: 600;",
                title = "Move assigned areas back to Staged Selection to add or remove geographies"
              ),
              shiny::span("|", style = "color: #cbd5e1;"),
              shiny::actionLink(
                inputId = paste0("reset_", rid),
                label = "Clear",
                style = "color: #dc2626;",
                title = "Remove all areas from this region"
              )
            )
          } else {
            shiny::span("Empty", style = "color: #94a3b8; font-style: italic;")
          }
        )
      )
    })
  })

  shiny::observe({
    regs <- regions_store()
    for (rid in names(regs)) {
      local({
        id <- rid

        shiny::observeEvent(input[[paste0("stage_", id)]], {
          cur <- sync_region_names(regions_store())
          shiny::req(!is.null(cur[[id]]), !is.null(cur[[id]]$sf), nrow(cur[[id]]$sf) > 0)

          moved_sf <- cur[[id]]$sf
          reg_label <- cur[[id]]$name

          existing_staged <- staged_sf()
          if (is.null(existing_staged) || nrow(existing_staged) == 0) {
            staged_sf(moved_sf)
          } else {
            add_rows <- moved_sf |> dplyr::filter(!UNIQUE_KEY %in% existing_staged$UNIQUE_KEY)
            staged_sf(rbind(existing_staged, add_rows))
          }

          cur[[id]]$sf <- NULL
          regions_store(cur)

          shiny::updateSelectInput(session, "target_region", selected = id)

          bslib::nav_select("main_tabs", selected = "selector_tab", session = session)
          shiny::updateSelectInput(session, "state", selected = moved_sf$SRC_STATE[1])
          shiny::updateSelectInput(session, "geometry", selected = moved_sf$SRC_GEOM[1])
          bbox <- as.numeric(sf::st_bbox(moved_sf))
          mapgl::maplibre_proxy("selector_map") |> mapgl::fit_bounds(bbox, padding = 30)

          shiny::showNotification(
            paste0("Moved ", nrow(moved_sf), " area(s) from '", reg_label, "' to Staged Selection. Click polygons to add or remove, then click 'Add Areas'."),
            type = "message",
            duration = 5
          )
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("reset_", id)]], {
          cur <- sync_region_names(regions_store())
          if (!is.null(cur[[id]])) {
            cur[[id]]$sf <- NULL
            regions_store(cur)
          }
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[paste0("del_", id)]], {
          cur <- sync_region_names(regions_store())
          if (length(cur) > 2 && !is.null(cur[[id]])) {
            cur[[id]] <- NULL
            regions_store(cur)
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # Fetch current state/geometry layer
  current_layer <- shiny::reactive({
    shiny::req(input$state, input$geometry)
    shiny::showNotification(paste("Loading", input$geometry, "for", input$state, "..."), id = "geo_load", duration = NULL)
    on.exit(shiny::removeNotification("geo_load"), add = TRUE)

    build_standardized_layer(input$geometry, input$state)
  })

  # Initialize MapLibre Selector Map
  output$selector_map <- mapgl::renderMaplibre({
    layer <- shiny::isolate(current_layer()) |>
      dplyr::mutate(
        fill_col = "#94a3b8",
        fill_op  = 0.2
      )

    mapgl::maplibre(style = mapgl::carto_style("positron"), bounds = as.numeric(sf::st_bbox(layer))) |>
      mapgl::add_navigation_control(position = "top-left") |>
      mapgl::add_draw_control(
        position = "top-right",
        controls = list(
          polygon = TRUE,
          line_string = FALSE,
          point = FALSE,
          trash = FALSE,
          combine_features = FALSE,
          uncombine_features = FALSE
        )
      ) |>
      mapgl::add_fill_layer(
        id = "base_fill",
        source = layer,
        fill_color = mapgl::get_column("fill_col"),
        fill_opacity = mapgl::get_column("fill_op"),
        tooltip = "AREA_LABEL",
        hover_options = list(fill_opacity = 0.85)
      ) |>
      mapgl::add_line_layer(
        id = "base_line",
        source = layer,
        line_color = "#334155",
        line_width = 1
      )
  })

  shiny::observeEvent(current_layer(), {
    bbox <- as.numeric(sf::st_bbox(current_layer()))
    mapgl::maplibre_proxy("selector_map") |>
      mapgl::fit_bounds(bbox, padding = 30)
  }, ignoreInit = TRUE)

  shiny::observe({
    layer  <- current_layer()
    staged <- staged_sf()
    regs   <- regions_store()

    staged_keys <- if (!is.null(staged)) staged$UNIQUE_KEY else character(0)

    key_color_map <- character(0)
    for (r in rev(regs)) {
      if (!is.null(r$sf) && nrow(r$sf) > 0) {
        key_color_map[r$sf$UNIQUE_KEY] <- r$color
      }
    }

    layer <- layer |>
      dplyr::mutate(
        saved_col = unname(key_color_map[UNIQUE_KEY]),
        fill_col = dplyr::case_when(
          UNIQUE_KEY %in% staged_keys ~ "#e74c3c",
          !is.na(saved_col)           ~ saved_col,
          TRUE                        ~ "#94a3b8"
        ),
        fill_op = dplyr::case_when(
          UNIQUE_KEY %in% staged_keys | !is.na(saved_col) ~ 0.65,
          TRUE ~ 0.2
        )
      )

    legend_vals <- c("Staged Selection", vapply(regs, function(r) r$name, character(1)), "Unselected")
    legend_cols <- c("#e74c3c", vapply(regs, function(r) r$color, character(1)), "#94a3b8")

    proxy <- mapgl::maplibre_proxy("selector_map") |>
      mapgl::clear_layer("base_fill") |>
      mapgl::clear_layer("base_line") |>
      mapgl::clear_layer("other_regions_fill") |>
      mapgl::clear_layer("other_staged_fill") |>
      mapgl::add_fill_layer(
        id = "base_fill",
        source = layer,
        fill_color = mapgl::get_column("fill_col"),
        fill_opacity = mapgl::get_column("fill_op"),
        tooltip = "AREA_LABEL",
        hover_options = list(fill_opacity = 0.85)
      ) |>
      mapgl::add_line_layer(
        id = "base_line",
        source = layer,
        line_color = "#334155",
        line_width = 1
      ) |>
      mapgl::add_legend(
        legend_title = "Selection Status",
        values = unname(legend_vals),
        colors = unname(legend_cols),
        type = "categorical",
        position = "bottom-right"
      )

    other_list <- lapply(rev(regs), function(r) {
      if (!is.null(r$sf) && nrow(r$sf) > 0) {
        sub_sf <- r$sf |> dplyr::filter(!UNIQUE_KEY %in% layer$UNIQUE_KEY)
        if (nrow(sub_sf) > 0) {
          sub_sf |> dplyr::mutate(reg_col = r$color, reg_tip = paste0(r$name, ": ", AREA_LABEL))
        } else NULL
      } else NULL
    })
    other_sf <- base::do.call(rbind, base::Filter(Negate(is.null), other_list))

    if (!is.null(other_sf) && nrow(other_sf) > 0) {
      proxy <- proxy |>
        mapgl::add_fill_layer(
          id = "other_regions_fill",
          source = other_sf,
          fill_color = mapgl::get_column("reg_col"),
          fill_opacity = 0.55,
          tooltip = "reg_tip"
        )
    }

    if (!is.null(staged) && nrow(staged) > 0) {
      other_staged <- staged |>
        dplyr::filter(!UNIQUE_KEY %in% layer$UNIQUE_KEY) |>
        dplyr::mutate(staged_tip = paste0("Staged: ", AREA_LABEL))
      if (nrow(other_staged) > 0) {
        proxy <- proxy |>
          mapgl::add_fill_layer(
            id = "other_staged_fill",
            source = other_staged,
            fill_color = "#e74c3c",
            fill_opacity = 0.65,
            tooltip = "staged_tip"
          )
      }
    }
  })

  shiny::observeEvent(input$selector_map_feature_click, {
    click <- input$selector_map_feature_click
    shiny::req(!is.null(click$lng), !is.null(click$lat))

    layer <- current_layer()
    pt <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
    hit_idx <- base::suppressMessages(which(sf::st_intersects(layer, pt, sparse = FALSE)[, 1]))
    shiny::req(length(hit_idx) > 0)

    clicked_row <- layer[hit_idx[1], ]
    clicked_key <- clicked_row$UNIQUE_KEY

    cur <- staged_sf()
    if (is.null(cur) || nrow(cur) == 0) {
      staged_sf(clicked_row)
    } else if (clicked_key %in% cur$UNIQUE_KEY) {
      rem <- cur |> dplyr::filter(UNIQUE_KEY != clicked_key)
      staged_sf(if (nrow(rem) == 0) NULL else rem)
    } else {
      staged_sf(rbind(cur, clicked_row))
    }
  })

  shiny::observeEvent(input$selector_map_drawn_features, {
    drawn <- mapgl::get_drawn_features(mapgl::maplibre_proxy("selector_map"))
    if (is.null(drawn) || nrow(drawn) == 0) {
      shiny::req(input$selector_map_drawn_features)
      drawn <- base::tryCatch(
        sf::st_read(input$selector_map_drawn_features, quiet = TRUE),
        error = function(e) NULL
      )
    }
    shiny::req(!is.null(drawn), nrow(drawn) > 0)

    layer <- current_layer()
    drawn <- sf::st_transform(drawn, sf::st_crs(layer)) |> sf::st_make_valid()
    layer_valid <- sf::st_make_valid(layer)

    hits <- base::suppressMessages(sf::st_intersects(layer_valid, drawn, sparse = FALSE))
    hit_mask <- apply(hits, 1, any)
    hit_rows <- layer[hit_mask, ]

    if (nrow(hit_rows) > 0) {
      cur <- staged_sf()
      if (is.null(cur) || nrow(cur) == 0) {
        staged_sf(hit_rows)
      } else {
        new_rows <- hit_rows |> dplyr::filter(!UNIQUE_KEY %in% cur$UNIQUE_KEY)
        staged_sf(rbind(cur, new_rows))
      }
    }

    mapgl::clear_drawn_features(mapgl::maplibre_proxy("selector_map"))
  })

  output$staged_summary <- shiny::renderText({
    cur <- staged_sf()
    n <- if (is.null(cur)) 0 else nrow(cur)
    paste0("Staged Selection: ", n, " area(s) ready")
  })

  shiny::observeEvent(input$clear_staged, {
    staged_sf(NULL)
    mapgl::clear_drawn_features(mapgl::maplibre_proxy("selector_map"))
  })

  shiny::observeEvent(input$assign_staged, {
    cur <- staged_sf()
    tid <- input$target_region
    shiny::req(!is.null(cur), nrow(cur) > 0, !is.null(tid))

    regs <- sync_region_names(regions_store())
    shiny::req(!is.null(regs[[tid]]))

    existing <- regs[[tid]]$sf
    if (is.null(existing) || nrow(existing) == 0) {
      regs[[tid]]$sf <- cur
    } else {
      add_rows <- cur |> dplyr::filter(!UNIQUE_KEY %in% existing$UNIQUE_KEY)
      regs[[tid]]$sf <- rbind(existing, add_rows)
    }
    regions_store(regs)
    staged_sf(NULL)
  })

  # ----------------------------------------------------------------------------
  # Fetch LODES RAC + WAC + OD & ACS S2301 Across All Custom Regions
  # ----------------------------------------------------------------------------
  shiny::observeEvent(input$run_comparison, {
    sf::sf_use_s2(FALSE)

    regs <- sync_region_names(regions_store())
    regions_store(regs)

    empty_regs <- base::Filter(function(r) is.null(r$sf) || nrow(r$sf) == 0, regs)

    if (length(empty_regs) > 0) {
      missing_names <- paste(vapply(empty_regs, function(r) r$name, character(1)), collapse = ", ")
      shiny::showNotification(paste0("Please assign at least one area to: ", missing_names), type = "error", duration = 6)
      return()
    }

    year_val        <- input$year
    acs_year_val    <- input$acs_year
    jt_val          <- input$job_type
    seg_val         <- input$segment
    seg_lbl         <- names(segment_choices)[segment_choices == seg_val]
    exclude_overlap <- isTRUE(input$exclude_higher_overlap)

    all_states <- unique(unlist(lapply(regs, function(r) r$sf$SRC_STATE)))

    shiny::withProgress(message = "Building Multi-Region LODES & ACS Profiles...", value = 0, {
      shiny::incProgress(0.08, detail = "Dissolving custom region geometries")

      dissolved_list <- list()
      base_parts_list <- list()
      cumulative_higher_geom <- NULL

      for (idx in seq_along(regs)) {
        r <- regs[[idx]]
        r_label <- if (nzchar(trimws(r$name))) trimws(r$name) else r$id
        base_parts_list[[idx]] <- r$sf |> dplyr::select(UNIQUE_KEY, geometry)
        r_geom <- dissolve_quietly(r$sf)

        if (exclude_overlap && !is.null(cumulative_higher_geom)) {
          r_geom <- difference_quietly(r_geom, cumulative_higher_geom)
          if (length(r_geom) == 0 || all(sf::st_is_empty(r_geom))) {
            shiny::showNotification(
              paste0("Excluding higher-priority overlap leaves '", r_label, "' empty. Add more areas or uncheck the overlap option."),
              type = "error", duration = 6
            )
            return()
          }
        }

        cumulative_higher_geom <- if (is.null(cumulative_higher_geom)) {
          r_geom
        } else {
          union_quietly(cumulative_higher_geom, r_geom)
        }

        dissolved_list[[idx]] <- sf::st_sf(
          region_id    = r$id,
          region_name  = r_label,
          region_color = r$color,
          priority     = idx,
          geometry     = r_geom
        )
      }

      regions_poly <- base::do.call(rbind, rev(dissolved_list))
      base_parts_sf <- base::do.call(rbind, base_parts_list) |> dplyr::distinct(UNIQUE_KEY, .keep_all = TRUE)

      reg_meta <- tibble::tibble(
        region_id    = vapply(regs, function(r) r$id, character(1)),
        region_name  = vapply(regs, function(r) if (nzchar(trimws(r$name))) trimws(r$name) else r$id, character(1)),
        region_color = vapply(regs, function(r) r$color, character(1))
      )

      # Fetch ACS 5-Year Subject Table S2301 (all demographic rows)
      shiny::incProgress(0.14, detail = paste("Fetching ACS", acs_year_val, "5-Year Table S2301 via tidycensus"))
      acs_full <- base::tryCatch(
        fetch_acs_s2301_regions(regs, regions_poly, reg_meta, acs_year_val, exclude_overlap),
        error = function(e) {
          shiny::showNotification(
            paste0("ACS S2301 fetch warning (LODES will still load): ", e$message,
                   ". Ensure tidycensus::census_api_key() is configured if needed."),
            type = "warning",
            duration = 10
          )
          NULL
        }
      )

      acs_headline <- if (!is.null(acs_full) && nrow(acs_full) > 0) {
        acs_full |> dplyr::filter(row_id == "001")
      } else {
        NULL
      }

      state_wac_list <- list()
      state_rac_list <- list()
      all_block_lookup_list <- list()
      od_raw_list <- list()

      for (i in seq_along(all_states)) {
        st <- all_states[i]
        st_lower <- tolower(st)
        shiny::incProgress(0.60 / length(all_states), detail = paste("Downloading LODES RAC, WAC & OD for", st))

        xwalk_url <- paste0(
          "https://lehd.ces.census.gov/data/lodes/LODES8/",
          st_lower, "/", st_lower, "_xwalk.csv.gz"
        )
        wac_url <- paste0(
          "https://lehd.ces.census.gov/data/lodes/LODES8/",
          st_lower, "/wac/", st_lower, "_wac_", seg_val, "_", jt_val, "_", year_val, ".csv.gz"
        )
        rac_url <- paste0(
          "https://lehd.ces.census.gov/data/lodes/LODES8/",
          st_lower, "/rac/", st_lower, "_rac_", seg_val, "_", jt_val, "_", year_val, ".csv.gz"
        )
        od_main_url <- paste0(
          "https://lehd.ces.census.gov/data/lodes/LODES8/",
          st_lower, "/od/", st_lower, "_od_main_", jt_val, "_", year_val, ".csv.gz"
        )
        od_aux_url <- paste0(
          "https://lehd.ces.census.gov/data/lodes/LODES8/",
          st_lower, "/od/", st_lower, "_od_aux_", jt_val, "_", year_val, ".csv.gz"
        )

        xwalk_dest   <- file.path(cache_dir, basename(xwalk_url))
        wac_dest     <- file.path(cache_dir, basename(wac_url))
        rac_dest     <- file.path(cache_dir, basename(rac_url))
        od_main_dest <- file.path(cache_dir, basename(od_main_url))
        od_aux_dest  <- file.path(cache_dir, basename(od_aux_url))

        if (!file.exists(xwalk_dest))   utils::download.file(xwalk_url, xwalk_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(wac_dest))     utils::download.file(wac_url, wac_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(rac_dest))     utils::download.file(rac_url, rac_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(od_main_dest)) base::try(utils::download.file(od_main_url, od_main_dest, mode = "wb", quiet = TRUE), silent = TRUE)
        if (!file.exists(od_aux_dest))  base::try(utils::download.file(od_aux_url, od_aux_dest, mode = "wb", quiet = TRUE), silent = TRUE)

        xwalk_df <- readr::read_csv(
          xwalk_dest,
          col_types = readr::cols_only(tabblk2020 = "c", blklatdd = "d", blklondd = "d")
        ) |> dplyr::filter(!is.na(blklatdd), !is.na(blklondd))

        blk_pts <- sf::st_as_sf(xwalk_df, coords = c("blklondd", "blklatdd"), crs = 4326)
        joined_pts <- base::suppressMessages(
          sf::st_join(blk_pts, regions_poly, join = sf::st_within, left = FALSE)
        ) |>
          sf::st_drop_geometry() |>
          dplyr::select(tabblk2020, region_id)

        highest_prio_pts <- base::suppressMessages(
          sf::st_join(blk_pts, regions_poly |> dplyr::arrange(priority), join = sf::st_within, left = FALSE)
        ) |>
          sf::st_drop_geometry() |>
          dplyr::distinct(tabblk2020, .keep_all = TRUE) |>
          dplyr::select(tabblk2020, region_id)
        all_block_lookup_list[[st]] <- highest_prio_pts

        if (nrow(joined_pts) > 0) {
          wac_raw <- readr::read_csv(wac_dest, col_types = readr::cols(.default = "d", w_geocode = "c"))
          rac_raw <- readr::read_csv(rac_dest, col_types = readr::cols(.default = "d", h_geocode = "c"))

          wac_blk <- summarize_lodes_blocks(wac_raw, "w_geocode") |>
            dplyr::inner_join(joined_pts, by = "tabblk2020", relationship = "many-to-many") |>
            dplyr::group_by(region_id) |>
            dplyr::summarise(dplyr::across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

          rac_blk <- summarize_lodes_blocks(rac_raw, "h_geocode") |>
            dplyr::inner_join(joined_pts, by = "tabblk2020", relationship = "many-to-many") |>
            dplyr::group_by(region_id) |>
            dplyr::summarise(dplyr::across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

          state_wac_list[[st]] <- wac_blk
          state_rac_list[[st]] <- rac_blk
        }

        od_cols <- readr::cols_only(
          w_geocode = "c", h_geocode = "c",
          S000 = "d", SA01 = "d", SA02 = "d", SA03 = "d",
          SE01 = "d", SE02 = "d", SE03 = "d",
          SI01 = "d", SI02 = "d", SI03 = "d"
        )
        if (file.exists(od_main_dest)) {
          od_raw_list[[paste0(st, "_main")]] <- readr::read_csv(od_main_dest, col_types = od_cols)
        }
        if (file.exists(od_aux_dest) && length(all_states) > 1) {
          od_raw_list[[paste0(st, "_aux")]] <- readr::read_csv(od_aux_dest, col_types = od_cols)
        }
      }

      shiny::incProgress(0.15, detail = "Compiling WAC, RAC & OD commuting matrices")

      wac_totals <- dplyr::bind_rows(state_wac_list) |>
        dplyr::group_by(region_id) |>
        dplyr::summarise(dplyr::across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      wac_totals <- reg_meta |> dplyr::left_join(wac_totals, by = "region_id") |> dplyr::mutate(dplyr::across(total_jobs:sex_female, ~dplyr::coalesce(., 0)))

      rac_totals <- dplyr::bind_rows(state_rac_list) |>
        dplyr::group_by(region_id) |>
        dplyr::summarise(dplyr::across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      rac_totals <- reg_meta |> dplyr::left_join(rac_totals, by = "region_id") |> dplyr::mutate(dplyr::across(total_jobs:sex_female, ~dplyr::coalesce(., 0)))

      block_lookup <- dplyr::bind_rows(all_block_lookup_list) |> dplyr::distinct(tabblk2020, .keep_all = TRUE)
      id_to_name <- stats::setNames(reg_meta$region_name, reg_meta$region_id)

      od_flows <- dplyr::bind_rows(od_raw_list) |>
        dplyr::left_join(block_lookup |> dplyr::rename(h_geocode = tabblk2020, origin_id = region_id), by = "h_geocode") |>
        dplyr::left_join(block_lookup |> dplyr::rename(w_geocode = tabblk2020, dest_id   = region_id), by = "w_geocode") |>
        dplyr::filter(!is.na(origin_id) | !is.na(dest_id)) |>
        dplyr::mutate(
          origin_id   = dplyr::coalesce(origin_id, "OUTSIDE"),
          dest_id     = dplyr::coalesce(dest_id, "OUTSIDE"),
          Origin      = ifelse(origin_id == "OUTSIDE", "Outside Selected Regions", unname(id_to_name[origin_id])),
          Destination = ifelse(dest_id == "OUTSIDE", "Outside Selected Regions", unname(id_to_name[dest_id]))
        ) |>
        dplyr::group_by(origin_id, Origin, dest_id, Destination) |>
        dplyr::summarise(
          Total_Jobs     = sum(S000, na.rm = TRUE),
          Age_Under_30   = sum(SA01, na.rm = TRUE),
          Age_30_to_54   = sum(SA02, na.rm = TRUE),
          Age_55_Plus    = sum(SA03, na.rm = TRUE),
          Earn_Low       = sum(SE01, na.rm = TRUE),
          Earn_Mid       = sum(SE02, na.rm = TRUE),
          Earn_High      = sum(SE03, na.rm = TRUE),
          Ind_Goods      = sum(SI01, na.rm = TRUE),
          Ind_Trade      = sum(SI02, na.rm = TRUE),
          Ind_Services   = sum(SI03, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::group_by(origin_id) |>
        dplyr::mutate(
          origin_tot = sum(Total_Jobs, na.rm = TRUE),
          Share_of_Origin_Workers = ifelse(origin_tot > 0, Total_Jobs / origin_tot, 0)
        ) |>
        dplyr::select(-origin_tot) |>
        dplyr::ungroup() |>
        dplyr::group_by(dest_id) |>
        dplyr::mutate(
          dest_tot = sum(Total_Jobs, na.rm = TRUE),
          Share_of_Dest_Jobs = ifelse(dest_tot > 0, Total_Jobs / dest_tot, 0)
        ) |>
        dplyr::select(-dest_tot) |>
        dplyr::ungroup()

      comparison_results(list(
        reg_meta        = reg_meta,
        exclude_overlap = exclude_overlap,
        year            = year_val,
        acs_year        = acs_year_val,
        job_type        = names(job_type_choices)[job_type_choices == jt_val],
        segment_label   = seg_lbl,
        regions_poly    = regions_poly,
        base_parts_sf   = base_parts_sf,
        wac_totals      = wac_totals,
        rac_totals      = rac_totals,
        od_flows        = od_flows,
        acs_summary     = acs_headline,
        acs_full        = acs_full
      ))

      bslib::nav_select("main_tabs", selected = "report_tab", session = session)
    })
  })

  # ----------------------------------------------------------------------------
  # Helper: Build Grouped Counts & Shares Table (Omits All-Zero Rows)
  # ----------------------------------------------------------------------------
  build_grouped_table_data <- function(totals_df) {
    n_reg <- nrow(totals_df)

    lapply(seq_len(nrow(metric_catalog)), function(idx) {
      v <- metric_catalog$var[idx]
      is_tot <- (v == "total_jobs")

      counts <- vapply(seq_len(n_reg), function(i) totals_df[[v]][i], numeric(1))
      if (!is_tot && sum(counts, na.rm = TRUE) == 0) return(NULL)

      row_list <- list(
        Category       = metric_catalog$category[idx],
        Characteristic = metric_catalog$label[idx]
      )

      for (i in seq_len(n_reg)) {
        rid <- totals_df$region_id[i]
        row_list[[paste0("Cnt_", rid)]] <- counts[i]
      }

      for (i in seq_len(n_reg)) {
        rid <- totals_df$region_id[i]
        tot <- totals_df$total_jobs[i]
        shr <- if (tot > 0) counts[i] / tot else 0
        row_list[[paste0("Shr_", rid)]] <- if (is_tot) NA_real_ else shr
      }

      tibble::as_tibble(row_list)
    }) |> dplyr::bind_rows()
  }

  format_dt_grouped <- function(df, meta) {
    n_reg <- nrow(meta)

    sketch <- htmltools::tags$table(
      class = "display",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th(rowspan = 2, "Category"),
          htmltools::tags$th(rowspan = 2, "Characteristic"),
          htmltools::tags$th(colspan = n_reg, class = "group-header-counts", "Counts (#)"),
          htmltools::tags$th(colspan = n_reg, class = "group-header-shares section-divider-left", "Share of Total (%)")
        ),
        htmltools::tags$tr(
          lapply(meta$region_name, function(rname) htmltools::tags$th(class = "dt-right", rname)),
          lapply(seq_along(meta$region_name), function(i) {
            cls <- if (i == 1) "section-divider-left share-col-tint dt-right" else "share-col-tint dt-right"
            htmltools::tags$th(class = cls, meta$region_name[i])
          })
        )
      )
    )

    count_cols <- grep("^Cnt_", names(df), value = TRUE)
    share_cols <- grep("^Shr_", names(df), value = TRUE)

    cnt_indices     <- seq(2, 1 + n_reg)
    first_share_idx <- 2 + n_reg
    rest_share_idx  <- if (n_reg > 1) seq(first_share_idx + 1, 1 + 2 * n_reg) else integer(0)

    col_defs <- list(
      list(className = "dt-right", targets = cnt_indices),
      list(className = "section-divider-left share-col-tint dt-right", targets = first_share_idx)
    )
    if (length(rest_share_idx) > 0) {
      col_defs[[3]] <- list(className = "share-col-tint dt-right", targets = rest_share_idx)
    }

    DT::datatable(
      df,
      container = sketch,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        columnDefs = col_defs
      )
    ) |>
      DT::formatCurrency(count_cols, currency = "", digits = 0) |>
      DT::formatPercentage(share_cols, digits = 1)
  }

  # ----------------------------------------------------------------------------
  # Tab 2: Comparison Summary Report
  # ----------------------------------------------------------------------------
  output$report_ui <- shiny::renderUI({
    res <- comparison_results()
    if (is.null(res)) {
      return(shiny::div(
        style = "padding: 40px; text-align: center; color: #64748b;",
        shiny::h4("No Comparison Generated Yet"),
        shiny::p("Build your custom regions in Tab 1, then click 'Fetch LODES & ACS Data'.")
      ))
    }

    meta <- res$reg_meta
    overlap_note <- if (isTRUE(res$exclude_overlap)) {
      " | Overlap Handling: Higher-priority regions excluded from lower regions"
    } else {
      " | Overlap Handling: Full selected boundaries included"
    }

    kpi_cards <- lapply(seq_len(nrow(meta)), function(i) {
      rid   <- meta$region_id[i]
      rname <- meta$region_name[i]
      rcol  <- meta$region_color[i]
      w_tot <- res$wac_totals$total_jobs[res$wac_totals$region_id == rid]
      r_tot <- res$rac_totals$total_jobs[res$rac_totals$region_id == rid]

      shiny::div(
        style = paste0("background: #f8fafc; border-left: 5px solid ", rcol, "; padding: 10px 14px; border-radius: 6px;"),
        shiny::div(style = "font-weight: 700; color: #0f172a; font-size: 1.05em; margin-bottom: 4px;", rname),
        shiny::div(style = "font-size: 0.92em; color: #0284c7;", shiny::HTML(paste0("Workplace (WAC): <b>", scales::comma(w_tot), "</b>"))),
        shiny::div(style = "font-size: 0.92em; color: #059669;", shiny::HTML(paste0("Residence (RAC): <b>", scales::comma(r_tot), "</b>")))
      )
    })

    shiny::tagList(
      shiny::div(
        style = "padding: 16px;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; margin-bottom: 14px;",
          shiny::div(
            shiny::h4(paste(meta$region_name, collapse = " vs. "), style = "margin: 0; color: #1e3a8a; font-weight: 700;"),
            shiny::p(style = "margin: 2px 0 0 0; color: #475569; font-size: 0.92em;",
                     paste0("LODES Year: ", res$year, " | ACS 5-Year: ", res$acs_year, " | Job Type: ", res$job_type, " | Segment: ", res$segment_label, overlap_note))
          ),
          shiny::div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            shiny::downloadButton("download_pdf_report", "Download Executive PDF Report", class = "btn-danger btn-sm"),
            shiny::downloadButton("download_report_csv", "Download RAC & WAC CSV", class = "btn-outline-primary btn-sm")
          )
        ),

        bslib::layout_column_wrap(
          width = "240px",
          !!!kpi_cards
        ),

        shiny::hr(),

        shiny::h5("Area Characteristics by Work Location (WAC)", style = "color: #0284c7; font-weight: 700;"),
        DT::DTOutput("wac_table"),

        shiny::hr(),

        shiny::h5("Area Characteristics by Home Location (RAC)", style = "color: #059669; font-weight: 700;"),
        DT::DTOutput("rac_table")
      )
    )
  })

  output$wac_table <- DT::renderDT({
    res <- comparison_results()
    shiny::req(res)
    df <- build_grouped_table_data(res$wac_totals)
    format_dt_grouped(df, res$reg_meta)
  })

  output$rac_table <- DT::renderDT({
    res <- comparison_results()
    shiny::req(res)
    df <- build_grouped_table_data(res$rac_totals)
    format_dt_grouped(df, res$reg_meta)
  })

  output$download_report_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("lodes_multi_region_rac_wac_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- comparison_results()
      shiny::req(res)
      meta <- res$reg_meta

      wac_df <- build_grouped_table_data(res$wac_totals) |> dplyr::mutate(Dataset = "WAC (Work Location)", .before = 1)
      rac_df <- build_grouped_table_data(res$rac_totals) |> dplyr::mutate(Dataset = "RAC (Home Location)", .before = 1)
      out <- dplyr::bind_rows(wac_df, rac_df)

      col_names <- c(
        "Dataset",
        "Category",
        "Characteristic",
        paste0("Count (#) - ", meta$region_name),
        paste0("Share (%) - ", meta$region_name)
      )

      names(out) <- col_names
      readr::write_csv(out, file)
    }
  )

  # ----------------------------------------------------------------------------
  # Tab 3: ACS 5-Year Labor Market Profile (Table S2301)
  # ----------------------------------------------------------------------------
  output$acs_ui <- shiny::renderUI({
    res <- comparison_results()
    if (is.null(res)) {
      return(shiny::div(
        style = "padding: 40px; text-align: center; color: #64748b;",
        shiny::h4("No ACS Labor Market Data Generated Yet"),
        shiny::p("Build your custom regions in Tab 1 and click 'Fetch LODES & ACS Data' to view ACS Table S2301 estimates.")
      ))
    }

    acs <- res$acs_summary
    if (is.null(acs) || nrow(acs) == 0) {
      return(shiny::div(
        style = "padding: 40px; text-align: center; color: #b91c1c;",
        shiny::h4("ACS Table S2301 Data Unavailable"),
        shiny::p("The tidycensus API call did not return data. Verify your internet connection or configure tidycensus::census_api_key().")
      ))
    }

    acs_cards <- lapply(seq_len(nrow(acs)), function(i) {
      r <- acs[i, ]
      lfpr_str  <- if (!is.na(r$lfpr_pct)) sprintf("%.1f%%", r$lfpr_pct) else "—"
      unemp_str <- if (!is.na(r$unemp_rate_pct)) sprintf("%.1f%%", r$unemp_rate_pct) else "—"

      shiny::div(
        style = paste0("background: #f8fafc; border-left: 5px solid ", r$region_color, "; padding: 10px 14px; border-radius: 6px;"),
        shiny::div(style = "font-weight: 700; color: #0f172a; font-size: 1.05em; margin-bottom: 4px;", r$region_name),
        shiny::div(style = "font-size: 0.9em; color: #334155;", shiny::HTML(paste0("Pop. 16+: <b>", scales::comma(r$population), "</b>"))),
        shiny::div(style = "font-size: 0.9em; color: #0284c7;", shiny::HTML(paste0("LFPR: <b>", lfpr_str, "</b>"))),
        shiny::div(style = "font-size: 0.9em; color: #d97706;", shiny::HTML(paste0("Unemployment Rate: <b>", unemp_str, "</b>")))
      )
    })

    shiny::tagList(
      shiny::div(
        style = "padding: 16px;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; margin-bottom: 14px;",
          shiny::div(
            shiny::h4("ACS 5-Year Labor Market Profile (Table S2301)", style = "margin: 0; color: #1e3a8a; font-weight: 700;"),
            shiny::p(style = "margin: 2px 0 0 0; color: #475569; font-size: 0.92em;",
                     paste0("Survey: ", res$acs_year, " 5-Year American Community Survey (Subject Table S2301: Employment Status; CSV download includes all demographic subsets)"))
          ),
          shiny::downloadButton("download_acs_csv", "Download ACS S2301 CSV (All Subsets)", class = "btn-outline-primary btn-sm")
        ),

        bslib::layout_column_wrap(
          width = "240px",
          !!!acs_cards
        ),

        shiny::hr(),

        shiny::h5("Headline Employment Status Comparison (Population 16+)", style = "color: #1e3a8a; font-weight: 700;"),
        DT::DTOutput("acs_s2301_dt"),

        shiny::hr(),

        shiny::h5("Labor Force Participation & Unemployment Rate Comparison", style = "color: #0284c7; font-weight: 700;"),
        shiny::div(
          style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
          shiny::plotOutput("acs_rates_plot", height = "380px", width = "100%")
        )
      )
    )
  })

  output$acs_s2301_dt <- DT::renderDT({
    res <- comparison_results()
    shiny::req(res, res$acs_summary)

    df <- res$acs_summary |>
      dplyr::transmute(
        Region                = region_name,
        Pop_16_Plus           = population,
        Pop_16_MOE            = population_moe,
        Est_Labor_Force       = est_labor_force,
        LFPR                  = lfpr_pct / 100,
        LFPR_MOE              = lfpr_moe / 100,
        Emp_Pop_Ratio         = emp_pop_ratio_pct / 100,
        Emp_Pop_Ratio_MOE     = emp_pop_ratio_moe / 100,
        Unemployment_Rate     = unemp_rate_pct / 100,
        Unemployment_Rate_MOE = unemp_rate_moe / 100,
        Method                = aggregation_method
      )

    col_names <- c(
      "Region",
      "Population 16+",
      "Pop. 16+ MOE (+/-)",
      "Est. Labor Force",
      "Labor Force Part. Rate (%)",
      "LFPR MOE (+/-)",
      "Emp.-Pop. Ratio (%)",
      "Emp.-Pop. MOE (+/-)",
      "Unemployment Rate (%)",
      "Unemp. MOE (+/-)",
      "Aggregation Method"
    )

    DT::datatable(
      df,
      colnames = col_names,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-right", targets = 1:9)
        )
      )
    ) |>
      DT::formatCurrency(c("Pop_16_Plus", "Pop_16_MOE", "Est_Labor_Force"), currency = "", digits = 0) |>
      DT::formatPercentage(c("LFPR", "LFPR_MOE", "Emp_Pop_Ratio", "Emp_Pop_Ratio_MOE", "Unemployment_Rate", "Unemployment_Rate_MOE"), digits = 1)
  })

  output$acs_rates_plot <- shiny::renderPlot({
    res <- comparison_results()
    shiny::req(res, res$acs_summary)

    acs <- res$acs_summary
    reg_levels <- as.character(acs$region_name)
    pal_vec <- stats::setNames(as.character(acs$region_color), reg_levels)

    plot_df <- dplyr::bind_rows(
      acs |> dplyr::transmute(
        region_name = factor(as.character(region_name), levels = reg_levels),
        Metric = "Labor Force Participation Rate (%)",
        Rate = as.numeric(lfpr_pct) / 100,
        Label = ifelse(is.na(lfpr_pct), "", sprintf("%.1f%%", lfpr_pct))
      ),
      acs |> dplyr::transmute(
        region_name = factor(as.character(region_name), levels = reg_levels),
        Metric = "Unemployment Rate (%)",
        Rate = as.numeric(unemp_rate_pct) / 100,
        Label = ifelse(is.na(unemp_rate_pct), "", sprintf("%.1f%%", unemp_rate_pct))
      )
    ) |>
      dplyr::mutate(Metric = factor(Metric, levels = c("Labor Force Participation Rate (%)", "Unemployment Rate (%)")))

    ggplot2::ggplot(plot_df, ggplot2::aes(x = region_name, y = Rate, fill = region_name)) +
      ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = Label), vjust = -0.35, size = 3.8, fontface = "bold", color = "#1e293b") +
      ggplot2::facet_wrap(~Metric, ncol = 2, scales = "free_y") +
      ggplot2::scale_fill_manual(values = pal_vec) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(
        title    = paste0("ACS ", res$acs_year, " 5-Year Labor Force Participation & Unemployment Rates (Table S2301)"),
        subtitle = "Civilian noninstitutionalized population 16 years and over",
        x        = NULL,
        y        = "Rate (%)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face = "bold", color = "#1e3a8a", size = 13),
        plot.subtitle      = ggplot2::element_text(color = "#475569", size = 10, margin = ggplot2::margin(b = 10)),
        axis.text.x        = ggplot2::element_text(angle = if (nrow(acs) > 3) 25 else 0, hjust = if (nrow(acs) > 3) 1 else 0.5, face = "bold", color = "#0f172a"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        strip.background   = ggplot2::element_rect(fill = "#f1f5f9", color = "#cbd5e1"),
        strip.text         = ggplot2::element_text(face = "bold", color = "#0f172a", size = 11)
      )
  })

  output$download_acs_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("acs_s2301_full_demographic_profile_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- comparison_results()
      shiny::req(res, res$acs_full)
      out <- res$acs_full |>
        dplyr::mutate(ACS_5Yr_End_Year = res$acs_year, .before = 1) |>
        dplyr::select(
          ACS_5Yr_End_Year,
          Region_ID = region_id,
          Region = region_name,
          S2301_Row = row_id,
          Category,
          Characteristic,
          Population = population,
          Population_MOE = population_moe,
          Est_Labor_Force = est_labor_force,
          Labor_Force_Part_Rate_Pct = lfpr_pct,
          Labor_Force_Part_Rate_MOE = lfpr_moe,
          Emp_Pop_Ratio_Pct = emp_pop_ratio_pct,
          Emp_Pop_Ratio_MOE = emp_pop_ratio_moe,
          Unemployment_Rate_Pct = unemp_rate_pct,
          Unemployment_Rate_MOE = unemp_rate_moe,
          Geographic_Units = n_units,
          Aggregation_Method = aggregation_method
        )
      readr::write_csv(out, file)
    }
  )

  # ----------------------------------------------------------------------------
  # Tab 4: Regional Comparison Bar Charts
  # ----------------------------------------------------------------------------
  output$charts_ui <- shiny::renderUI({
    res <- comparison_results()
    if (is.null(res)) {
      return(shiny::div(
        style = "padding: 40px; text-align: center; color: #64748b;",
        shiny::h4("No Comparison Data Generated Yet"),
        shiny::p("Build your custom regions in Tab 1 and click 'Fetch LODES & ACS Data' to visualize regional comparisons.")
      ))
    }

    meta <- res$reg_meta
    reg_choices <- stats::setNames(as.character(meta$region_id), as.character(meta$region_name))
    cat_choices <- unique(as.character(metric_catalog$category))

    shiny::tagList(
      shiny::div(
        style = "padding: 16px;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px; margin-bottom: 10px;",
          shiny::div(
            shiny::h4("Regional Comparison Bar Charts", style = "margin: 0; color: #1e3a8a; font-weight: 700;"),
            shiny::p(style = "margin: 2px 0 0 0; color: #475569; font-size: 0.9em;",
                     paste0("LODES Year: ", res$year, " | Job Type: ", res$job_type, " | Segment: ", res$segment_label))
          ),
          shiny::downloadButton("download_chart_png", "Download Chart (PNG)", class = "btn-outline-primary btn-sm")
        ),

        shiny::div(
          class = "chart-controls-bar",
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 14px; margin-bottom: 16px;",
          bslib::layout_columns(
            col_widths = c(2, 2, 3, 2, 3),
            shiny::selectInput(
              "chart_dataset",
              "Dataset:",
              choices = c("WAC (Work Location)" = "wac", "RAC (Home Location)" = "rac"),
              selected = "wac"
            ),
            shiny::selectInput(
              "chart_category",
              "Category:",
              choices = cat_choices,
              selected = "Broad Industry"
            ),
            shiny::selectInput(
              "chart_characteristic",
              "Characteristic:",
              choices = c("All in Category (Faceted)" = "ALL"),
              selected = "ALL"
            ),
            shiny::selectInput(
              "chart_metric_type",
              "Plot Values:",
              choices = c("Counts (#)" = "count", "Share of Total (%)" = "share"),
              selected = "count"
            ),
            shiny::selectizeInput(
              "chart_exclude_regions",
              "Exclude Regions:",
              choices = reg_choices,
              selected = character(0),
              multiple = TRUE,
              options = list(placeholder = "None excluded")
            )
          )
        ),

        shiny::div(
          style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px;",
          shiny::uiOutput("comparison_plot_container")
        )
      )
    )
  })

  shiny::observeEvent(list(input$chart_category, input$chart_dataset, comparison_results()), {
    res <- comparison_results()
    shiny::req(res, input$chart_category)

    ds <- if (!is.null(input$chart_dataset) && identical(as.character(input$chart_dataset), "rac")) {
      res$rac_totals
    } else {
      res$wac_totals
    }
    sub_cat <- metric_catalog |> dplyr::filter(category == as.character(input$chart_category))

    active_vars <- vapply(sub_cat$var, function(v) {
      v == "total_jobs" || sum(as.numeric(ds[[v]]), na.rm = TRUE) > 0
    }, logical(1))
    sub_cat <- sub_cat[active_vars, ]

    char_choices <- c(
      "All in Category (Faceted)" = "ALL",
      stats::setNames(as.character(sub_cat$var), as.character(sub_cat$label))
    )
    cur_val <- shiny::isolate(input$chart_characteristic)
    if (is.null(cur_val) || !as.character(cur_val) %in% unname(char_choices)) {
      cur_val <- "ALL"
    }
    shiny::updateSelectInput(session, "chart_characteristic", choices = char_choices, selected = cur_val)
  })

  build_comparison_ggplot <- shiny::reactive({
    res <- comparison_results()
    shiny::req(res, input$chart_dataset, input$chart_category, input$chart_characteristic, input$chart_metric_type)

    ds_choice   <- as.character(input$chart_dataset)[1]
    cat_choice  <- as.character(input$chart_category)[1]
    char_choice <- as.character(input$chart_characteristic)[1]
    type_choice <- as.character(input$chart_metric_type)[1]

    meta <- res$reg_meta
    excl <- input$chart_exclude_regions
    if (!is.null(excl) && length(excl) > 0) {
      meta <- meta |> dplyr::filter(!region_id %in% as.character(excl))
    }
    shiny::validate(shiny::need(nrow(meta) > 0, "All regions are excluded. Remove at least one region from 'Exclude Regions' to display the chart."))

    totals_df <- if (identical(ds_choice, "rac")) res$rac_totals else res$wac_totals
    totals_df <- totals_df |> dplyr::filter(region_id %in% meta$region_id)

    sub_cat <- metric_catalog |> dplyr::filter(category == cat_choice)
    if (!identical(char_choice, "ALL") && char_choice %in% sub_cat$var) {
      sub_cat <- sub_cat |> dplyr::filter(var == char_choice)
    }

    keep_mask <- vapply(sub_cat$var, function(v) {
      v == "total_jobs" || sum(as.numeric(totals_df[[v]]), na.rm = TRUE) > 0
    }, logical(1))
    sub_cat <- sub_cat[keep_mask, ]
    shiny::validate(shiny::need(nrow(sub_cat) > 0, "No non-zero observations available for the selected category and regions."))

    tot_vec <- as.numeric(totals_df$total_jobs)
    plot_rows <- lapply(seq_len(nrow(sub_cat)), function(idx) {
      v   <- as.character(sub_cat$var[idx])
      lbl <- as.character(sub_cat$label[idx])
      cnt <- as.numeric(totals_df[[v]])
      tibble::tibble(
        region_id      = as.character(totals_df$region_id),
        Characteristic = lbl,
        Count          = cnt,
        Share          = ifelse(tot_vec > 0, cnt / tot_vec, 0)
      )
    }) |> dplyr::bind_rows()

    reg_levels  <- as.character(meta$region_name)
    char_levels <- unique(as.character(sub_cat$label))

    plot_df <- plot_rows |>
      dplyr::left_join(meta |> dplyr::select(region_id, region_name, region_color), by = "region_id") |>
      dplyr::mutate(
        region_name    = factor(as.character(region_name), levels = reg_levels),
        Characteristic = factor(as.character(Characteristic), levels = char_levels),
        PlotVal        = if (identical(type_choice, "share")) as.numeric(Share) else as.numeric(Count),
        LabelText      = if (identical(type_choice, "share")) {
          as.character(scales::label_percent(accuracy = 0.1)(Share))
        } else {
          as.character(scales::label_comma(accuracy = 1)(Count))
        }
      )

    pal_vec <- stats::setNames(as.character(meta$region_color), as.character(meta$region_name))
    ds_title <- if (identical(ds_choice, "rac")) {
      "Area Characteristics by Home Location (RAC)"
    } else {
      "Area Characteristics by Work Location (WAC)"
    }
    metric_title <- if (identical(type_choice, "share")) "Share of Total (%)" else "Counts (#)"
    chart_title <- if (identical(char_choice, "ALL")) cat_choice else as.character(sub_cat$label[1])
    sub_title <- if (identical(char_choice, "ALL")) {
      paste0(ds_title, " | Category: ", cat_choice, " | ", metric_title, " (", res$year, ", ", res$job_type, ", ", res$segment_label, ")")
    } else {
      paste0(ds_title, " | ", as.character(sub_cat$label[1]), " | ", metric_title, " (", res$year, ", ", res$job_type, ", ", res$segment_label, ")")
    }

    is_faceted <- (identical(char_choice, "ALL") && nrow(sub_cat) > 1)
    n_facets   <- length(char_levels)
    n_cols     <- if (n_facets <= 2) n_facets else if (n_facets <= 9) 3 else 4

    y_scale <- if (identical(type_choice, "share")) {
      ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.16)))
    } else {
      ggplot2::scale_y_continuous(labels = scales::label_comma(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.16)))
    }

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = region_name, y = PlotVal, fill = region_name)) +
      ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = LabelText),
        vjust = -0.35,
        size = 3.4,
        fontface = "bold",
        color = "#1e293b"
      ) +
      ggplot2::scale_fill_manual(values = pal_vec) +
      y_scale +
      ggplot2::labs(
        title    = as.character(chart_title),
        subtitle = as.character(sub_title),
        x        = NULL,
        y        = as.character(metric_title)
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face = "bold", color = "#1e3a8a", size = 14),
        plot.subtitle      = ggplot2::element_text(color = "#475569", size = 10, margin = ggplot2::margin(b = 10)),
        axis.text.x        = ggplot2::element_text(angle = if (nrow(meta) > 3) 25 else 0, hjust = if (nrow(meta) > 3) 1 else 0.5, face = "bold", color = "#0f172a"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        strip.background   = ggplot2::element_rect(fill = "#f1f5f9", color = "#cbd5e1"),
        strip.text         = ggplot2::element_text(face = "bold", color = "#0f172a", size = 10)
      )

    if (is_faceted) {
      p <- p + ggplot2::facet_wrap(~Characteristic, ncol = n_cols, scales = "free_y")
    }

    list(plot = p, n_facets = n_facets, n_cols = n_cols, is_faceted = is_faceted)
  })

  output$comparison_plot_container <- shiny::renderUI({
    built <- build_comparison_ggplot()
    n_rows <- if (built$is_faceted) ceiling(built$n_facets / built$n_cols) else 1
    plot_h <- max(440, n_rows * 270)
    shiny::plotOutput("comparison_bar_plot", height = paste0(plot_h, "px"), width = "100%")
  })

  output$comparison_bar_plot <- shiny::renderPlot({
    built <- build_comparison_ggplot()
    print(built$plot)
  })

  output$download_chart_png <- shiny::downloadHandler(
    filename = function() {
      cat_slug <- gsub("[^A-Za-z0-9]+", "_", tolower(input$chart_category))
      paste0("lodes_comparison_chart_", input$chart_dataset, "_", cat_slug, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      built <- build_comparison_ggplot()
      n_rows <- if (built$is_faceted) ceiling(built$n_facets / built$n_cols) else 1
      h_in <- max(5.5, n_rows * 3.1)
      ggplot2::ggsave(file, plot = built$plot, width = 12, height = h_in, dpi = 300, bg = "white")
    }
  )

  # ----------------------------------------------------------------------------
  # Executive PDF Report Generator (Map + ACS S2301 + WAC/RAC + OD + Sources)
  # ----------------------------------------------------------------------------
  output$download_pdf_report <- shiny::downloadHandler(
    filename = function() {
      paste0("LODES_ACS_Regional_Comparison_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      res <- comparison_results()
      shiny::req(res)

      shiny::withProgress(message = "Rendering Printable PDF Report...", value = 0.2, {
        work_dir <- get_latex_safe_build_dir()
        on.exit(base::unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

        data_rds <- file.path(work_dir, "report_data.rds")
        base::saveRDS(res, data_rds)

        shiny::incProgress(0.3, detail = "Generating map & summary tables")

        is_landscape <- nrow(res$reg_meta) > 5
        geom_yaml    <- if (is_landscape) 'geometry: "margin=0.5in,landscape"' else "geometry: margin=0.6in"
        map_chunk_hdr <- if (is_landscape) {
          "{r map_and_kpis, fig.width=9.5, fig.height=3.0, fig.align='center'}"
        } else {
          "{r map_and_kpis, fig.width=8, fig.height=3.5, fig.align='center'}"
        }

        bt <- paste0(rep("`", 3), collapse = "")
        rmd_path <- file.path(work_dir, "comparison_report.Rmd")

        rmd_lines <- c(
          "---",
          'title: "Regional Workforce & Labor Market Comparison"',
          'date: "`r format(Sys.Date(), \'%B %d, %Y\')`"',
          "output:",
          "  pdf_document:",
          "    latex_engine: pdflatex",
          geom_yaml,
          "header-includes:",
          "  - \\usepackage{booktabs}",
          "  - \\usepackage{longtable}",
          "  - \\usepackage{array}",
          "  - \\usepackage{graphicx}",
          "  - \\usepackage{float}",
          "  - \\usepackage{xcolor}",
          "  - \\setlength{\\tabcolsep}{4pt}",
          "  - \\renewcommand{\\arraystretch}{1.3}",
          "---",
          "",
          paste0(bt, "{r setup, include=FALSE}"),
          "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
          'res <- readRDS("report_data.rds")',
          "meta <- res$reg_meta",
          "is_land <- nrow(meta) > 5",
          "",
          "render_wrapped_longtable <- function(df, cat_width = 1.05) {",
          "  n_reg <- ncol(df) - 1",
          "  page_w <- if (is_land) 9.8 else 7.15",
          "  usable_w <- page_w - cat_width - (n_reg + 1) * 0.115",
          "  reg_w <- round(max(0.95, usable_w / max(1, n_reg)), 2)",
          "  col_spec <- paste0(",
          '    ">{\\\\raggedright\\\\arraybackslash}p{", cat_width, "in}",',
          '    paste(rep(paste0(">{\\\\raggedright\\\\arraybackslash}p{", reg_w, "in}"), n_reg), collapse = "")',
          "  )",
          '  tex <- knitr::kable(df, format = "latex", booktabs = TRUE, longtable = TRUE)',
          '  tex <- gsub("LINEBREAKHERE", " \\\\newline ", tex, fixed = TRUE)',
          "  tex_lines <- strsplit(tex, '\\n', fixed = TRUE)[[1]]",
          "  lt_idx <- which(startsWith(tex_lines, '\\\\begin{longtable}'))",
          "  if (length(lt_idx) > 0) {",
          "    tex_lines[lt_idx[1]] <- paste0('\\\\begin{longtable}[l]{', col_spec, '}')",
          "  }",
          "  tex <- paste(tex_lines, collapse = '\\n')",
          '  cat("\\\\footnotesize\\n", tex, "\\n\\\\normalsize\\n")',
          "}",
          bt,
          "",
          "**Parameters:** LODES Year `r res$year` | ACS 5-Year `r res$acs_year` | Job Type: `r res$job_type` | Workforce Segment: `r res$segment_label` | Overlap Mode: `r ifelse(res$exclude_overlap, 'Higher-priority regions excluded from lower regions', 'Full selected boundaries included')`",
          "",
          "## 1. Custom Regions Map & LODES Headline Totals",
          "",
          paste0(bt, map_chunk_hdr),
          "polys_plot <- res$regions_poly |> dplyr::arrange(priority)",
          "polys_plot$region_name <- factor(polys_plot$region_name, levels = meta$region_name)",
          "pal_map <- stats::setNames(meta$region_color, meta$region_name)",
          "",
          "p <- ggplot2::ggplot() +",
          '  ggplot2::geom_sf(data = res$base_parts_sf, fill = "#f8fafc", color = "#cbd5e1", linewidth = 0.25)',
          "for (idx in rev(seq_len(nrow(polys_plot)))) {",
          '  p <- p + ggplot2::geom_sf(data = polys_plot[idx, ], ggplot2::aes(fill = region_name), color = "#0f172a", linewidth = 0.45, alpha = 0.72)',
          "}",
          'p + ggplot2::scale_fill_manual(values = pal_map, name = "Custom Regions") +',
          "  ggplot2::theme_void() +",
          "  ggplot2::theme(",
          '    legend.position = "right",',
          '    legend.title = ggplot2::element_text(face = "bold", size = 10),',
          "    legend.text = ggplot2::element_text(size = 9),",
          "    plot.margin = ggplot2::margin(4, 4, 4, 4)",
          "  )",
          bt,
          "",
          paste0(bt, "{r kpi_table}"),
          "kpi_df <- tibble::tibble(",
          "  Region = meta$region_name,",
          "  `Jobs by Work Location` = res$wac_totals$total_jobs,",
          "  `Workers by Home Location` = res$rac_totals$total_jobs",
          ") |>",
          "  dplyr::mutate(",
          "    `Jobs-to-Workers Ratio` = ifelse(`Workers by Home Location` > 0, round(`Jobs by Work Location` / `Workers by Home Location`, 2), NA_real_),",
          "    `Jobs by Work Location` = scales::comma(`Jobs by Work Location`),",
          "    `Workers by Home Location` = scales::comma(`Workers by Home Location`)",
          "  )",
          'knitr::kable(kpi_df, booktabs = TRUE, align = c("l", "r", "r", "r"))',
          bt,
          "",
          "## 2. ACS 5-Year Labor Market Profile (Table S2301)",
          "",
          "*Civilian noninstitutionalized population 16 years and over, labor force participation rate, and unemployment rate from the `r res$acs_year` 5-Year American Community Survey.*",
          "",
          paste0(bt, "{r acs_s2301_table, results='asis'}"),
          "if (is.null(res$acs_summary) || nrow(res$acs_summary) == 0) {",
          '  cat("*ACS Table S2301 estimates unavailable for this run.*\\n")',
          "} else {",
          "  acs_tbl <- res$acs_summary |>",
          "    dplyr::transmute(",
          "      Region = region_name,",
          "      `Population 16+` = scales::comma(population),",
          "      `Est. Labor Force` = scales::comma(est_labor_force),",
          '      `Labor Force Part. Rate` = ifelse(is.na(lfpr_pct), "—", sprintf("%.1f%%", lfpr_pct)),',
          '      `Emp.-Pop. Ratio` = ifelse(is.na(emp_pop_ratio_pct), "—", sprintf("%.1f%%", emp_pop_ratio_pct)),',
          '      `Unemployment Rate` = ifelse(is.na(unemp_rate_pct), "—", sprintf("%.1f%%", unemp_rate_pct)),',
          "      `Source Method` = aggregation_method",
          "    )",
          '  acs_tex <- knitr::kable(acs_tbl, format = "latex", booktabs = TRUE, align = c("l", "r", "r", "r", "r", "r", "l"))',
          '  cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", acs_tex, "\\n}\\n\\\\end{table}\\n")',
          "}",
          bt,
          "",
          "## 3. Area Characteristics by Work Location (WAC)",
          "",
          "*Demographic and industry breakdown of jobs located inside each region (values sorted descending within each category).*",
          "",
          paste0(bt, "{r wac_highlights, results='asis'}"),
          "wac_hi <- summarize_top_groups(res$wac_totals, meta) |>",
          "  tidyr::pivot_wider(names_from = Region, values_from = Highlight)",
          "render_wrapped_longtable(wac_hi)",
          bt,
          "",
          "*Notes: Categories showing a subset display the largest non-zero groups in descending order: Top 2 groups for Age, Education, and Race; and Top 3 sectors for 2-digit NAICS codes (see Section 6 for sector titles). Active categories are shown for Earnings, Broad Industry, and Sex. Ethnicity displays the Hispanic or Latino share. Monthly earnings tiers follow LEHD LODES definitions: Very Low = $1,250/month or less; Low = $1,251 to $3,333/month; Average = greater than $3,333/month.*",
          "",
          "## 4. Area Characteristics by Home Location (RAC)",
          "",
          "*Demographic and industry breakdown of employed residents living inside each region (values sorted descending within each category).*",
          "",
          paste0(bt, "{r rac_highlights, results='asis'}"),
          "rac_hi <- summarize_top_groups(res$rac_totals, meta) |>",
          "  tidyr::pivot_wider(names_from = Region, values_from = Highlight)",
          "render_wrapped_longtable(rac_hi)",
          bt,
          "",
          "*Notes: Categories showing a subset display the largest non-zero groups in descending order: Top 2 groups for Age, Education, and Race; and Top 3 sectors for 2-digit NAICS codes (see Section 6 for sector titles). Active categories are shown for Earnings, Broad Industry, and Sex. Ethnicity displays the Hispanic or Latino share. Monthly earnings tiers follow LEHD LODES definitions: Very Low = $1,250/month or less; Low = $1,251 to $3,333/month; Average = greater than $3,333/month.*",
          "",
          "## 5. Commuting Flows Between Regions (Origin-Destination)",
          "",
          "*Origin (rows) = worker home location; Destination (columns) = worker work location (All Workers S000).*",
          "",
          paste0(bt, "{r od_matrix, results='asis'}"),
          "od <- res$od_flows",
          'all_ids   <- c(meta$region_id, "OUTSIDE")',
          'all_names <- c(meta$region_name, "Outside Selected Regions")',
          "od_mat <- lapply(seq_len(nrow(meta)), function(i) {",
          "  oid <- meta$region_id[i]",
          "  sub_od <- od |> dplyr::filter(origin_id == oid)",
          "  tot_res <- sum(sub_od$Total_Jobs, na.rm = TRUE)",
          "  row_vals <- list(`Home Origin` = meta$region_name[i], `Total Resident Workers` = scales::comma(tot_res))",
          "  for (j in seq_along(all_ids)) {",
          "    did <- all_ids[j]",
          "    v <- sub_od$Total_Jobs[sub_od$dest_id == did]",
          "    v <- if (length(v) == 0) 0 else v[1]",
          '    pct <- if (tot_res > 0) scales::percent(v / tot_res, accuracy = 0.1) else "0.0%"',
          '    row_vals[[all_names[j]]] <- paste0(scales::comma(v), " (", pct, ")")',
          "  }",
          "  tibble::as_tibble(row_vals)",
          "}) |> dplyr::bind_rows()",
          'od_tex <- knitr::kable(od_mat, format = "latex", booktabs = TRUE, align = c("l", rep("r", ncol(od_mat) - 1)))',
          'cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", od_tex, "\\n}\\n\\\\end{table}\\n")',
          bt,
          "",
          "### Top Inter-Regional & Internal Commuting Corridors",
          "",
          paste0(bt, "{r od_top_pairs, results='asis'}"),
          "filtered_od <- od |>",
          '  dplyr::filter(origin_id != "OUTSIDE" | dest_id != "OUTSIDE") |>',
          "  dplyr::arrange(dplyr::desc(Total_Jobs))",
          "total_corridors <- nrow(filtered_od)",
          "top_od <- filtered_od |>",
          "  dplyr::slice_head(n = 20) |>",
          "  dplyr::transmute(",
          "    `Home Origin` = Origin,",
          "    `Work Destination` = Destination,",
          "    `Commuters` = scales::comma(Total_Jobs),",
          "    `Origin %` = scales::percent(Share_of_Origin_Workers, accuracy = 0.1),",
          "    `Dest. %` = scales::percent(Share_of_Dest_Jobs, accuracy = 0.1),",
          "    `Goods Producing` = scales::comma(Ind_Goods),",
          "    `Trade & Transp.` = scales::comma(Ind_Trade),",
          "    `Services` = scales::comma(Ind_Services)",
          "  )",
          'cor_tex <- knitr::kable(top_od, format = "latex", booktabs = TRUE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))',
          "cor_lines <- strsplit(cor_tex, '\\n', fixed = TRUE)[[1]]",
          "tab_idx <- which(startsWith(cor_lines, '\\\\begin{tabular}'))",
          "if (length(tab_idx) > 0) {",
          "  cor_lines[tab_idx[1]] <- '\\\\begin{tabular}[t]{>{\\\\raggedright\\\\arraybackslash}p{1.2in}>{\\\\raggedright\\\\arraybackslash}p{1.2in}rrrrrr}'",
          "}",
          "cor_tex <- paste(cor_lines, collapse = '\\n')",
          'cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", cor_tex, "\\n}\\n\\\\end{table}\\n")',
          "if (total_corridors > 20) {",
          '  cat(sprintf("\\n*Note: Showing the top 20 commuting corridors out of %s total available origin-destination pairs.*\\n", scales::comma(total_corridors)))',
          "}",
          bt,
          "",
          "## 6. Data Sources & Definitions",
          "",
          "- **American Community Survey (ACS) 5-Year Estimates — Subject Table S2301 (Employment Status):** Provides civilian noninstitutionalized population aged 16 years and over, labor force participation rates, employment-to-population ratios, and unemployment rates via the U.S. Census Bureau API (`tidycensus`). Multi-unit custom regions aggregate population 16+ directly and compute population-weighted labor force participation rates and labor-force-weighted unemployment rates.",
          "- **U.S. Census Bureau Longitudinal Employer-Household Dynamics (LEHD) Origin-Destination Employment Statistics (LODES, Version 8):**",
          "  - **Work Location Characteristics (WAC — Workplace Area Characteristics):** Summarizes employment by the Census block where workers are employed across worker age, monthly earnings, educational attainment, race, ethnicity, sex, and 2-digit NAICS industry sector.",
          "  - **Home Location Characteristics (RAC — Residence Area Characteristics):** Summarizes employed workers by the Census block where they reside across the same demographic and industry dimensions.",
          "  - **Commuting Flows (OD — Origin-Destination):** Links workers' home Census blocks (origin) to their employment Census blocks (destination) for both within-state (`od_main`) and cross-state (`od_aux`) commuters.",
          "- **Program & Data Links:**",
          "  - American Community Survey (ACS) Tables: https://data.census.gov/",
          "  - LEHD Program Overview: https://lehd.ces.census.gov/",
          "  - LODES Data Repository & Technical Documentation: https://lehd.ces.census.gov/data/",
          "",
          "### 2-Digit NAICS Industry Sector Reference",
          "",
          paste0(bt, "{r naics_ref_table, results='asis'}"),
          "naics_df <- metric_catalog |>",
          '  dplyr::filter(category == "2-Digit NAICS") |>',
          "  dplyr::mutate(",
          '    Code = sub(":.*$", "", label),',
          '    Sector = sub("^[^:]+:\\\\s*", "", label),',
          "    Broad = dplyr::case_when(",
          '      var %in% c("cns01", "cns02", "cns03", "cns04", "cns05") ~ "Goods Producing",',
          '      var %in% c("cns06", "cns07", "cns08") ~ "Trade & Transp.",',
          '      TRUE ~ "Services"',
          "    )",
          "  ) |>",
          "  dplyr::select(Code, Sector, Broad)",
          "naics_wide <- dplyr::bind_cols(naics_df[1:10, ], naics_df[11:20, ])",
          'names(naics_wide) <- c("NAICS Code", "Industry Sector", "Broad Sector", "NAICS Code ", "Industry Sector ", "Broad Sector ")',
          'n_tex <- knitr::kable(naics_wide, format = "latex", booktabs = TRUE, align = c("l", "l", "l", "l", "l", "l"))',
          'cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", n_tex, "\\n}\\n\\\\end{table}\\n")',
          bt
        )
        base::writeLines(rmd_lines, rmd_path)

        shiny::incProgress(0.4, detail = "Compiling PDF via LaTeX")

        render_env <- new.env(parent = globalenv())
        render_env$summarize_top_groups <- summarize_top_groups
        render_env$clean_pdf_label <- clean_pdf_label
        render_env$metric_catalog <- metric_catalog

        base::tryCatch({
          rendered_pdf <- rmarkdown::render(
            input = rmd_path,
            output_format = "pdf_document",
            output_file = "comparison_report.pdf",
            output_dir = work_dir,
            intermediates_dir = work_dir,
            knit_root_dir = work_dir,
            envir = render_env,
            quiet = TRUE
          )
          base::file.copy(rendered_pdf, file, overwrite = TRUE)
        }, error = function(e) {
          log_file <- file.path(work_dir, "comparison_report.log")
          log_hint <- ""
          if (file.exists(log_file)) {
            log_lines <- base::readLines(log_file, warn = FALSE)
            err_idx <- grep("^!|Error|not found|Permission", log_lines)
            if (length(err_idx) > 0) {
              keep_idx <- unique(pmin(length(log_lines), c(err_idx, err_idx + 1)))
              log_hint <- paste(" | Log:", paste(utils::head(log_lines[keep_idx], 4), collapse = " ; "))
            }
          }
          message("PDF Render Error: ", e$message, log_hint)
          shiny::showNotification(
            paste0("PDF compilation failed: ", e$message, log_hint),
            type = "error",
            duration = 15
          )
        })
      })
    }
  )

  # ----------------------------------------------------------------------------
  # Tab 5: Regional Profile Map (RAC & WAC)
  # ----------------------------------------------------------------------------
  output$profile_map <- mapgl::renderMaplibre({
    res <- comparison_results()
    shiny::req(res)

    polys <- res$regions_poly
    meta  <- res$reg_meta

    popups <- vapply(polys$region_id, function(rid) {
      rname <- meta$region_name[meta$region_id == rid]
      build_region_popup(
        rname,
        res$wac_totals |> dplyr::filter(region_id == rid),
        res$rac_totals |> dplyr::filter(region_id == rid),
        res$segment_label
      )
    }, character(1))

    polys$popup_html <- popups

    m <- mapgl::maplibre(style = mapgl::carto_style("positron"), bounds = as.numeric(sf::st_bbox(polys)))

    for (i in seq_len(nrow(polys))) {
      sub_poly <- polys[i, ]
      rid <- sub_poly$region_id
      m <- m |>
        mapgl::add_fill_layer(
          id = paste0("profile_fill_", rid),
          source = sub_poly,
          fill_color = sub_poly$region_color,
          fill_opacity = 0.6,
          popup = "popup_html",
          tooltip = "region_name",
          hover_options = list(fill_opacity = 0.85)
        ) |>
        mapgl::add_line_layer(
          id = paste0("profile_line_", rid),
          source = sub_poly,
          line_color = "#0f172a",
          line_width = 2
        )
    }

    m |>
      mapgl::add_legend(
        legend_title = "Custom Regions (Click for RAC/WAC Profile)",
        values = unname(meta$region_name),
        colors = unname(meta$region_color),
        type = "categorical",
        position = "bottom-right"
      ) |>
      mapgl::add_navigation_control(position = "top-right") |>
      mapgl::add_fullscreen_control(position = "top-right") |>
      mapgl::add_scale_control(position = "bottom-left")
  })

  # ----------------------------------------------------------------------------
  # Tab 6: Origin-Destination (OD) Commuting Flows
  # ----------------------------------------------------------------------------
  output$od_ui <- shiny::renderUI({
    res <- comparison_results()
    if (is.null(res)) {
      return(shiny::div(
        style = "padding: 40px; text-align: center; color: #64748b;",
        shiny::h4("No Commuting Data Generated Yet"),
        shiny::p("Build your custom regions in Tab 1 and click 'Fetch LODES & ACS Data' to analyze origin-destination worker flows.")
      ))
    }

    shiny::tagList(
      shiny::div(
        style = "padding: 16px;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; margin-bottom: 12px;",
          shiny::div(
            shiny::h4("Origin-Destination (OD) Commuting Flows", style = "margin: 0; color: #1e3a8a; font-weight: 700;"),
            shiny::p(style = "margin: 2px 0 0 0; color: #475569; font-size: 0.92em;",
                     paste0("LODES Year: ", res$year, " | Job Type: ", res$job_type,
                            " (Note: LODES OD files cover all workers S000 broken out by age, earnings, and broad industry)"))
          ),
          shiny::downloadButton("download_od_csv", "Download OD Flows CSV", class = "btn-outline-primary btn-sm")
        ),

        shiny::h5("Commuting Origin-Destination Matrix (Residence → Workplace)", style = "color: #0284c7; font-weight: 700;"),
        shiny::p(class = "text-muted", style = "font-size: 0.85em;",
                 "Rows represent where workers live (Origin); columns represent where they work (Destination)."),
        DT::DTOutput("od_matrix_table"),

        shiny::hr(),

        shiny::h5("Detailed Pairwise Commuting Flows by Worker & Industry Group", style = "color: #059669; font-weight: 700;"),
        DT::DTOutput("od_detail_table")
      )
    )
  })

  output$od_matrix_table <- DT::renderDT({
    res <- comparison_results()
    shiny::req(res)
    meta <- res$reg_meta
    od   <- res$od_flows

    all_ids   <- c(meta$region_id, "OUTSIDE")
    all_names <- c(meta$region_name, "Outside Selected Regions")
    n_dest    <- length(all_ids)

    matrix_rows <- lapply(seq_len(nrow(meta)), function(i) {
      oid   <- meta$region_id[i]
      oname <- meta$region_name[i]
      sub_od <- od |> dplyr::filter(origin_id == oid)
      tot_res <- sum(sub_od$Total_Jobs, na.rm = TRUE)

      row_list <- list(Origin = oname, Total_Resident_Workers = tot_res)

      for (j in seq_along(all_ids)) {
        did <- all_ids[j]
        val <- sub_od$Total_Jobs[sub_od$dest_id == did]
        val <- if (length(val) == 0) 0 else val[1]
        row_list[[paste0("Cnt_", did)]] <- val
      }
      for (j in seq_along(all_ids)) {
        did <- all_ids[j]
        val <- row_list[[paste0("Cnt_", did)]]
        row_list[[paste0("Shr_", did)]] <- if (tot_res > 0) val / tot_res else 0
      }

      tibble::as_tibble(row_list)
    }) |> dplyr::bind_rows()

    sketch <- htmltools::tags$table(
      class = "display",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th(rowspan = 2, "Residence Origin"),
          htmltools::tags$th(rowspan = 2, class = "dt-right", "Total Resident Workers"),
          htmltools::tags$th(colspan = n_dest, class = "group-header-counts section-divider-left", "Workplace Destination — Counts (#)"),
          htmltools::tags$th(colspan = n_dest, class = "group-header-shares section-divider-left", "Workplace Destination — Share of Origin Workers (%)")
        ),
        htmltools::tags$tr(
          lapply(seq_along(all_names), function(i) {
            cls <- if (i == 1) "section-divider-left dt-right" else "dt-right"
            htmltools::tags$th(class = cls, all_names[i])
          }),
          lapply(seq_along(all_names), function(i) {
            cls <- if (i == 1) "section-divider-left share-col-tint dt-right" else "share-col-tint dt-right"
            htmltools::tags$th(class = cls, all_names[i])
          })
        )
      )
    )

    cnt_cols <- c("Total_Resident_Workers", grep("^Cnt_", names(matrix_rows), value = TRUE))
    shr_cols <- grep("^Shr_", names(matrix_rows), value = TRUE)

    first_cnt_idx   <- 2
    rest_cnt_idx    <- if (n_dest > 1) seq(first_cnt_idx + 1, 1 + n_dest) else integer(0)
    first_share_idx <- 2 + n_dest
    rest_share_idx  <- if (n_dest > 1) seq(first_share_idx + 1, 1 + 2 * n_dest) else integer(0)

    col_defs <- list(
      list(className = "dt-right", targets = 1),
      list(className = "section-divider-left dt-right", targets = first_cnt_idx),
      list(className = "section-divider-left share-col-tint dt-right", targets = first_share_idx)
    )
    if (length(rest_cnt_idx) > 0) {
      col_defs[[length(col_defs) + 1]] <- list(className = "dt-right", targets = rest_cnt_idx)
    }
    if (length(rest_share_idx) > 0) {
      col_defs[[length(col_defs) + 1]] <- list(className = "share-col-tint dt-right", targets = rest_share_idx)
    }

    DT::datatable(
      matrix_rows,
      container = sketch,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE,
        columnDefs = col_defs
      )
    ) |>
      DT::formatCurrency(cnt_cols, currency = "", digits = 0) |>
      DT::formatPercentage(shr_cols, digits = 1)
  })

  output$od_detail_table <- DT::renderDT({
    res <- comparison_results()
    shiny::req(res)

    df <- res$od_flows |>
      dplyr::select(
        Origin, Destination, Total_Jobs,
        Share_of_Origin_Workers, Share_of_Dest_Jobs,
        Age_Under_30, Age_30_to_54, Age_55_Plus,
        Earn_Low, Earn_Mid, Earn_High,
        Ind_Goods, Ind_Trade, Ind_Services
      ) |>
      dplyr::arrange(dplyr::desc(Total_Jobs))

    col_names <- c(
      "Origin (Residence)", "Destination (Workplace)", "Total Commuters",
      "Share of Origin (%)", "Share of Dest. (%)",
      "Age < 30", "Age 30-54", "Age 55+",
      "Earn <= $1,250", "Earn $1,250-$3,333", "Earn > $3,333",
      "Goods Producing", "Trade & Transp.", "Services"
    )

    num_cols <- c(
      "Total_Jobs", "Age_Under_30", "Age_30_to_54", "Age_55_Plus",
      "Earn_Low", "Earn_Mid", "Earn_High",
      "Ind_Goods", "Ind_Trade", "Ind_Services"
    )

    DT::datatable(
      df,
      colnames = col_names,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        columnDefs = list(
          list(className = "dt-right", targets = 2:13)
        )
      )
    ) |>
      DT::formatCurrency(num_cols, currency = "", digits = 0) |>
      DT::formatPercentage(c("Share_of_Origin_Workers", "Share_of_Dest_Jobs"), digits = 1)
  })

  output$download_od_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("lodes_od_commuting_flows_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- comparison_results()
      shiny::req(res)
      out <- res$od_flows |>
        dplyr::select(
          Origin, Destination, Total_Jobs,
          Share_of_Origin_Workers, Share_of_Dest_Jobs,
          Age_Under_30, Age_30_to_54, Age_55_Plus,
          Earn_Low, Earn_Mid, Earn_High,
          Ind_Goods, Ind_Trade, Ind_Services
        ) |>
        dplyr::arrange(dplyr::desc(Total_Jobs))
      readr::write_csv(out, file)
    }
  )
}

shiny::shinyApp(ui, server)
