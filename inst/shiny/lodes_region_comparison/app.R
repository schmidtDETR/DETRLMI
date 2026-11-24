# ==============================================================================
# LODES Dynamic Multi-Region Builder & RAC/WAC/OD Comparator
# (with JSON Region Import/Export, Standalone R Script Export & PDF Report)
#
# Required Packages:
# shiny, bslib, mapgl, tigris, sf, dplyr, readr, scales, DT, htmltools,
# ggplot2, rmarkdown, knitr, jsonlite
# ==============================================================================

library(shiny)
library(bslib)
library(mapgl)
library(tigris)
library(sf)
library(dplyr)
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
# Cache, PDF Build Directory & Static Dictionaries
# ------------------------------------------------------------------------------
get_data_cache_dir <- function() {
  dir_path <- tools::R_user_dir("lodes_region_compare_cache", which = "cache")
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  dir_path
}
cache_dir <- get_data_cache_dir()

# Controlled folder for PDF compilation
pdf_base_dir <- "S:/ra/RADUMP/pdf_tmp"

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

state_choices <- setNames(
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
# Spatial & LODES Helpers
# ------------------------------------------------------------------------------
dissolve_quietly <- function(sf_obj) {
  suppressMessages(suppressWarnings(
    sf_obj |>
      sf::st_transform(5070) |>
      sf::st_make_valid() |>
      sf::st_buffer(0) |>
      sf::st_union() |>
      sf::st_transform(4326)
  ))
}

difference_quietly <- function(geom_a, geom_b) {
  suppressMessages(suppressWarnings(
    sf::st_difference(
      sf::st_transform(geom_a, 5070),
      sf::st_transform(geom_b, 5070)
    ) |>
      sf::st_transform(4326)
  ))
}

union_quietly <- function(geom_a, geom_b) {
  suppressMessages(suppressWarnings(
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
        st_shp <- do.call(tigris::states, args)
        st_shp[st_shp$STUSPS == state_id, ]
      },
      "Counties"                          = do.call(tigris::counties, args),
      "Tracts"                            = do.call(tigris::tracts, args),
      "Block Groups"                      = do.call(tigris::block_groups, args),
      "PUMAs"                             = do.call(tigris::pumas, args),
      "ZCTAs"                             = do.call(tigris::zctas, args),
      "Core-based Areas"                  = do.call(tigris::core_based_statistical_areas, args),
      "County Subdivisions"               = do.call(tigris::county_subdivisions, args),
      "Census Places"                     = do.call(tigris::places, args),
      "School Districts"                  = do.call(tigris::school_districts, args),
      "Lower State Legislative Districts" = do.call(tigris::state_legislative_districts, args),
      "Upper State Legislative Districts" = do.call(tigris::state_legislative_districts, args),
      "Urban Areas"                       = do.call(tigris::urban_areas, args),
      "Voting Districts"                  = do.call(tigris::voting_districts, args)
    )
    
    national_geoms <- c("ZCTAs", "Core-based Areas", "Urban Areas")
    if (geom_type %in% national_geoms) {
      state_args <- list(cb = use_cb, progress_bar = FALSE)
      if (!is.null(target_year)) state_args$year <- target_year
      state_boundary <- do.call(tigris::states, state_args)
      state_boundary <- state_boundary[state_boundary$STUSPS == state_id, ]
      state_boundary <- sf::st_transform(state_boundary, sf::st_crs(raw_shape))
      
      intersects <- suppressMessages(sf::st_intersects(raw_shape, state_boundary, sparse = FALSE)[, 1])
      touches    <- suppressMessages(sf::st_touches(raw_shape, state_boundary, sparse = FALSE)[, 1])
      raw_shape  <- raw_shape[intersects & !touches, ]
    }
    raw_shape
  }
  
  tryCatch(call_tigris(TRUE), error = function(e1) {
    tryCatch(call_tigris(TRUE, 2020), error = function(e2) {
      tryCatch(call_tigris(FALSE), error = function(e3) {
        call_tigris(FALSE, 2020)
      })
    })
  })
}

build_standardized_layer <- function(geom_type, state_id) {
  raw_data <- fetch_geo_robustly(geom_type, state_id)
  possible_ids <- c("GEOID", "GEOID20", "GEOID10", "ZCTA5CE20", "ZCTA5CE10")
  id_col <- intersect(possible_ids, names(raw_data))[1]
  if (is.na(id_col)) id_col <- grep("^GEOID|^ZCTA", names(raw_data), value = TRUE)[1]
  
  name_col <- intersect(c("NAMELSAD", "NAME", "NAMELSAD20", "NAME20"), names(raw_data))[1]
  
  raw_data |>
    mutate(
      MAP_GEOID  = as.character(.data[[id_col]]),
      AREA_LABEL = if (!is.na(name_col)) paste0(.data[[name_col]], " (", MAP_GEOID, ")") else MAP_GEOID,
      UNIQUE_KEY = paste(state_id, geom_type, MAP_GEOID, sep = "::"),
      SRC_STATE  = as.character(state_id),
      SRC_GEOM   = as.character(geom_type)
    ) |>
    select(UNIQUE_KEY, MAP_GEOID, AREA_LABEL, SRC_STATE, SRC_GEOM) |>
    st_transform(4326) |>
    st_make_valid()
}

summarize_lodes_blocks <- function(df, geocode_col) {
  df |>
    rename(tabblk2020 = !!sym(geocode_col)) |>
    group_by(tabblk2020) |>
    summarise(
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
    row   <- totals_df |> filter(region_id == rid)
    tot   <- row$total_jobs[1]
    
    demo_rows <- lapply(demo_cats, function(cat_name) {
      sub_cat <- metric_catalog |> filter(category == cat_name)
      vals <- vapply(sub_cat$var, function(v) row[[v]][1], numeric(1))
      
      if (cat_name == "Ethnicity") {
        sel_idx <- which(sub_cat$var == "eth_hispanic")
      } else {
        pos_idx <- which(vals > 0)
        if (length(pos_idx) == 0) {
          return(tibble(Region = rname, Category = cat_name, Highlight = "—"))
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
      
      tibble(
        Region    = rname,
        Category  = cat_name,
        Highlight = paste(item_strs, collapse = "LINEBREAKHERE")
      )
    }) |> bind_rows()
    
    naics_cat <- metric_catalog |> filter(category == "2-Digit NAICS")
    n_vals <- vapply(naics_cat$var, function(v) row[[v]][1], numeric(1))
    pos_naics <- which(n_vals > 0)
    
    if (length(pos_naics) == 0) {
      naics_row <- tibble(Region = rname, Category = "Top NAICS Codes", Highlight = "—")
    } else {
      top3_idx <- pos_naics[order(n_vals[pos_naics], decreasing = TRUE)][1:min(3, length(pos_naics))]
      top3_strs <- vapply(top3_idx, function(idx) {
        val <- n_vals[idx]
        pct <- if (tot > 0) scales::percent(val / tot, accuracy = 0.1) else "0.0%"
        naics_code_only <- sub(":.*$", "", naics_cat$label[idx])
        paste0(naics_code_only, " (", scales::comma(val), " | ", pct, ")")
      }, character(1))
      
      naics_row <- tibble(
        Region    = rname,
        Category  = "Top NAICS Codes",
        Highlight = paste(top3_strs, collapse = "LINEBREAKHERE")
      )
    }
    
    bind_rows(demo_rows, naics_row)
  }) |> bind_rows()
}

# Helper to generate a standalone, self-contained R script for the active regions
generate_standalone_r_script <- function(regs, year_val, jt_val, seg_val, exclude_overlap, pdf_dir) {
  reg_entries <- vapply(regs, function(r) {
    r_name <- if (nzchar(trimws(r$name))) trimws(r$name) else r$id
    if (is.null(r$sf) || nrow(r$sf) == 0) {
      areas_code <- "tibble::tibble(SRC_STATE = character(), SRC_GEOM = character(), MAP_GEOID = character(), AREA_LABEL = character())"
    } else {
      df_sub <- r$sf |>
        sf::st_drop_geometry() |>
        select(SRC_STATE, SRC_GEOM, MAP_GEOID, AREA_LABEL)
      areas_code <- paste(capture.output(dput(df_sub)), collapse = "\n    ")
    }
    paste0(
      "  ", r$id, " = list(\n",
      "    id    = ", deparse(r$id), ",\n",
      "    name  = ", deparse(r_name), ",\n",
      "    color = ", deparse(r$color), ",\n",
      "    areas = ", areas_code, "\n",
      "  )"
    )
  }, character(1))
  
  regions_def_code <- paste0("regions_def <- list(\n", paste(reg_entries, collapse = ",\n"), "\n)")
  
  script_lines <- c(
    "# ==============================================================================",
    "# Standalone LODES Multi-Region RAC / WAC / OD Analysis & PDF Report Generator",
    paste0("# Generated on ", format(Sys.Date(), "%Y-%m-%d"), " from LODES Custom Region Builder"),
    "#",
    "# Run this script directly in R (source('...')) to refresh the CSVs and PDF",
    "# without opening the Shiny interface.",
    "# ==============================================================================",
    "",
    "library(tigris)",
    "library(sf)",
    "library(dplyr)",
    "library(readr)",
    "library(tidyr)",
    "library(scales)",
    "library(ggplot2)",
    "library(rmarkdown)",
    "library(knitr)",
    "",
    "options(tigris_use_cache = TRUE)",
    "options(timeout = 300)",
    "sf::sf_use_s2(FALSE)",
    "",
    "# ------------------------------------------------------------------------------",
    "# 1. Analysis Parameters & Output Paths (Edit here to refresh for new years)",
    "# ------------------------------------------------------------------------------",
    paste0("year_val        <- ", as.integer(year_val)),
    paste0("jt_val          <- ", deparse(as.character(jt_val)), "  # JT00=All, JT01=Primary, JT02=Private, JT03=Private Primary"),
    paste0("seg_val         <- ", deparse(as.character(seg_val)), "  # S000=All Workers, SA01-03=Age, SE01-03=Earnings, SI01-03=Industry"),
    paste0("exclude_overlap <- ", ifelse(isTRUE(exclude_overlap), "TRUE", "FALSE")),
    "",
    paste0("pdf_base_dir    <- ", deparse(pdf_dir)),
    'output_dir      <- getwd()  # Folder where final CSVs and PDF report will be saved',
    "",
    "# ------------------------------------------------------------------------------",
    "# 2. Saved Custom Region Definitions",
    "# ------------------------------------------------------------------------------",
    regions_def_code,
    "",
    "# ------------------------------------------------------------------------------",
    "# 3. Static Dictionaries & Helper Functions",
    "# ------------------------------------------------------------------------------",
    "segment_labels <- c(",
    '  S000 = "All Workers", SA01 = "Age: 29 or Younger", SA02 = "Age: 30 to 54",',
    '  SA03 = "Age: 55 and Older", SE01 = "Earnings: $1,250/month or Less",',
    '  SE02 = "Earnings: $1,251 to $3,333/month", SE03 = "Earnings: Average (> $3,333/month)",',
    '  SI01 = "Industry: Goods Producing", SI02 = "Industry: Trade, Transportation & Utilities",',
    '  SI03 = "Industry: All Other Services"',
    ")",
    "job_type_labels <- c(",
    '  JT00 = "All Jobs", JT01 = "Primary Jobs", JT02 = "All Private Jobs",',
    '  JT03 = "Private Primary Jobs", JT04 = "All Federal Jobs", JT05 = "Federal Primary Jobs"',
    ")",
    "seg_lbl <- ifelse(seg_val %in% names(segment_labels), segment_labels[seg_val], seg_val)",
    "jt_lbl  <- ifelse(jt_val %in% names(job_type_labels), job_type_labels[jt_val], jt_val)",
    "",
    "metric_catalog <- tibble::tribble(",
    '  ~category,            ~var,               ~label,',
    '  "Totals",             "total_jobs",       "Total Segment Jobs / Workers",',
    '  "Age",                "age_under_30",     "Under 30",',
    '  "Age",                "age_30_to_54",     "30 to 54",',
    '  "Age",                "age_over_55",      "55 and Older",',
    '  "Earnings",           "earn_low",         "Very Low ($1,250 or less/mo)",',
    '  "Earnings",           "earn_mid",         "Low ($1,250 to $3,333/mo)",',
    '  "Earnings",           "earn_high",        "Average (> $3,333/mo)",',
    '  "Education",          "edu_less_hs",      "Less than High School",',
    '  "Education",          "edu_hs",           "High School or Equivalent",',
    '  "Education",          "edu_some_college", "Some College or Associate",',
    '  "Education",          "edu_bachelors",    "Bachelor\'s Degree or Higher",',
    '  "Race",               "race_white",       "White Alone",',
    '  "Race",               "race_black",       "Black or African American",',
    '  "Race",               "race_native",      "American Indian or Alaska Native",',
    '  "Race",               "race_asian",       "Asian Alone",',
    '  "Race",               "race_pacific",     "Native Hawaiian or Pacific Islander",',
    '  "Race",               "race_two_plus",    "Two or More Races",',
    '  "Ethnicity",          "eth_hispanic",     "Hispanic or Latino",',
    '  "Ethnicity",          "eth_not_hispanic", "Not Hispanic or Latino",',
    '  "Sex",                "sex_male",         "Male",',
    '  "Sex",                "sex_female",       "Female",',
    '  "Broad Industry",     "ind_goods",        "Goods Producing",',
    '  "Broad Industry",     "ind_trade",        "Trade & Transportation",',
    '  "Broad Industry",     "ind_service",      "Services",',
    '  "2-Digit NAICS",      "cns01",            "NAICS 11: Agriculture, Forestry, Fishing",',
    '  "2-Digit NAICS",      "cns02",            "NAICS 21: Mining, Oil & Gas Extraction",',
    '  "2-Digit NAICS",      "cns03",            "NAICS 22: Utilities",',
    '  "2-Digit NAICS",      "cns04",            "NAICS 23: Construction",',
    '  "2-Digit NAICS",      "cns05",            "NAICS 31-33: Manufacturing",',
    '  "2-Digit NAICS",      "cns06",            "NAICS 42: Wholesale Trade",',
    '  "2-Digit NAICS",      "cns07",            "NAICS 44-45: Retail Trade",',
    '  "2-Digit NAICS",      "cns08",            "NAICS 48-49: Transportation & Warehousing",',
    '  "2-Digit NAICS",      "cns09",            "NAICS 51: Information",',
    '  "2-Digit NAICS",      "cns10",            "NAICS 52: Finance & Insurance",',
    '  "2-Digit NAICS",      "cns11",            "NAICS 53: Real Estate & Rental/Leasing",',
    '  "2-Digit NAICS",      "cns12",            "NAICS 54: Professional, Scientific & Tech",',
    '  "2-Digit NAICS",      "cns13",            "NAICS 55: Management of Companies",',
    '  "2-Digit NAICS",      "cns14",            "NAICS 56: Administrative & Waste Mgmt",',
    '  "2-Digit NAICS",      "cns15",            "NAICS 61: Educational Services",',
    '  "2-Digit NAICS",      "cns16",            "NAICS 62: Health Care & Social Assistance",',
    '  "2-Digit NAICS",      "cns17",            "NAICS 71: Arts, Entertainment & Recreation",',
    '  "2-Digit NAICS",      "cns18",            "NAICS 72: Accommodation & Food Services",',
    '  "2-Digit NAICS",      "cns19",            "NAICS 81: Other Services (ex. Public Admin)",',
    '  "2-Digit NAICS",      "cns20",            "NAICS 92: Public Administration"',
    ")",
    "",
    "dissolve_quietly <- function(sf_obj) {",
    "  suppressMessages(suppressWarnings(",
    "    sf_obj |> sf::st_transform(5070) |> sf::st_make_valid() |> sf::st_buffer(0) |> sf::st_union() |> sf::st_transform(4326)",
    "  ))",
    "}",
    "difference_quietly <- function(geom_a, geom_b) {",
    "  suppressMessages(suppressWarnings(",
    "    sf::st_difference(sf::st_transform(geom_a, 5070), sf::st_transform(geom_b, 5070)) |> sf::st_transform(4326)",
    "  ))",
    "}",
    "union_quietly <- function(geom_a, geom_b) {",
    "  suppressMessages(suppressWarnings(",
    "    sf::st_union(sf::st_transform(geom_a, 5070), sf::st_transform(geom_b, 5070)) |> sf::st_transform(4326)",
    "  ))",
    "}",
    "",
    "fetch_geo_robustly <- function(geom_type, state_id) {",
    "  call_tigris <- function(use_cb, target_year = NULL) {",
    "    args <- list(cb = use_cb, progress_bar = FALSE)",
    "    if (!is.null(target_year)) args$year <- target_year",
    '    if (geom_type == "ZCTAs" && is.null(target_year)) args$year <- 2020',
    "    state_level_geoms <- c(",
    '      "Counties", "Tracts", "Block Groups", "PUMAs",',
    '      "County Subdivisions", "Census Places", "School Districts",',
    '      "Lower State Legislative Districts", "Upper State Legislative Districts",',
    '      "Voting Districts"',
    "    )",
    "    if (geom_type %in% state_level_geoms) args$state <- state_id",
    '    if (geom_type == "Lower State Legislative Districts") args$house <- "lower"',
    '    if (geom_type == "Upper State Legislative Districts") args$house <- "upper"',
    "    raw_shape <- switch(",
    "      geom_type,",
    '      "Full State" = {',
    '        st_shp <- do.call(tigris::states, args)',
    '        st_shp[st_shp$STUSPS == state_id, ]',
    '      },',
    '      "Counties"                          = do.call(tigris::counties, args),',
    '      "Tracts"                            = do.call(tigris::tracts, args),',
    '      "Block Groups"                      = do.call(tigris::block_groups, args),',
    '      "PUMAs"                             = do.call(tigris::pumas, args),',
    '      "ZCTAs"                             = do.call(tigris::zctas, args),',
    '      "Core-based Areas"                  = do.call(tigris::core_based_statistical_areas, args),',
    '      "County Subdivisions"               = do.call(tigris::county_subdivisions, args),',
    '      "Census Places"                     = do.call(tigris::places, args),',
    '      "School Districts"                  = do.call(tigris::school_districts, args),',
    '      "Lower State Legislative Districts" = do.call(tigris::state_legislative_districts, args),',
    '      "Upper State Legislative Districts" = do.call(tigris::state_legislative_districts, args),',
    '      "Urban Areas"                       = do.call(tigris::urban_areas, args),',
    '      "Voting Districts"                  = do.call(tigris::voting_districts, args)',
    "    )",
    '    if (geom_type %in% c("ZCTAs", "Core-based Areas", "Urban Areas")) {',
    "      state_args <- list(cb = use_cb, progress_bar = FALSE)",
    "      if (!is.null(target_year)) state_args$year <- target_year",
    "      state_boundary <- do.call(tigris::states, state_args)",
    "      state_boundary <- state_boundary[state_boundary$STUSPS == state_id, ]",
    "      state_boundary <- sf::st_transform(state_boundary, sf::st_crs(raw_shape))",
    "      intersects <- suppressMessages(sf::st_intersects(raw_shape, state_boundary, sparse = FALSE)[, 1])",
    "      touches    <- suppressMessages(sf::st_touches(raw_shape, state_boundary, sparse = FALSE)[, 1])",
    "      raw_shape  <- raw_shape[intersects & !touches, ]",
    "    }",
    "    raw_shape",
    "  }",
    "  tryCatch(call_tigris(TRUE), error = function(e1) {",
    "    tryCatch(call_tigris(TRUE, 2020), error = function(e2) {",
    "      tryCatch(call_tigris(FALSE), error = function(e3) call_tigris(FALSE, 2020))",
    "    })",
    "  })",
    "}",
    "",
    "summarize_lodes_blocks <- function(df, geocode_col) {",
    "  df |>",
    "    rename(tabblk2020 = !!sym(geocode_col)) |>",
    "    group_by(tabblk2020) |>",
    "    summarise(",
    "      total_jobs = sum(C000, na.rm = TRUE),",
    "      age_under_30 = sum(CA01, na.rm = TRUE), age_30_to_54 = sum(CA02, na.rm = TRUE), age_over_55 = sum(CA03, na.rm = TRUE),",
    "      earn_low = sum(CE01, na.rm = TRUE), earn_mid = sum(CE02, na.rm = TRUE), earn_high = sum(CE03, na.rm = TRUE),",
    "      ind_goods = sum(CNS01 + CNS02 + CNS03 + CNS04 + CNS05, na.rm = TRUE),",
    "      ind_trade = sum(CNS06 + CNS07 + CNS08, na.rm = TRUE),",
    "      ind_service = sum(CNS09 + CNS10 + CNS11 + CNS12 + CNS13 + CNS14 + CNS15 + CNS16 + CNS17 + CNS18 + CNS19 + CNS20, na.rm = TRUE),",
    "      cns01 = sum(CNS01, na.rm = TRUE), cns02 = sum(CNS02, na.rm = TRUE), cns03 = sum(CNS03, na.rm = TRUE), cns04 = sum(CNS04, na.rm = TRUE),",
    "      cns05 = sum(CNS05, na.rm = TRUE), cns06 = sum(CNS06, na.rm = TRUE), cns07 = sum(CNS07, na.rm = TRUE), cns08 = sum(CNS08, na.rm = TRUE),",
    "      cns09 = sum(CNS09, na.rm = TRUE), cns10 = sum(CNS10, na.rm = TRUE), cns11 = sum(CNS11, na.rm = TRUE), cns12 = sum(CNS12, na.rm = TRUE),",
    "      cns13 = sum(CNS13, na.rm = TRUE), cns14 = sum(CNS14, na.rm = TRUE), cns15 = sum(CNS15, na.rm = TRUE), cns16 = sum(CNS16, na.rm = TRUE),",
    "      cns17 = sum(CNS17, na.rm = TRUE), cns18 = sum(CNS18, na.rm = TRUE), cns19 = sum(CNS19, na.rm = TRUE), cns20 = sum(CNS20, na.rm = TRUE),",
    "      race_white = sum(CR01, na.rm = TRUE), race_black = sum(CR02, na.rm = TRUE), race_native = sum(CR03, na.rm = TRUE),",
    "      race_asian = sum(CR04, na.rm = TRUE), race_pacific = sum(CR05, na.rm = TRUE), race_two_plus = sum(CR07, na.rm = TRUE),",
    "      eth_not_hispanic = sum(CT01, na.rm = TRUE), eth_hispanic = sum(CT02, na.rm = TRUE),",
    "      edu_less_hs = sum(CD01, na.rm = TRUE), edu_hs = sum(CD02, na.rm = TRUE),",
    "      edu_some_college = sum(CD03, na.rm = TRUE), edu_bachelors = sum(CD04, na.rm = TRUE),",
    "      sex_male = sum(CS01, na.rm = TRUE), sex_female = sum(CS02, na.rm = TRUE),",
    "      .groups = 'drop'",
    "    )",
    "}",
    "",
    "clean_pdf_label <- function(raw_lbl) {",
    '  lbl <- sub("^[^:]+:\\\\s*", "", raw_lbl)',
    '  lbl <- sub("\\\\s*\\\\([^)]+\\\\)$", "", lbl)',
    '  lbl <- sub(" or Equivalent$", "", lbl)',
    '  lbl <- sub(" or Associate$", "/Assoc.", lbl)',
    '  lbl <- sub(" Degree or Higher$", "+", lbl)',
    '  lbl <- sub(" or Latino$", "", lbl)',
    '  lbl <- sub(" Alone$", "", lbl)',
    "  lbl",
    "}",
    "",
    "summarize_top_groups <- function(totals_df, meta) {",
    '  demo_cats <- c("Age", "Earnings", "Education", "Race", "Ethnicity", "Sex", "Broad Industry")',
    "  lapply(seq_len(nrow(meta)), function(i) {",
    "    rid   <- meta$region_id[i]",
    "    rname <- meta$region_name[i]",
    "    row   <- totals_df |> filter(region_id == rid)",
    "    tot   <- row$total_jobs[1]",
    "    demo_rows <- lapply(demo_cats, function(cat_name) {",
    "      sub_cat <- metric_catalog |> filter(category == cat_name)",
    "      vals <- vapply(sub_cat$var, function(v) row[[v]][1], numeric(1))",
    '      if (cat_name == "Ethnicity") {',
    '        sel_idx <- which(sub_cat$var == "eth_hispanic")',
    "      } else {",
    "        pos_idx <- which(vals > 0)",
    '        if (length(pos_idx) == 0) return(tibble(Region = rname, Category = cat_name, Highlight = "—"))',
    "        ord <- pos_idx[order(vals[pos_idx], decreasing = TRUE)]",
    '        if (cat_name %in% c("Age", "Education", "Race")) {',
    "          sel_idx <- ord[1:min(2, length(ord))]",
    '        } else if (cat_name %in% c("Earnings", "Broad Industry", "Sex")) {',
    "          sel_idx <- ord",
    "        } else {",
    "          sel_idx <- ord[1]",
    "        }",
    "      }",
    "      item_strs <- vapply(sel_idx, function(idx) {",
    "        val <- vals[idx]",
    "        lbl <- clean_pdf_label(sub_cat$label[idx])",
    '        pct <- if (tot > 0) scales::percent(val / tot, accuracy = 0.1) else "0.0%"',
    '        paste0(lbl, " (", scales::comma(val), " | ", pct, ")")',
    "      }, character(1))",
    '      tibble(Region = rname, Category = cat_name, Highlight = paste(item_strs, collapse = "LINEBREAKHERE"))',
    "    }) |> bind_rows()",
    '    naics_cat <- metric_catalog |> filter(category == "2-Digit NAICS")',
    "    n_vals <- vapply(naics_cat$var, function(v) row[[v]][1], numeric(1))",
    "    pos_naics <- which(n_vals > 0)",
    "    if (length(pos_naics) == 0) {",
    '      naics_row <- tibble(Region = rname, Category = "Top NAICS Codes", Highlight = "—")',
    "    } else {",
    "      top3_idx <- pos_naics[order(n_vals[pos_naics], decreasing = TRUE)][1:min(3, length(pos_naics))]",
    "      top3_strs <- vapply(top3_idx, function(idx) {",
    "        val <- n_vals[idx]",
    '        pct <- if (tot > 0) scales::percent(val / tot, accuracy = 0.1) else "0.0%"',
    '        naics_code_only <- sub(":.*$", "", naics_cat$label[idx])',
    '        paste0(naics_code_only, " (", scales::comma(val), " | ", pct, ")")',
    "      }, character(1))",
    '      naics_row <- tibble(Region = rname, Category = "Top NAICS Codes", Highlight = paste(top3_strs, collapse = "LINEBREAKHERE"))',
    "    }",
    "    bind_rows(demo_rows, naics_row)",
    "  }) |> bind_rows()",
    "}",
    "",
    "# ------------------------------------------------------------------------------",
    "# 4. Rebuild Custom Region Geometries from TIGER/Line",
    "# ------------------------------------------------------------------------------",
    "message('Fetching TIGER/Line boundaries for defined regions...')",
    "all_areas_df <- bind_rows(lapply(regions_def, function(r) r$areas))",
    "unique_layers <- all_areas_df |> distinct(SRC_STATE, SRC_GEOM)",
    "layer_cache <- list()",
    "",
    "for (k in seq_len(nrow(unique_layers))) {",
    "  st_k <- unique_layers$SRC_STATE[k]",
    "  gm_k <- unique_layers$SRC_GEOM[k]",
    "  key_k <- paste(st_k, gm_k, sep = '::')",
    "  raw_shp <- fetch_geo_robustly(gm_k, st_k)",
    '  possible_ids <- c("GEOID", "GEOID20", "GEOID10", "ZCTA5CE20", "ZCTA5CE10")',
    "  id_col <- intersect(possible_ids, names(raw_shp))[1]",
    '  if (is.na(id_col)) id_col <- grep("^GEOID|^ZCTA", names(raw_shp), value = TRUE)[1]',
    "  layer_cache[[key_k]] <- raw_shp |>",
    "    mutate(MAP_GEOID = as.character(.data[[id_col]]), UNIQUE_KEY = paste(st_k, gm_k, MAP_GEOID, sep = '::')) |>",
    "    select(UNIQUE_KEY, MAP_GEOID) |>",
    "    st_transform(4326) |>",
    "    st_make_valid()",
    "}",
    "",
    "dissolved_list <- list()",
    "base_parts_list <- list()",
    "cumulative_higher_geom <- NULL",
    "",
    "for (idx in seq_along(regions_def)) {",
    "  r <- regions_def[[idx]]",
    "  r_areas <- r$areas |> mutate(LAYER_KEY = paste(SRC_STATE, SRC_GEOM, sep = '::'), UNIQUE_KEY = paste(SRC_STATE, SRC_GEOM, MAP_GEOID, sep = '::'))",
    "  r_sf_parts <- lapply(split(r_areas, r_areas$LAYER_KEY), function(sub_df) {",
    "    layer_cache[[sub_df$LAYER_KEY[1]]] |> filter(UNIQUE_KEY %in% sub_df$UNIQUE_KEY)",
    "  })",
    "  r_sf <- do.call(rbind, r_sf_parts)",
    "  base_parts_list[[idx]] <- r_sf |> select(UNIQUE_KEY, geometry)",
    "  r_geom <- dissolve_quietly(r_sf)",
    "  if (exclude_overlap && !is.null(cumulative_higher_geom)) {",
    "    r_geom <- difference_quietly(r_geom, cumulative_higher_geom)",
    "  }",
    "  cumulative_higher_geom <- if (is.null(cumulative_higher_geom)) r_geom else union_quietly(cumulative_higher_geom, r_geom)",
    "  dissolved_list[[idx]] <- st_sf(region_id = r$id, region_name = r$name, region_color = r$color, priority = idx, geometry = r_geom)",
    "}",
    "",
    "regions_poly  <- do.call(rbind, rev(dissolved_list))",
    "base_parts_sf <- do.call(rbind, base_parts_list) |> distinct(UNIQUE_KEY, .keep_all = TRUE)",
    "",
    "# ------------------------------------------------------------------------------",
    "# 5. Download & Aggregate LODES8 RAC, WAC & OD Data",
    "# ------------------------------------------------------------------------------",
    'cache_dir <- tools::R_user_dir("lodes_region_compare_cache", which = "cache")',
    "if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)",
    "",
    "all_states <- unique(all_areas_df$SRC_STATE)",
    "state_wac_list <- list()",
    "state_rac_list <- list()",
    "all_block_lookup_list <- list()",
    "od_raw_list <- list()",
    "",
    "for (st in all_states) {",
    "  st_lower <- tolower(st)",
    "  message('Processing LODES8 data for ', st, '...')",
    '  xwalk_url   <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/", st_lower, "/", st_lower, "_xwalk.csv.gz")',
    '  wac_url     <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/", st_lower, "/wac/", st_lower, "_wac_", seg_val, "_", jt_val, "_", year_val, ".csv.gz")',
    '  rac_url     <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/", st_lower, "/rac/", st_lower, "_rac_", seg_val, "_", jt_val, "_", year_val, ".csv.gz")',
    '  od_main_url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/", st_lower, "/od/", st_lower, "_od_main_", jt_val, "_", year_val, ".csv.gz")',
    '  od_aux_url  <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/", st_lower, "/od/", st_lower, "_od_aux_", jt_val, "_", year_val, ".csv.gz")',
    "",
    "  xwalk_dest   <- file.path(cache_dir, basename(xwalk_url))",
    "  wac_dest     <- file.path(cache_dir, basename(wac_url))",
    "  rac_dest     <- file.path(cache_dir, basename(rac_url))",
    "  od_main_dest <- file.path(cache_dir, basename(od_main_url))",
    "  od_aux_dest  <- file.path(cache_dir, basename(od_aux_url))",
    "",
    '  if (!file.exists(xwalk_dest))   download.file(xwalk_url, xwalk_dest, mode = "wb", quiet = TRUE)',
    '  if (!file.exists(wac_dest))     download.file(wac_url, wac_dest, mode = "wb", quiet = TRUE)',
    '  if (!file.exists(rac_dest))     download.file(rac_url, rac_dest, mode = "wb", quiet = TRUE)',
    '  if (!file.exists(od_main_dest)) try(download.file(od_main_url, od_main_dest, mode = "wb", quiet = TRUE), silent = TRUE)',
    '  if (!file.exists(od_aux_dest))  try(download.file(od_aux_url, od_aux_dest, mode = "wb", quiet = TRUE), silent = TRUE)',
    "",
    '  xwalk_df <- read_csv(xwalk_dest, col_types = cols_only(tabblk2020 = "c", blklatdd = "d", blklondd = "d")) |> filter(!is.na(blklatdd), !is.na(blklondd))',
    '  blk_pts <- st_as_sf(xwalk_df, coords = c("blklondd", "blklatdd"), crs = 4326)',
    "  joined_pts <- suppressMessages(st_join(blk_pts, regions_poly, join = st_within, left = FALSE)) |> st_drop_geometry() |> select(tabblk2020, region_id)",
    "  highest_prio_pts <- suppressMessages(st_join(blk_pts, regions_poly |> arrange(priority), join = st_within, left = FALSE)) |> st_drop_geometry() |> distinct(tabblk2020, .keep_all = TRUE) |> select(tabblk2020, region_id)",
    "  all_block_lookup_list[[st]] <- highest_prio_pts",
    "",
    "  if (nrow(joined_pts) > 0) {",
    '    wac_raw <- read_csv(wac_dest, col_types = cols(.default = "d", w_geocode = "c"))',
    '    rac_raw <- read_csv(rac_dest, col_types = cols(.default = "d", h_geocode = "c"))',
    '    state_wac_list[[st]] <- summarize_lodes_blocks(wac_raw, "w_geocode") |> inner_join(joined_pts, by = "tabblk2020", relationship = "many-to-many") |> group_by(region_id) |> summarise(across(total_jobs:sex_female, \\(x) sum(x, na.rm = TRUE)), .groups = "drop")',
    '    state_rac_list[[st]] <- summarize_lodes_blocks(rac_raw, "h_geocode") |> inner_join(joined_pts, by = "tabblk2020", relationship = "many-to-many") |> group_by(region_id) |> summarise(across(total_jobs:sex_female, \\(x) sum(x, na.rm = TRUE)), .groups = "drop")',
    "  }",
    "",
    '  od_cols <- cols_only(w_geocode = "c", h_geocode = "c", S000 = "d", SA01 = "d", SA02 = "d", SA03 = "d", SE01 = "d", SE02 = "d", SE03 = "d", SI01 = "d", SI02 = "d", SI03 = "d")',
    '  if (file.exists(od_main_dest)) od_raw_list[[paste0(st, "_main")]] <- read_csv(od_main_dest, col_types = od_cols)',
    '  if (file.exists(od_aux_dest) && length(all_states) > 1) od_raw_list[[paste0(st, "_aux")]] <- read_csv(od_aux_dest, col_types = od_cols)',
    "}",
    "",
    "reg_meta <- tibble(",
    "  region_id    = vapply(regions_def, function(r) r$id, character(1)),",
    "  region_name  = vapply(regions_def, function(r) r$name, character(1)),",
    "  region_color = vapply(regions_def, function(r) r$color, character(1))",
    ")",
    "",
    "wac_totals <- reg_meta |> left_join(bind_rows(state_wac_list) |> group_by(region_id) |> summarise(across(total_jobs:sex_female, \\(x) sum(x, na.rm = TRUE)), .groups = 'drop'), by = 'region_id') |> mutate(across(total_jobs:sex_female, ~coalesce(., 0)))",
    "rac_totals <- reg_meta |> left_join(bind_rows(state_rac_list) |> group_by(region_id) |> summarise(across(total_jobs:sex_female, \\(x) sum(x, na.rm = TRUE)), .groups = 'drop'), by = 'region_id') |> mutate(across(total_jobs:sex_female, ~coalesce(., 0)))",
    "",
    "block_lookup <- bind_rows(all_block_lookup_list) |> distinct(tabblk2020, .keep_all = TRUE)",
    "id_to_name <- setNames(reg_meta$region_name, reg_meta$region_id)",
    "",
    "od_flows <- bind_rows(od_raw_list) |>",
    '  left_join(block_lookup |> rename(h_geocode = tabblk2020, origin_id = region_id), by = "h_geocode") |>',
    '  left_join(block_lookup |> rename(w_geocode = tabblk2020, dest_id   = region_id), by = "w_geocode") |>',
    "  filter(!is.na(origin_id) | !is.na(dest_id)) |>",
    "  mutate(",
    '    origin_id   = coalesce(origin_id, "OUTSIDE"),',
    '    dest_id     = coalesce(dest_id, "OUTSIDE"),',
    '    Origin      = ifelse(origin_id == "OUTSIDE", "Outside Selected Regions", unname(id_to_name[origin_id])),',
    '    Destination = ifelse(dest_id == "OUTSIDE", "Outside Selected Regions", unname(id_to_name[dest_id]))',
    "  ) |>",
    "  group_by(origin_id, Origin, dest_id, Destination) |>",
    "  summarise(",
    "    Total_Jobs = sum(S000, na.rm = TRUE),",
    "    Age_Under_30 = sum(SA01, na.rm = TRUE), Age_30_to_54 = sum(SA02, na.rm = TRUE), Age_55_Plus = sum(SA03, na.rm = TRUE),",
    "    Earn_Low = sum(SE01, na.rm = TRUE), Earn_Mid = sum(SE02, na.rm = TRUE), Earn_High = sum(SE03, na.rm = TRUE),",
    "    Ind_Goods = sum(SI01, na.rm = TRUE), Ind_Trade = sum(SI02, na.rm = TRUE), Ind_Services = sum(SI03, na.rm = TRUE),",
    "    .groups = 'drop'",
    "  ) |>",
    "  group_by(origin_id) |> mutate(origin_tot = sum(Total_Jobs, na.rm = TRUE), Share_of_Origin_Workers = ifelse(origin_tot > 0, Total_Jobs / origin_tot, 0)) |> select(-origin_tot) |> ungroup() |>",
    "  group_by(dest_id) |> mutate(dest_tot = sum(Total_Jobs, na.rm = TRUE), Share_of_Dest_Jobs = ifelse(dest_tot > 0, Total_Jobs / dest_tot, 0)) |> select(-dest_tot) |> ungroup()",
    "",
    "# ------------------------------------------------------------------------------",
    "# 6. Export CSV Tables (All-Zero Rows Omitted)",
    "# ------------------------------------------------------------------------------",
    "build_grouped_csv <- function(totals_df, ds_label) {",
    "  n_reg <- nrow(totals_df)",
    "  lapply(seq_len(nrow(metric_catalog)), function(idx) {",
    "    v <- metric_catalog$var[idx]",
    '    is_tot <- (v == "total_jobs")',
    "    counts <- vapply(seq_len(n_reg), function(i) totals_df[[v]][i], numeric(1))",
    "    if (!is_tot && sum(counts, na.rm = TRUE) == 0) return(NULL)",
    "    row_list <- list(Dataset = ds_label, Category = metric_catalog$category[idx], Characteristic = metric_catalog$label[idx])",
    "    for (i in seq_len(n_reg)) row_list[[paste0('Count (#) - ', totals_df$region_name[i])]] <- counts[i]",
    "    for (i in seq_len(n_reg)) {",
    "      tot <- totals_df$total_jobs[i]",
    "      row_list[[paste0('Share (%) - ', totals_df$region_name[i])]] <- if (is_tot) NA_real_ else if (tot > 0) counts[i] / tot else 0",
    "    }",
    "    as_tibble(row_list)",
    "  }) |> bind_rows()",
    "}",
    "",
    'rac_wac_csv_path <- file.path(output_dir, paste0("lodes_multi_region_rac_wac_", Sys.Date(), ".csv"))',
    'od_csv_path      <- file.path(output_dir, paste0("lodes_od_commuting_flows_", Sys.Date(), ".csv"))',
    'write_csv(bind_rows(build_grouped_csv(wac_totals, "WAC (Work Location)"), build_grouped_csv(rac_totals, "RAC (Home Location)")), rac_wac_csv_path)',
    "write_csv(od_flows |> select(Origin, Destination, Total_Jobs, Share_of_Origin_Workers, Share_of_Dest_Jobs, Age_Under_30, Age_30_to_54, Age_55_Plus, Earn_Low, Earn_Mid, Earn_High, Ind_Goods, Ind_Trade, Ind_Services) |> arrange(desc(Total_Jobs)), od_csv_path)",
    "message('Saved CSVs to: ', output_dir)",
    "",
    "# ------------------------------------------------------------------------------",
    "# 7. Render Executive PDF Report",
    "# ------------------------------------------------------------------------------",
    "res_bundle <- list(",
    "  reg_meta = reg_meta, exclude_overlap = exclude_overlap, year = year_val,",
    "  job_type = jt_lbl, segment_label = seg_lbl, regions_poly = regions_poly,",
    "  base_parts_sf = base_parts_sf, wac_totals = wac_totals, rac_totals = rac_totals, od_flows = od_flows",
    ")",
    "",
    "if (!dir.exists(pdf_base_dir)) dir.create(pdf_base_dir, recursive = TRUE, showWarnings = FALSE)",
    'work_dir <- normalizePath(file.path(pdf_base_dir, paste0("standalone_", as.integer(Sys.time()))), winslash = "/", mustWork = FALSE)',
    "dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)",
    "on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)",
    'saveRDS(res_bundle, file.path(work_dir, "report_data.rds"))',
    "",
    "is_landscape <- nrow(reg_meta) > 5",
    'geom_yaml    <- if (is_landscape) \'geometry: "margin=0.5in,landscape"\' else "geometry: margin=0.6in"',
    'map_chunk_hdr <- if (is_landscape) "{r map_and_kpis, fig.width=9.5, fig.height=3.0, fig.align=\'center\'}" else "{r map_and_kpis, fig.width=8, fig.height=3.5, fig.align=\'center\'}"',
    'bt <- paste0(rep("`", 3), collapse = "")',
    'rmd_path <- file.path(work_dir, "comparison_report.Rmd")',
    "",
    "rmd_lines <- c(",
    '  "---",',
    '  \'title: "Regional Workforce & Commuting Comparison"\',',
    '  \'date: "`r format(Sys.Date(), \\\'%B %d, %Y\\\')`"\',',
    '  "output:", "  pdf_document:", "    latex_engine: pdflatex",',
    "  geom_yaml,",
    '  "header-includes:", "  - \\\\usepackage{booktabs}", "  - \\\\usepackage{longtable}", "  - \\\\usepackage{array}", "  - \\\\usepackage{graphicx}", "  - \\\\usepackage{float}", "  - \\\\usepackage{xcolor}", "  - \\\\setlength{\\\\tabcolsep}{4pt}", "  - \\\\renewcommand{\\\\arraystretch}{1.3}",',
    '  "---", "",',
    '  paste0(bt, "{r setup, include=FALSE}"),',
    '  "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",',
    '  "library(dplyr); library(tidyr); library(sf); library(ggplot2); library(scales); library(knitr)",',
    '  \'res <- readRDS("report_data.rds"); meta <- res$reg_meta; is_land <- nrow(meta) > 5\',',
    '  "render_wrapped_longtable <- function(df, cat_width = 1.05) {",',
    '  "  n_reg <- ncol(df) - 1; page_w <- if (is_land) 9.8 else 7.15",',
    '  "  usable_w <- page_w - cat_width - (n_reg + 1) * 0.115; reg_w <- round(max(0.95, usable_w / max(1, n_reg)), 2)",',
    '  \'  col_spec <- paste0(">{\\\\\\\\raggedright\\\\\\\\arraybackslash}p{", cat_width, "in}", paste(rep(paste0(">{\\\\\\\\raggedright\\\\\\\\arraybackslash}p{", reg_w, "in}"), n_reg), collapse = ""))\',',
    '  \'  tex <- kable(df, format = "latex", booktabs = TRUE, longtable = TRUE)\',',
    '  \'  tex <- gsub("LINEBREAKHERE", " \\\\\\\\newline ", tex, fixed = TRUE)\',',
    '  "  tex_lines <- strsplit(tex, \'\\\\n\', fixed = TRUE)[[1]]",',
    '  "  lt_idx <- which(startsWith(tex_lines, \'\\\\\\\\begin{longtable}\'))",',
    '  "  if (length(lt_idx) > 0) tex_lines[lt_idx[1]] <- paste0(\'\\\\\\\\begin{longtable}[l]{\', col_spec, \'}\')",',
    '  \'  cat("\\\\\\\\footnotesize\\\\n", paste(tex_lines, collapse = "\\\\n"), "\\\\n\\\\\\\\normalsize\\\\n")\',',
    '  "}", bt, "",',
    '  "**Parameters:** Year `r res$year` | Job Type: `r res$job_type` | Workforce Segment: `r res$segment_label` | Overlap Mode: `r ifelse(res$exclude_overlap, \'Higher-priority regions excluded from lower regions\', \'Full selected boundaries included\')`", "",',
    '  "## 1. Custom Regions Map & Headline Totals", "",',
    '  paste0(bt, map_chunk_hdr),',
    '  "polys_plot <- res$regions_poly |> arrange(priority); polys_plot$region_name <- factor(polys_plot$region_name, levels = meta$region_name); pal_map <- setNames(meta$region_color, meta$region_name)",',
    '  \'p <- ggplot() + geom_sf(data = res$base_parts_sf, fill = "#f8fafc", color = "#cbd5e1", linewidth = 0.25)\',',
    '  \'for (idx in rev(seq_len(nrow(polys_plot)))) p <- p + geom_sf(data = polys_plot[idx, ], aes(fill = region_name), color = "#0f172a", linewidth = 0.45, alpha = 0.72)\',',
    '  \'p + scale_fill_manual(values = pal_map, name = "Custom Regions") + theme_void() + theme(legend.position = "right", legend.title = element_text(face = "bold", size = 10), legend.text = element_text(size = 9), plot.margin = margin(4, 4, 4, 4))\',',
    '  bt, "",',
    '  paste0(bt, "{r kpi_table}"),',
    '  "kpi_df <- tibble(Region = meta$region_name, `Jobs by Work Location` = res$wac_totals$total_jobs, `Workers by Home Location` = res$rac_totals$total_jobs) |> mutate(`Jobs-to-Workers Ratio` = ifelse(`Workers by Home Location` > 0, round(`Jobs by Work Location` / `Workers by Home Location`, 2), NA_real_), `Jobs by Work Location` = comma(`Jobs by Work Location`), `Workers by Home Location` = comma(`Workers by Home Location`))",',
    '  \'kable(kpi_df, booktabs = TRUE, align = c("l", "r", "r", "r"))\',',
    '  bt, "",',
    '  "## 2. Area Characteristics by Work Location (WAC)", "",',
    '  paste0(bt, "{r wac_highlights, results=\'asis\'}"),',
    '  "render_wrapped_longtable(summarize_top_groups(res$wac_totals, meta) |> pivot_wider(names_from = Region, values_from = Highlight))",',
    '  bt, "",',
    '  "*Notes: Categories showing a subset display the largest non-zero groups in descending order: Top 2 groups for Age, Education, and Race; and Top 3 sectors for 2-digit NAICS codes (see Section 5 for sector titles). Active categories are shown for Earnings, Broad Industry, and Sex. Ethnicity displays the Hispanic or Latino share. Monthly earnings tiers: Very Low = $1,250/mo or less; Low = $1,251 to $3,333/mo; Average = > $3,333/mo.*", "",',
    '  "## 3. Area Characteristics by Home Location (RAC)", "",',
    '  paste0(bt, "{r rac_highlights, results=\'asis\'}"),',
    '  "render_wrapped_longtable(summarize_top_groups(res$rac_totals, meta) |> pivot_wider(names_from = Region, values_from = Highlight))",',
    '  bt, "",',
    '  "*Notes: Categories showing a subset display the largest non-zero groups in descending order: Top 2 groups for Age, Education, and Race; and Top 3 sectors for 2-digit NAICS codes (see Section 5 for sector titles). Active categories are shown for Earnings, Broad Industry, and Sex. Ethnicity displays the Hispanic or Latino share. Monthly earnings tiers: Very Low = $1,250/mo or less; Low = $1,251 to $3,333/mo; Average = > $3,333/mo.*", "",',
    '  "## 4. Commuting Flows Between Regions (Origin-Destination)", "",',
    '  paste0(bt, "{r od_matrix, results=\'asis\'}"),',
    '  \'od <- res$od_flows; all_ids <- c(meta$region_id, "OUTSIDE"); all_names <- c(meta$region_name, "Outside Selected Regions")\',',
    '  \'od_mat <- lapply(seq_len(nrow(meta)), function(i) { oid <- meta$region_id[i]; sub_od <- od |> filter(origin_id == oid); tot_res <- sum(sub_od$Total_Jobs, na.rm = TRUE); row_vals <- list(`Home Origin` = meta$region_name[i], `Total Resident Workers` = comma(tot_res)); for (j in seq_along(all_ids)) { did <- all_ids[j]; v <- sub_od$Total_Jobs[sub_od$dest_id == did]; v <- if (length(v) == 0) 0 else v[1]; pct <- if (tot_res > 0) percent(v / tot_res, accuracy = 0.1) else "0.0%"; row_vals[[all_names[j]]] <- paste0(comma(v), " (", pct, ")") }; as_tibble(row_vals) }) |> bind_rows()\',',
    '  \'od_tex <- kable(od_mat, format = "latex", booktabs = TRUE, align = c("l", rep("r", ncol(od_mat) - 1)))\',',
    '  \'cat("\\\\\\\\begin{table}[H]\\\\n\\\\\\\\centering\\\\n\\\\\\\\resizebox{\\\\\\\\textwidth}{!}{\\\\n", od_tex, "\\\\n}\\\\n\\\\\\\\end{table}\\\\n")\',',
    '  bt, "",',
    '  "### Top Inter-Regional & Internal Commuting Corridors", "",',
    '  paste0(bt, "{r od_top_pairs, results=\'asis\'}"),',
    '  \'filtered_od <- od |> filter(origin_id != "OUTSIDE" | dest_id != "OUTSIDE") |> arrange(desc(Total_Jobs)); total_corridors <- nrow(filtered_od)\',',
    '  "top_od <- filtered_od |> slice_head(n = 20) |> transmute(`Home Origin` = Origin, `Work Destination` = Destination, `Commuters` = comma(Total_Jobs), `Origin %` = percent(Share_of_Origin_Workers, accuracy = 0.1), `Dest. %` = percent(Share_of_Dest_Jobs, accuracy = 0.1), `Goods Producing` = comma(Ind_Goods), `Trade & Transp.` = comma(Ind_Trade), `Services` = comma(Ind_Services))",',
    '  \'cor_tex <- kable(top_od, format = "latex", booktabs = TRUE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))\',',
    '  "cor_lines <- strsplit(cor_tex, \'\\\\n\', fixed = TRUE)[[1]]; tab_idx <- which(startsWith(cor_lines, \'\\\\\\\\begin{tabular}\'))",',
    '  "if (length(tab_idx) > 0) cor_lines[tab_idx[1]] <- \'\\\\\\\\begin{tabular}[t]{>{\\\\\\\\raggedright\\\\\\\\arraybackslash}p{1.2in}>{\\\\\\\\raggedright\\\\\\\\arraybackslash}p{1.2in}rrrrrr}\'",',
    '  \'cat("\\\\\\\\begin{table}[H]\\\\n\\\\\\\\centering\\\\n\\\\\\\\resizebox{\\\\\\\\textwidth}{!}{\\\\n", paste(cor_lines, collapse = "\\\\n"), "\\\\n}\\\\n\\\\\\\\end{table}\\\\n")\',',
    '  \'if (total_corridors > 20) cat(sprintf("\\\\n*Note: Showing the top 20 commuting corridors out of %s total available origin-destination pairs.*\\\\n", comma(total_corridors)))\',',
    '  bt, "",',
    '  "## 5. Data Sources & Definitions", "",',
    '  "All workforce and commuting statistics are derived from the **U.S. Census Bureau Longitudinal Employer-Household Dynamics (LEHD) Origin-Destination Employment Statistics (LODES, Version 8)** program (https://lehd.ces.census.gov/ and https://lehd.ces.census.gov/data/).", "",',
    '  "### 2-Digit NAICS Industry Sector Reference", "",',
    '  paste0(bt, "{r naics_ref_table, results=\'asis\'}"),',
    '  \'naics_df <- metric_catalog |> filter(category == "2-Digit NAICS") |> mutate(Code = sub(":.*$", "", label), Sector = sub("^[^:]+:\\\\\\\\s*", "", label), Broad = case_when(var %in% c("cns01", "cns02", "cns03", "cns04", "cns05") ~ "Goods Producing", var %in% c("cns06", "cns07", "cns08") ~ "Trade & Transp.", TRUE ~ "Services")) |> select(Code, Sector, Broad)\',',
    '  \'naics_wide <- bind_cols(naics_df[1:10, ], naics_df[11:20, ]); names(naics_wide) <- c("NAICS Code", "Industry Sector", "Broad Sector", "NAICS Code ", "Industry Sector ", "Broad Sector ")\',',
    '  \'n_tex <- kable(naics_wide, format = "latex", booktabs = TRUE, align = c("l", "l", "l", "l", "l", "l"))\',',
    '  \'cat("\\\\\\\\begin{table}[H]\\\\n\\\\\\\\centering\\\\n\\\\\\\\resizebox{\\\\\\\\textwidth}{!}{\\\\n", n_tex, "\\\\n}\\\\n\\\\\\\\end{table}\\\\n")\',',
    '  bt',
    ")",
    "writeLines(rmd_lines, rmd_path)",
    "",
    'pdf_out_path <- file.path(output_dir, paste0("LODES_Regional_Comparison_Report_", Sys.Date(), ".pdf"))',
    "render_env <- new.env(parent = globalenv())",
    "render_env$summarize_top_groups <- summarize_top_groups",
    "render_env$clean_pdf_label <- clean_pdf_label",
    "render_env$metric_catalog <- metric_catalog",
    "",
    "rmarkdown::render(",
    "  input = rmd_path,",
    '  output_format = "pdf_document",',
    "  output_file = pdf_out_path,",
    "  intermediates_dir = work_dir,",
    "  knit_root_dir = work_dir,",
    "  envir = render_env,",
    "  quiet = TRUE",
    ")",
    "message('Saved PDF report to: ', pdf_out_path)"
  )
  
  paste(script_lines, collapse = "\n")
}

# ------------------------------------------------------------------------------
# User Interface
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "LODES Custom Region Builder & RAC/WAC/OD Comparator",
  sidebar = sidebar(
    width = "410px",
    padding = c("10px", "14px"),
    gap = "8px",
    
    tags$head(tags$style(HTML("
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
    
    # --- STEP 1: BUILD CUSTOM REGIONS ---
    h5("1. Select Areas on Map", style = "color: #1e3a8a; font-weight: 700; margin-bottom: 2px;"),
    p(class = "text-muted", style = "font-size: 0.82em; margin-bottom: 4px;",
      "Click polygons or use the polygon draw tool (top-right). Switch states/geographies anytime—your staged selection persists until assigned."),
    
    layout_columns(
      col_widths = c(5, 7),
      selectInput("state", "State:", choices = state_choices, selected = "NV"),
      selectInput("geometry", "Geography:", choices = c(
        "Full State", "Counties", "Tracts", "Block Groups", "PUMAs", "ZCTAs",
        "Core-based Areas", "County Subdivisions", "Census Places",
        "School Districts", "Lower State Legislative Districts",
        "Upper State Legislative Districts", "Urban Areas", "Voting Districts"
      ), selected = "Counties")
    ),
    
    div(
      style = "background: #f1f5f9; padding: 8px; border-radius: 6px; font-size: 0.88em;",
      div(style = "font-weight: 600; margin-bottom: 4px;", textOutput("staged_summary")),
      div(
        style = "display: flex; gap: 6px; align-items: center;",
        div(style = "flex: 1;", selectInput("target_region", label = NULL, choices = c("Custom Region 1" = "R1", "Custom Region 2" = "R2"), width = "100%")),
        actionButton("assign_staged", "Add Areas", class = "btn-primary btn-sm"),
        actionButton("clear_staged", "Clear", class = "btn-outline-secondary btn-sm")
      )
    ),
    
    hr(style = "margin: 6px 0;"),
    
    # --- STEP 2: DYNAMICALLY ADD, NAME, IMPORT/EXPORT, RESET & DELETE REGIONS ---
    div(
      style = "display: flex; justify-content: space-between; align-items: center; gap: 4px; flex-wrap: wrap;",
      h5("2. Custom Regions", style = "color: #1e3a8a; font-weight: 700; margin: 0;"),
      div(
        style = "display: flex; gap: 4px;",
        actionButton("update_names_btn", "Update Names", class = "btn-outline-secondary btn-sm", style = "padding: 2px 6px; font-size: 0.8em;"),
        actionButton("add_region_btn", "+ Add", class = "btn-outline-primary btn-sm", style = "padding: 2px 6px; font-size: 0.8em;"),
        actionButton("reset_all_regions_btn", "Reset All", class = "btn-outline-danger btn-sm", style = "padding: 2px 6px; font-size: 0.8em;", title = "Remove all regions, names, and selections and start over")
      )
    ),
    
    # Import / Export Saved Region Definitions
    div(
      style = "background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 6px; padding: 6px 8px; font-size: 0.82em;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; gap: 6px;",
        span(style = "font-weight: 600; color: #334155;", "Region Definitions:"),
        downloadButton("export_regions_btn", "Export JSON", class = "btn-outline-secondary btn-sm", style = "padding: 2px 8px; font-size: 0.9em;")
      ),
      div(
        class = "compact-file-input",
        style = "margin-top: 5px;",
        fileInput(
          "import_regions_file",
          label = NULL,
          accept = c(".json", "application/json"),
          buttonLabel = "Import JSON...",
          placeholder = "Load saved regions file",
          width = "100%"
        )
      )
    ),
    
    div(
      style = "max-height: 220px; overflow-y: auto; padding-right: 2px;",
      uiOutput("dynamic_regions_ui")
    ),
    
    checkboxInput(
      "exclude_higher_overlap",
      "Exclude higher-ranked regions' overlap from lower regions (e.g., R1 from R2, R1–R2 from R3)",
      value = FALSE
    ),
    
    hr(style = "margin: 6px 0;"),
    
    # --- STEP 3: LODES PARAMETERS & RUN ---
    h5("3. LODES Parameters & Compare", style = "color: #1e3a8a; font-weight: 700; margin-bottom: 2px;"),
    layout_columns(
      col_widths = c(4, 8),
      selectInput("year", "Year:", choices = seq(2023, 2002), selected = 2023),
      selectInput("job_type", "Job Type:", choices = job_type_choices, selected = "JT00")
    ),
    selectInput("segment", "Workforce Segment:", choices = segment_choices, selected = "S000"),
    
    actionButton("run_comparison", "Fetch RAC, WAC & OD Data", class = "btn-success w-100 fw-bold")
  ),
  
  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      title = "1. Region Selector Map",
      value = "selector_tab",
      fillable = TRUE,
      maplibreOutput("selector_map", height = "calc(100vh - 135px)", width = "100%")
    ),
    nav_panel(
      title = "2. Comparison Summary Report",
      value = "report_tab",
      uiOutput("report_ui")
    ),
    nav_panel(
      title = "3. Regional Profile Map (RAC & WAC)",
      value = "profile_map_tab",
      fillable = TRUE,
      maplibreOutput("profile_map", height = "calc(100vh - 135px)", width = "100%")
    ),
    nav_panel(
      title = "4. OD Commuting Flows",
      value = "od_tab",
      uiOutput("od_ui")
    )
  )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  staged_sf          <- reactiveVal(NULL)
  comparison_results <- reactiveVal(NULL)
  next_reg_num       <- reactiveVal(3)
  
  make_default_regions <- function() {
    list(
      R1 = list(id = "R1", name = "Custom Region 1", color = region_palette[1], sf = NULL),
      R2 = list(id = "R2", name = "Custom Region 2", color = region_palette[2], sf = NULL)
    )
  }
  
  regions_store <- reactiveVal(make_default_regions())
  
  sync_region_names <- function(regs) {
    for (rid in names(regs)) {
      inp_val <- isolate(input[[paste0("name_", rid)]])
      if (!is.null(inp_val) && nzchar(trimws(inp_val))) {
        regs[[rid]]$name <- trimws(inp_val)
      }
    }
    regs
  }
  
  # ----------------------------------------------------------------------------
  # Master Reset All Regions, Names, Colors & Selections
  # ----------------------------------------------------------------------------
  observeEvent(input$reset_all_regions_btn, {
    regions_store(make_default_regions())
    next_reg_num(3)
    staged_sf(NULL)
    comparison_results(NULL)
    clear_drawn_features(maplibre_proxy("selector_map"))
    updateCheckboxInput(session, "exclude_higher_overlap", value = FALSE)
    updateSelectInput(
      session,
      "target_region",
      choices = c("Custom Region 1" = "R1", "Custom Region 2" = "R2"),
      selected = "R1"
    )
    showNotification(
      "All regions, names, and colors have been reset to default.",
      type = "message",
      duration = 3
    )
  })
  
  # ----------------------------------------------------------------------------
  # Export / Import Region Definitions (JSON)
  # ----------------------------------------------------------------------------
  output$export_regions_btn <- downloadHandler(
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
            tibble(
              UNIQUE_KEY = character(),
              MAP_GEOID  = character(),
              AREA_LABEL = character(),
              SRC_STATE  = character(),
              SRC_GEOM   = character()
            )
          } else {
            r$sf |>
              sf::st_drop_geometry() |>
              select(UNIQUE_KEY, MAP_GEOID, AREA_LABEL, SRC_STATE, SRC_GEOM)
          }
          list(
            id    = r$id,
            name  = r$name,
            color = r$color,
            areas = areas_tbl
          )
        }))
      )
      
      writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE), file)
    }
  )
  
  observeEvent(input$import_regions_file, {
    file_info <- input$import_regions_file
    req(file_info$datapath)
    
    parsed <- tryCatch(
      jsonlite::fromJSON(file_info$datapath, simplifyVector = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(parsed) || is.null(parsed$regions) || length(parsed$regions) < 2) {
      showNotification("Invalid region definitions file. Please select a valid exported JSON file.", type = "error", duration = 6)
      return()
    }
    
    withProgress(message = "Importing Region Definitions...", value = 0.1, {
      all_area_rows <- lapply(parsed$regions, function(reg_item) {
        if (length(reg_item$areas) == 0) return(NULL)
        bind_rows(lapply(reg_item$areas, as_tibble))
      })
      all_areas_df <- bind_rows(all_area_rows)
      
      layer_lookup <- list()
      if (nrow(all_areas_df) > 0) {
        unique_pairs <- all_areas_df |> distinct(SRC_STATE, SRC_GEOM)
        for (k in seq_len(nrow(unique_pairs))) {
          st_k <- unique_pairs$SRC_STATE[k]
          gm_k <- unique_pairs$SRC_GEOM[k]
          incProgress(0.7 / nrow(unique_pairs), detail = paste("Fetching", gm_k, "for", st_k))
          layer_lookup[[paste(st_k, gm_k, sep = "::")]] <- build_standardized_layer(gm_k, st_k)
        }
      }
      
      new_regs <- list()
      max_r_idx <- 0
      
      for (idx in seq_along(parsed$regions)) {
        reg_item <- parsed$regions[[idx]]
        rid <- if (!is.null(reg_item$id)) as.character(reg_item$id) else paste0("R", idx)
        num_part <- suppressWarnings(as.integer(sub("^R", "", rid)))
        if (!is.na(num_part) && num_part > max_r_idx) max_r_idx <- num_part
        
        col_val <- if (!is.null(reg_item$color)) {
          as.character(reg_item$color)
        } else {
          region_palette[((idx - 1) %% length(region_palette)) + 1]
        }
        
        reg_sf <- NULL
        if (length(reg_item$areas) > 0) {
          reg_areas_df <- bind_rows(lapply(reg_item$areas, as_tibble)) |>
            mutate(PAIR_KEY = paste(SRC_STATE, SRC_GEOM, sep = "::"))
          
          sf_pieces <- lapply(split(reg_areas_df, reg_areas_df$PAIR_KEY), function(sub_df) {
            base_lyr <- layer_lookup[[sub_df$PAIR_KEY[1]]]
            if (is.null(base_lyr)) return(NULL)
            base_lyr |> filter(UNIQUE_KEY %in% sub_df$UNIQUE_KEY)
          })
          sf_pieces <- Filter(Negate(is.null), sf_pieces)
          if (length(sf_pieces) > 0) {
            reg_sf <- do.call(rbind, sf_pieces)
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
        updateCheckboxInput(session, "exclude_higher_overlap", value = isTRUE(parsed$exclude_higher_overlap))
      }
      
      imported_sfs <- Filter(function(x) !is.null(x) && nrow(x) > 0, lapply(new_regs, function(r) r$sf))
      if (length(imported_sfs) > 0) {
        combined_imp <- do.call(rbind, imported_sfs)
        updateSelectInput(session, "state", selected = combined_imp$SRC_STATE[1])
        updateSelectInput(session, "geometry", selected = combined_imp$SRC_GEOM[1])
        bbox <- as.numeric(st_bbox(combined_imp))
        maplibre_proxy("selector_map") |> fit_bounds(bbox, padding = 30)
      }
      
      showNotification(
        paste0("Imported ", length(new_regs), " custom regions successfully."),
        type = "message",
        duration = 4
      )
    })
  })
  
  # ----------------------------------------------------------------------------
  # Dynamic Region Management
  # ----------------------------------------------------------------------------
  observeEvent(input$add_region_btn, {
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
    updateSelectInput(session, "target_region", selected = new_id)
  })
  
  observeEvent(input$update_names_btn, {
    regs <- sync_region_names(regions_store())
    regions_store(regs)
    showNotification("Region names updated.", type = "message", duration = 2)
  })
  
  observeEvent(regions_store(), {
    regs <- regions_store()
    choices <- setNames(
      names(regs),
      vapply(regs, function(r) if (nzchar(trimws(r$name))) trimws(r$name) else r$id, character(1))
    )
    cur_sel <- isolate(input$target_region)
    if (is.null(cur_sel) || !cur_sel %in% names(regs)) {
      cur_sel <- names(regs)[1]
    }
    updateSelectInput(session, "target_region", choices = choices, selected = cur_sel)
  })
  
  output$dynamic_regions_ui <- renderUI({
    regs <- regions_store()
    can_delete <- length(regs) > 2
    
    lapply(seq_along(regs), function(idx) {
      r <- regs[[idx]]
      rid <- r$id
      n_areas <- if (is.null(r$sf)) 0 else nrow(r$sf)
      states_str <- if (n_areas > 0) paste0(" across ", paste(unique(r$sf$SRC_STATE), collapse = ", ")) else ""
      
      div(
        class = "region-card",
        style = paste0("border-left: 5px solid ", r$color, ";"),
        div(
          style = "display: flex; gap: 6px; align-items: center; margin-bottom: 3px;",
          textInput(
            inputId = paste0("name_", rid),
            label = NULL,
            value = r$name,
            width = "100%"
          ),
          if (can_delete) {
            actionButton(
              inputId = paste0("del_", rid),
              label = "✕",
              class = "btn-outline-danger btn-sm",
              style = "padding: 2px 7px; margin-bottom: 14px;",
              title = "Delete Region"
            )
          }
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center; font-size: 0.82em; color: #475569; margin-top: -10px;",
          span(paste0("Priority #", idx, " • ", n_areas, " area(s)", states_str)),
          if (n_areas > 0) {
            div(
              style = "display: flex; gap: 8px; align-items: center;",
              actionLink(
                inputId = paste0("stage_", rid),
                label = "Edit Areas",
                style = "color: #2563eb; font-weight: 600;",
                title = "Move assigned areas back to Staged Selection to add or remove geographies"
              ),
              span("|", style = "color: #cbd5e1;"),
              actionLink(
                inputId = paste0("reset_", rid),
                label = "Clear",
                style = "color: #dc2626;",
                title = "Remove all areas from this region"
              )
            )
          } else {
            span("Empty", style = "color: #94a3b8; font-style: italic;")
          }
        )
      )
    })
  })
  
  observe({
    regs <- regions_store()
    for (rid in names(regs)) {
      local({
        id <- rid
        
        observeEvent(input[[paste0("stage_", id)]], {
          cur <- sync_region_names(regions_store())
          req(!is.null(cur[[id]]), !is.null(cur[[id]]$sf), nrow(cur[[id]]$sf) > 0)
          
          moved_sf <- cur[[id]]$sf
          reg_label <- cur[[id]]$name
          
          existing_staged <- staged_sf()
          if (is.null(existing_staged) || nrow(existing_staged) == 0) {
            staged_sf(moved_sf)
          } else {
            add_rows <- moved_sf |> filter(!UNIQUE_KEY %in% existing_staged$UNIQUE_KEY)
            staged_sf(rbind(existing_staged, add_rows))
          }
          
          cur[[id]]$sf <- NULL
          regions_store(cur)
          
          updateSelectInput(session, "target_region", selected = id)
          
          updateTabsetPanel(session, "main_tabs", selected = "selector_tab")
          updateSelectInput(session, "state", selected = moved_sf$SRC_STATE[1])
          updateSelectInput(session, "geometry", selected = moved_sf$SRC_GEOM[1])
          bbox <- as.numeric(st_bbox(moved_sf))
          maplibre_proxy("selector_map") |> fit_bounds(bbox, padding = 30)
          
          showNotification(
            paste0("Moved ", nrow(moved_sf), " area(s) from '", reg_label, "' to Staged Selection. Click polygons to add/remove, then click 'Add Areas'."),
            type = "message",
            duration = 5
          )
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("reset_", id)]], {
          cur <- sync_region_names(regions_store())
          if (!is.null(cur[[id]])) {
            cur[[id]]$sf <- NULL
            regions_store(cur)
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("del_", id)]], {
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
  current_layer <- reactive({
    req(input$state, input$geometry)
    showNotification(paste("Loading", input$geometry, "for", input$state, "..."), id = "geo_load", duration = NULL)
    on.exit(removeNotification("geo_load"), add = TRUE)
    
    build_standardized_layer(input$geometry, input$state)
  })
  
  # Initialize MapLibre Selector Map directly with sf source
  output$selector_map <- renderMaplibre({
    layer <- isolate(current_layer()) |>
      mutate(
        fill_col = "#94a3b8",
        fill_op  = 0.2
      )
    
    maplibre(style = carto_style("positron"), bounds = as.numeric(st_bbox(layer))) |>
      add_navigation_control(position = "top-left") |>
      add_draw_control(
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
      add_fill_layer(
        id = "base_fill",
        source = layer,
        fill_color = get_column("fill_col"),
        fill_opacity = get_column("fill_op"),
        tooltip = "AREA_LABEL",
        hover_options = list(fill_opacity = 0.85)
      ) |>
      add_line_layer(
        id = "base_line",
        source = layer,
        line_color = "#334155",
        line_width = 1
      )
  })
  
  observeEvent(current_layer(), {
    bbox <- as.numeric(st_bbox(current_layer()))
    maplibre_proxy("selector_map") |>
      fit_bounds(bbox, padding = 30)
  }, ignoreInit = TRUE)
  
  observe({
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
      mutate(
        saved_col = unname(key_color_map[UNIQUE_KEY]),
        fill_col = case_when(
          UNIQUE_KEY %in% staged_keys ~ "#e74c3c",
          !is.na(saved_col)           ~ saved_col,
          TRUE                        ~ "#94a3b8"
        ),
        fill_op = case_when(
          UNIQUE_KEY %in% staged_keys | !is.na(saved_col) ~ 0.65,
          TRUE ~ 0.2
        )
      )
    
    legend_vals <- c("Staged Selection", vapply(regs, function(r) r$name, character(1)), "Unselected")
    legend_cols <- c("#e74c3c", vapply(regs, function(r) r$color, character(1)), "#94a3b8")
    
    proxy <- maplibre_proxy("selector_map") |>
      clear_layer("base_fill") |>
      clear_layer("base_line") |>
      clear_layer("other_regions_fill") |>
      clear_layer("other_staged_fill") |>
      add_fill_layer(
        id = "base_fill",
        source = layer,
        fill_color = get_column("fill_col"),
        fill_opacity = get_column("fill_op"),
        tooltip = "AREA_LABEL",
        hover_options = list(fill_opacity = 0.85)
      ) |>
      add_line_layer(
        id = "base_line",
        source = layer,
        line_color = "#334155",
        line_width = 1
      ) |>
      add_legend(
        legend_title = "Selection Status",
        values = unname(legend_vals),
        colors = unname(legend_cols),
        type = "categorical",
        position = "bottom-right"
      )
    
    other_list <- lapply(rev(regs), function(r) {
      if (!is.null(r$sf) && nrow(r$sf) > 0) {
        sub_sf <- r$sf |> filter(!UNIQUE_KEY %in% layer$UNIQUE_KEY)
        if (nrow(sub_sf) > 0) {
          sub_sf |> mutate(reg_col = r$color, reg_tip = paste0(r$name, ": ", AREA_LABEL))
        } else NULL
      } else NULL
    })
    other_sf <- do.call(rbind, Filter(Negate(is.null), other_list))
    
    if (!is.null(other_sf) && nrow(other_sf) > 0) {
      proxy <- proxy |>
        add_fill_layer(
          id = "other_regions_fill",
          source = other_sf,
          fill_color = get_column("reg_col"),
          fill_opacity = 0.55,
          tooltip = "reg_tip"
        )
    }
    
    if (!is.null(staged) && nrow(staged) > 0) {
      other_staged <- staged |>
        filter(!UNIQUE_KEY %in% layer$UNIQUE_KEY) |>
        mutate(staged_tip = paste0("Staged: ", AREA_LABEL))
      if (nrow(other_staged) > 0) {
        proxy <- proxy |>
          add_fill_layer(
            id = "other_staged_fill",
            source = other_staged,
            fill_color = "#e74c3c",
            fill_opacity = 0.65,
            tooltip = "staged_tip"
          )
      }
    }
  })
  
  observeEvent(input$selector_map_feature_click, {
    click <- input$selector_map_feature_click
    req(!is.null(click$lng), !is.null(click$lat))
    
    layer <- current_layer()
    pt <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
    hit_idx <- suppressMessages(which(st_intersects(layer, pt, sparse = FALSE)[, 1]))
    req(length(hit_idx) > 0)
    
    clicked_row <- layer[hit_idx[1], ]
    clicked_key <- clicked_row$UNIQUE_KEY
    
    cur <- staged_sf()
    if (is.null(cur) || nrow(cur) == 0) {
      staged_sf(clicked_row)
    } else if (clicked_key %in% cur$UNIQUE_KEY) {
      rem <- cur |> filter(UNIQUE_KEY != clicked_key)
      staged_sf(if (nrow(rem) == 0) NULL else rem)
    } else {
      staged_sf(rbind(cur, clicked_row))
    }
  })
  
  observeEvent(input$selector_map_drawn_features, {
    drawn <- get_drawn_features(maplibre_proxy("selector_map"))
    if (is.null(drawn) || nrow(drawn) == 0) {
      req(input$selector_map_drawn_features)
      drawn <- tryCatch(
        sf::st_read(input$selector_map_drawn_features, quiet = TRUE),
        error = function(e) NULL
      )
    }
    req(!is.null(drawn), nrow(drawn) > 0)
    
    layer <- current_layer()
    drawn <- sf::st_transform(drawn, sf::st_crs(layer)) |> sf::st_make_valid()
    layer_valid <- sf::st_make_valid(layer)
    
    hits <- suppressMessages(sf::st_intersects(layer_valid, drawn, sparse = FALSE))
    hit_mask <- apply(hits, 1, any)
    hit_rows <- layer[hit_mask, ]
    
    if (nrow(hit_rows) > 0) {
      cur <- staged_sf()
      if (is.null(cur) || nrow(cur) == 0) {
        staged_sf(hit_rows)
      } else {
        new_rows <- hit_rows |> filter(!UNIQUE_KEY %in% cur$UNIQUE_KEY)
        staged_sf(rbind(cur, new_rows))
      }
    }
    
    clear_drawn_features(maplibre_proxy("selector_map"))
  })
  
  output$staged_summary <- renderText({
    cur <- staged_sf()
    n <- if (is.null(cur)) 0 else nrow(cur)
    paste0("Staged Selection: ", n, " area(s) ready")
  })
  
  observeEvent(input$clear_staged, {
    staged_sf(NULL)
    clear_drawn_features(maplibre_proxy("selector_map"))
  })
  
  observeEvent(input$assign_staged, {
    cur <- staged_sf()
    tid <- input$target_region
    req(!is.null(cur), nrow(cur) > 0, !is.null(tid))
    
    regs <- sync_region_names(regions_store())
    req(!is.null(regs[[tid]]))
    
    existing <- regs[[tid]]$sf
    if (is.null(existing) || nrow(existing) == 0) {
      regs[[tid]]$sf <- cur
    } else {
      add_rows <- cur |> filter(!UNIQUE_KEY %in% existing$UNIQUE_KEY)
      regs[[tid]]$sf <- rbind(existing, add_rows)
    }
    regions_store(regs)
    staged_sf(NULL)
  })
  
  # ----------------------------------------------------------------------------
  # Fetch LODES RAC + WAC + OD & Aggregate Across All Custom Regions
  # ----------------------------------------------------------------------------
  observeEvent(input$run_comparison, {
    sf::sf_use_s2(FALSE)
    
    regs <- sync_region_names(regions_store())
    regions_store(regs)
    
    empty_regs <- Filter(function(r) is.null(r$sf) || nrow(r$sf) == 0, regs)
    
    if (length(empty_regs) > 0) {
      missing_names <- paste(vapply(empty_regs, function(r) r$name, character(1)), collapse = ", ")
      showNotification(paste0("Please assign at least one area to: ", missing_names), type = "error", duration = 6)
      return()
    }
    
    year_val        <- input$year
    jt_val          <- input$job_type
    seg_val         <- input$segment
    seg_lbl         <- names(segment_choices)[segment_choices == seg_val]
    exclude_overlap <- isTRUE(input$exclude_higher_overlap)
    
    all_states <- unique(unlist(lapply(regs, function(r) r$sf$SRC_STATE)))
    
    withProgress(message = "Building Multi-Region RAC, WAC & OD Profiles...", value = 0, {
      incProgress(0.1, detail = "Dissolving custom region geometries")
      
      dissolved_list <- list()
      base_parts_list <- list()
      cumulative_higher_geom <- NULL
      
      for (idx in seq_along(regs)) {
        r <- regs[[idx]]
        r_label <- if (nzchar(trimws(r$name))) trimws(r$name) else r$id
        base_parts_list[[idx]] <- r$sf |> select(UNIQUE_KEY, geometry)
        r_geom <- dissolve_quietly(r$sf)
        
        if (exclude_overlap && !is.null(cumulative_higher_geom)) {
          r_geom <- difference_quietly(r_geom, cumulative_higher_geom)
          if (length(r_geom) == 0 || all(st_is_empty(r_geom))) {
            showNotification(
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
        
        dissolved_list[[idx]] <- st_sf(
          region_id    = r$id,
          region_name  = r_label,
          region_color = r$color,
          priority     = idx,
          geometry     = r_geom
        )
      }
      
      regions_poly <- do.call(rbind, rev(dissolved_list))
      base_parts_sf <- do.call(rbind, base_parts_list) |> distinct(UNIQUE_KEY, .keep_all = TRUE)
      
      state_wac_list <- list()
      state_rac_list <- list()
      all_block_lookup_list <- list()
      od_raw_list <- list()
      
      for (i in seq_along(all_states)) {
        st <- all_states[i]
        st_lower <- tolower(st)
        incProgress(0.65 / length(all_states), detail = paste("Downloading RAC, WAC & OD for", st))
        
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
        
        if (!file.exists(xwalk_dest))   download.file(xwalk_url, xwalk_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(wac_dest))     download.file(wac_url, wac_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(rac_dest))     download.file(rac_url, rac_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(od_main_dest)) try(download.file(od_main_url, od_main_dest, mode = "wb", quiet = TRUE), silent = TRUE)
        if (!file.exists(od_aux_dest))  try(download.file(od_aux_url, od_aux_dest, mode = "wb", quiet = TRUE), silent = TRUE)
        
        xwalk_df <- read_csv(
          xwalk_dest,
          col_types = cols_only(tabblk2020 = "c", blklatdd = "d", blklondd = "d")
        ) |> filter(!is.na(blklatdd), !is.na(blklondd))
        
        blk_pts <- st_as_sf(xwalk_df, coords = c("blklondd", "blklatdd"), crs = 4326)
        joined_pts <- suppressMessages(
          st_join(blk_pts, regions_poly, join = st_within, left = FALSE)
        ) |>
          st_drop_geometry() |>
          select(tabblk2020, region_id)
        
        highest_prio_pts <- suppressMessages(
          st_join(blk_pts, regions_poly |> arrange(priority), join = st_within, left = FALSE)
        ) |>
          st_drop_geometry() |>
          distinct(tabblk2020, .keep_all = TRUE) |>
          select(tabblk2020, region_id)
        all_block_lookup_list[[st]] <- highest_prio_pts
        
        if (nrow(joined_pts) > 0) {
          wac_raw <- read_csv(wac_dest, col_types = cols(.default = "d", w_geocode = "c"))
          rac_raw <- read_csv(rac_dest, col_types = cols(.default = "d", h_geocode = "c"))
          
          wac_blk <- summarize_lodes_blocks(wac_raw, "w_geocode") |>
            inner_join(joined_pts, by = "tabblk2020", relationship = "many-to-many") |>
            group_by(region_id) |>
            summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
          
          rac_blk <- summarize_lodes_blocks(rac_raw, "h_geocode") |>
            inner_join(joined_pts, by = "tabblk2020", relationship = "many-to-many") |>
            group_by(region_id) |>
            summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
          
          state_wac_list[[st]] <- wac_blk
          state_rac_list[[st]] <- rac_blk
        }
        
        od_cols <- cols_only(
          w_geocode = "c", h_geocode = "c",
          S000 = "d", SA01 = "d", SA02 = "d", SA03 = "d",
          SE01 = "d", SE02 = "d", SE03 = "d",
          SI01 = "d", SI02 = "d", SI03 = "d"
        )
        if (file.exists(od_main_dest)) {
          od_raw_list[[paste0(st, "_main")]] <- read_csv(od_main_dest, col_types = od_cols)
        }
        if (file.exists(od_aux_dest) && length(all_states) > 1) {
          od_raw_list[[paste0(st, "_aux")]] <- read_csv(od_aux_dest, col_types = od_cols)
        }
      }
      
      incProgress(0.15, detail = "Compiling WAC, RAC & OD commuting matrices")
      
      reg_meta <- tibble(
        region_id    = vapply(regs, function(r) r$id, character(1)),
        region_name  = vapply(regs, function(r) if (nzchar(trimws(r$name))) trimws(r$name) else r$id, character(1)),
        region_color = vapply(regs, function(r) r$color, character(1))
      )
      
      wac_totals <- bind_rows(state_wac_list) |>
        group_by(region_id) |>
        summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      wac_totals <- reg_meta |> left_join(wac_totals, by = "region_id") |> mutate(across(total_jobs:sex_female, ~coalesce(., 0)))
      
      rac_totals <- bind_rows(state_rac_list) |>
        group_by(region_id) |>
        summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      rac_totals <- reg_meta |> left_join(rac_totals, by = "region_id") |> mutate(across(total_jobs:sex_female, ~coalesce(., 0)))
      
      block_lookup <- bind_rows(all_block_lookup_list) |> distinct(tabblk2020, .keep_all = TRUE)
      id_to_name <- setNames(reg_meta$region_name, reg_meta$region_id)
      
      od_flows <- bind_rows(od_raw_list) |>
        left_join(block_lookup |> rename(h_geocode = tabblk2020, origin_id = region_id), by = "h_geocode") |>
        left_join(block_lookup |> rename(w_geocode = tabblk2020, dest_id   = region_id), by = "w_geocode") |>
        filter(!is.na(origin_id) | !is.na(dest_id)) |>
        mutate(
          origin_id   = coalesce(origin_id, "OUTSIDE"),
          dest_id     = coalesce(dest_id, "OUTSIDE"),
          Origin      = ifelse(origin_id == "OUTSIDE", "Outside Selected Regions", unname(id_to_name[origin_id])),
          Destination = ifelse(dest_id == "OUTSIDE", "Outside Selected Regions", unname(id_to_name[dest_id]))
        ) |>
        group_by(origin_id, Origin, dest_id, Destination) |>
        summarise(
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
        group_by(origin_id) |>
        mutate(
          origin_tot = sum(Total_Jobs, na.rm = TRUE),
          Share_of_Origin_Workers = ifelse(origin_tot > 0, Total_Jobs / origin_tot, 0)
        ) |>
        select(-origin_tot) |>
        ungroup() |>
        group_by(dest_id) |>
        mutate(
          dest_tot = sum(Total_Jobs, na.rm = TRUE),
          Share_of_Dest_Jobs = ifelse(dest_tot > 0, Total_Jobs / dest_tot, 0)
        ) |>
        select(-dest_tot) |>
        ungroup()
      
      comparison_results(list(
        reg_meta        = reg_meta,
        regs_snapshot   = regs,
        exclude_overlap = exclude_overlap,
        year            = year_val,
        job_type_code   = jt_val,
        segment_code    = seg_val,
        job_type        = names(job_type_choices)[job_type_choices == jt_val],
        segment_label   = seg_lbl,
        regions_poly    = regions_poly,
        base_parts_sf   = base_parts_sf,
        wac_totals      = wac_totals,
        rac_totals      = rac_totals,
        od_flows        = od_flows
      ))
      
      updateTabsetPanel(session, "main_tabs", selected = "report_tab")
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
      
      as_tibble(row_list)
    }) |> bind_rows()
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
    
    datatable(
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
      formatCurrency(count_cols, currency = "", digits = 0) |>
      formatPercentage(share_cols, digits = 1)
  }
  
  # ----------------------------------------------------------------------------
  # Render Comparison Summary Report (Tab 2)
  # ----------------------------------------------------------------------------
  output$report_ui <- renderUI({
    res <- comparison_results()
    if (is.null(res)) {
      return(div(
        style = "padding: 40px; text-align: center; color: #64748b;",
        h4("No Comparison Generated Yet"),
        p("Use the MapLibre map in Tab 1 to build your custom regions, then click 'Fetch RAC, WAC & OD Data'.")
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
      
      div(
        style = paste0("background: #f8fafc; border-left: 5px solid ", rcol, "; padding: 10px 14px; border-radius: 6px;"),
        div(style = "font-weight: 700; color: #0f172a; font-size: 1.05em; margin-bottom: 4px;", rname),
        div(style = "font-size: 0.92em; color: #0284c7;", HTML(paste0("Workplace (WAC): <b>", scales::comma(w_tot), "</b>"))),
        div(style = "font-size: 0.92em; color: #059669;", HTML(paste0("Residence (RAC): <b>", scales::comma(r_tot), "</b>")))
      )
    })
    
    tagList(
      div(
        style = "padding: 16px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; margin-bottom: 14px;",
          div(
            h4(paste(meta$region_name, collapse = " vs. "), style = "margin: 0; color: #1e3a8a; font-weight: 700;"),
            p(style = "margin: 2px 0 0 0; color: #475569; font-size: 0.92em;",
              paste0("Year: ", res$year, " | Job Type: ", res$job_type, " | Segment: ", res$segment_label, overlap_note))
          ),
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            downloadButton("download_pdf_report", "Download Executive PDF Report", class = "btn-danger btn-sm"),
            downloadButton("download_report_csv", "Download RAC & WAC CSV", class = "btn-outline-primary btn-sm"),
            downloadButton("download_r_script", "Download Standalone R Script", class = "btn-outline-dark btn-sm")
          )
        ),
        
        layout_column_wrap(
          width = "240px",
          !!!kpi_cards
        ),
        
        hr(),
        
        h5("Area Characteristics by Work Location (WAC)", style = "color: #0284c7; font-weight: 700;"),
        DTOutput("wac_table"),
        
        hr(),
        
        h5("Area Characteristics by Home Location (RAC)", style = "color: #059669; font-weight: 700;"),
        DTOutput("rac_table")
      )
    )
  })
  
  output$wac_table <- renderDT({
    res <- comparison_results()
    req(res)
    df <- build_grouped_table_data(res$wac_totals)
    format_dt_grouped(df, res$reg_meta)
  })
  
  output$rac_table <- renderDT({
    res <- comparison_results()
    req(res)
    df <- build_grouped_table_data(res$rac_totals)
    format_dt_grouped(df, res$reg_meta)
  })
  
  output$download_report_csv <- downloadHandler(
    filename = function() {
      paste0("lodes_multi_region_rac_wac_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- comparison_results()
      req(res)
      meta <- res$reg_meta
      
      wac_df <- build_grouped_table_data(res$wac_totals) |> mutate(Dataset = "WAC (Work Location)", .before = 1)
      rac_df <- build_grouped_table_data(res$rac_totals) |> mutate(Dataset = "RAC (Home Location)", .before = 1)
      out <- bind_rows(wac_df, rac_df)
      
      col_names <- c(
        "Dataset",
        "Category",
        "Characteristic",
        paste0("Count (#) - ", meta$region_name),
        paste0("Share (%) - ", meta$region_name)
      )
      
      names(out) <- col_names
      write_csv(out, file)
    }
  )
  
  # ----------------------------------------------------------------------------
  # Standalone R Script Download Handler
  # ----------------------------------------------------------------------------
  output$download_r_script <- downloadHandler(
    filename = function() {
      paste0("run_lodes_regional_comparison_", Sys.Date(), ".R")
    },
    content = function(file) {
      res <- comparison_results()
      req(res)
      
      script_txt <- generate_standalone_r_script(
        regs            = res$regs_snapshot,
        year_val        = res$year,
        jt_val          = res$job_type_code,
        seg_val         = res$segment_code,
        exclude_overlap = res$exclude_overlap,
        pdf_dir         = pdf_base_dir
      )
      writeLines(script_txt, file)
    }
  )
  
  # ----------------------------------------------------------------------------
  # Executive PDF Report Generator (Map + Workforce Highlights + OD + Sources)
  # ----------------------------------------------------------------------------
  output$download_pdf_report <- downloadHandler(
    filename = function() {
      paste0("LODES_Regional_Comparison_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      res <- comparison_results()
      req(res)
      
      withProgress(message = "Rendering Printable PDF Report...", value = 0.2, {
        if (!dir.exists(pdf_base_dir)) {
          dir.create(pdf_base_dir, recursive = TRUE, showWarnings = FALSE)
        }
        work_dir <- file.path(pdf_base_dir, paste0("build_", as.integer(Sys.time())))
        dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
        work_dir <- normalizePath(work_dir, winslash = "/", mustWork = TRUE)
        
        on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
        
        data_rds <- file.path(work_dir, "report_data.rds")
        saveRDS(res, data_rds)
        
        incProgress(0.3, detail = "Generating map & summary tables")
        
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
          'title: "Regional Workforce & Commuting Comparison"',
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
          "library(dplyr)",
          "library(tidyr)",
          "library(sf)",
          "library(ggplot2)",
          "library(scales)",
          "library(knitr)",
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
          '  tex <- kable(df, format = "latex", booktabs = TRUE, longtable = TRUE)',
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
          "**Parameters:** Year `r res$year` | Job Type: `r res$job_type` | Workforce Segment: `r res$segment_label` | Overlap Mode: `r ifelse(res$exclude_overlap, 'Higher-priority regions excluded from lower regions', 'Full selected boundaries included')`",
          "",
          "## 1. Custom Regions Map & Headline Totals",
          "",
          paste0(bt, map_chunk_hdr),
          "polys_plot <- res$regions_poly |> arrange(priority)",
          "polys_plot$region_name <- factor(polys_plot$region_name, levels = meta$region_name)",
          "pal_map <- setNames(meta$region_color, meta$region_name)",
          "",
          "p <- ggplot() +",
          '  geom_sf(data = res$base_parts_sf, fill = "#f8fafc", color = "#cbd5e1", linewidth = 0.25)',
          "for (idx in rev(seq_len(nrow(polys_plot)))) {",
          '  p <- p + geom_sf(data = polys_plot[idx, ], aes(fill = region_name), color = "#0f172a", linewidth = 0.45, alpha = 0.72)',
          "}",
          'p + scale_fill_manual(values = pal_map, name = "Custom Regions") +',
          "  theme_void() +",
          "  theme(",
          '    legend.position = "right",',
          '    legend.title = element_text(face = "bold", size = 10),',
          "    legend.text = element_text(size = 9),",
          "    plot.margin = margin(4, 4, 4, 4)",
          "  )",
          bt,
          "",
          paste0(bt, "{r kpi_table}"),
          "kpi_df <- tibble(",
          "  Region = meta$region_name,",
          "  `Jobs by Work Location` = res$wac_totals$total_jobs,",
          "  `Workers by Home Location` = res$rac_totals$total_jobs",
          ") |>",
          "  mutate(",
          "    `Jobs-to-Workers Ratio` = ifelse(`Workers by Home Location` > 0, round(`Jobs by Work Location` / `Workers by Home Location`, 2), NA_real_),",
          "    `Jobs by Work Location` = comma(`Jobs by Work Location`),",
          "    `Workers by Home Location` = comma(`Workers by Home Location`)",
          "  )",
          'kable(kpi_df, booktabs = TRUE, align = c("l", "r", "r", "r"))',
          bt,
          "",
          "## 2. Area Characteristics by Work Location (WAC)",
          "",
          "*Demographic and industry breakdown of jobs located inside each region (values sorted descending within each category).*",
          "",
          paste0(bt, "{r wac_highlights, results='asis'}"),
          "wac_hi <- summarize_top_groups(res$wac_totals, meta) |>",
          "  pivot_wider(names_from = Region, values_from = Highlight)",
          "render_wrapped_longtable(wac_hi)",
          bt,
          "",
          "*Notes: Categories showing a subset display the largest non-zero groups in descending order: Top 2 groups for Age, Education, and Race; and Top 3 sectors for 2-digit NAICS codes (see Section 5 for sector titles). Active categories are shown for Earnings, Broad Industry, and Sex. Ethnicity displays the Hispanic or Latino share. Monthly earnings tiers follow LEHD LODES definitions: Very Low = $1,250/month or less; Low = $1,251 to $3,333/month; Average = greater than $3,333/month.*",
          "",
          "## 3. Area Characteristics by Home Location (RAC)",
          "",
          "*Demographic and industry breakdown of employed residents living inside each region (values sorted descending within each category).*",
          "",
          paste0(bt, "{r rac_highlights, results='asis'}"),
          "rac_hi <- summarize_top_groups(res$rac_totals, meta) |>",
          "  pivot_wider(names_from = Region, values_from = Highlight)",
          "render_wrapped_longtable(rac_hi)",
          bt,
          "",
          "*Notes: Categories showing a subset display the largest non-zero groups in descending order: Top 2 groups for Age, Education, and Race; and Top 3 sectors for 2-digit NAICS codes (see Section 5 for sector titles). Active categories are shown for Earnings, Broad Industry, and Sex. Ethnicity displays the Hispanic or Latino share. Monthly earnings tiers follow LEHD LODES definitions: Very Low = $1,250/month or less; Low = $1,251 to $3,333/month; Average = greater than $3,333/month.*",
          "",
          "## 4. Commuting Flows Between Regions (Origin-Destination)",
          "",
          "*Origin (rows) = worker home location; Destination (columns) = worker work location (All Workers S000).*",
          "",
          paste0(bt, "{r od_matrix, results='asis'}"),
          "od <- res$od_flows",
          'all_ids   <- c(meta$region_id, "OUTSIDE")',
          'all_names <- c(meta$region_name, "Outside Selected Regions")',
          "od_mat <- lapply(seq_len(nrow(meta)), function(i) {",
          "  oid <- meta$region_id[i]",
          "  sub_od <- od |> filter(origin_id == oid)",
          "  tot_res <- sum(sub_od$Total_Jobs, na.rm = TRUE)",
          "  row_vals <- list(`Home Origin` = meta$region_name[i], `Total Resident Workers` = comma(tot_res))",
          "  for (j in seq_along(all_ids)) {",
          "    did <- all_ids[j]",
          "    v <- sub_od$Total_Jobs[sub_od$dest_id == did]",
          "    v <- if (length(v) == 0) 0 else v[1]",
          '    pct <- if (tot_res > 0) percent(v / tot_res, accuracy = 0.1) else "0.0%"',
          '    row_vals[[all_names[j]]] <- paste0(comma(v), " (", pct, ")")',
          "  }",
          "  as_tibble(row_vals)",
          "}) |> bind_rows()",
          'od_tex <- kable(od_mat, format = "latex", booktabs = TRUE, align = c("l", rep("r", ncol(od_mat) - 1)))',
          'cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", od_tex, "\\n}\\n\\\\end{table}\\n")',
          bt,
          "",
          "### Top Inter-Regional & Internal Commuting Corridors",
          "",
          paste0(bt, "{r od_top_pairs, results='asis'}"),
          "filtered_od <- od |>",
          '  filter(origin_id != "OUTSIDE" | dest_id != "OUTSIDE") |>',
          "  arrange(desc(Total_Jobs))",
          "total_corridors <- nrow(filtered_od)",
          "top_od <- filtered_od |>",
          "  slice_head(n = 20) |>",
          "  transmute(",
          "    `Home Origin` = Origin,",
          "    `Work Destination` = Destination,",
          "    `Commuters` = comma(Total_Jobs),",
          "    `Origin %` = percent(Share_of_Origin_Workers, accuracy = 0.1),",
          "    `Dest. %` = percent(Share_of_Dest_Jobs, accuracy = 0.1),",
          "    `Goods Producing` = comma(Ind_Goods),",
          "    `Trade & Transp.` = comma(Ind_Trade),",
          "    `Services` = comma(Ind_Services)",
          "  )",
          'cor_tex <- kable(top_od, format = "latex", booktabs = TRUE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))',
          "cor_lines <- strsplit(cor_tex, '\\n', fixed = TRUE)[[1]]",
          "tab_idx <- which(startsWith(cor_lines, '\\\\begin{tabular}'))",
          "if (length(tab_idx) > 0) {",
          "  cor_lines[tab_idx[1]] <- '\\\\begin{tabular}[t]{>{\\\\raggedright\\\\arraybackslash}p{1.2in}>{\\\\raggedright\\\\arraybackslash}p{1.2in}rrrrrr}'",
          "}",
          "cor_tex <- paste(cor_lines, collapse = '\\n')",
          'cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", cor_tex, "\\n}\\n\\\\end{table}\\n")',
          "if (total_corridors > 20) {",
          '  cat(sprintf("\\n*Note: Showing the top 20 commuting corridors out of %s total available origin-destination pairs.*\\n", comma(total_corridors)))',
          "}",
          bt,
          "",
          "## 5. Data Sources & Definitions",
          "",
          "All workforce and commuting statistics are derived from the **U.S. Census Bureau Longitudinal Employer-Household Dynamics (LEHD) Origin-Destination Employment Statistics (LODES, Version 8)** program:",
          "",
          "- **Work Location Characteristics (WAC — Workplace Area Characteristics):** Summarizes employment by the Census block where workers are employed, including worker age, monthly earnings, educational attainment, race, ethnicity, sex, and 2-digit NAICS industry sector.",
          "- **Home Location Characteristics (RAC — Residence Area Characteristics):** Summarizes employed workers by the Census block where they reside across the same demographic and industry dimensions.",
          "- **Commuting Flows (OD — Origin-Destination):** Links workers' home Census blocks (origin) to their employment Census blocks (destination) for both within-state (`od_main`) and cross-state (`od_aux`) commuters, disaggregated by worker age, monthly earnings tier, and broad industry sector (Goods Producing, Trade/Transportation/Utilities, and All Other Services).",
          "- **Program & Data Links:**",
          "  - LEHD Program Overview: https://lehd.ces.census.gov/",
          "  - LODES Data Repository & Technical Documentation: https://lehd.ces.census.gov/data/",
          "",
          "### 2-Digit NAICS Industry Sector Reference",
          "",
          paste0(bt, "{r naics_ref_table, results='asis'}"),
          "naics_df <- metric_catalog |>",
          '  filter(category == "2-Digit NAICS") |>',
          "  mutate(",
          '    Code = sub(":.*$", "", label),',
          '    Sector = sub("^[^:]+:\\\\s*", "", label),',
          "    Broad = case_when(",
          '      var %in% c("cns01", "cns02", "cns03", "cns04", "cns05") ~ "Goods Producing",',
          '      var %in% c("cns06", "cns07", "cns08") ~ "Trade & Transp.",',
          '      TRUE ~ "Services"',
          "    )",
          "  ) |>",
          "  select(Code, Sector, Broad)",
          "naics_wide <- bind_cols(naics_df[1:10, ], naics_df[11:20, ])",
          'names(naics_wide) <- c("NAICS Code", "Industry Sector", "Broad Sector", "NAICS Code ", "Industry Sector ", "Broad Sector ")',
          'n_tex <- kable(naics_wide, format = "latex", booktabs = TRUE, align = c("l", "l", "l", "l", "l", "l"))',
          'cat("\\\\begin{table}[H]\\n\\\\centering\\n\\\\resizebox{\\\\textwidth}{!}{\\n", n_tex, "\\n}\\n\\\\end{table}\\n")',
          bt
        )
        writeLines(rmd_lines, rmd_path)
        
        incProgress(0.4, detail = "Compiling PDF via LaTeX")
        
        render_env <- new.env(parent = globalenv())
        render_env$summarize_top_groups <- summarize_top_groups
        render_env$clean_pdf_label <- clean_pdf_label
        render_env$metric_catalog <- metric_catalog
        
        tryCatch({
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
          file.copy(rendered_pdf, file, overwrite = TRUE)
        }, error = function(e) {
          log_file <- file.path(work_dir, "comparison_report.log")
          log_hint <- ""
          if (file.exists(log_file)) {
            log_lines <- readLines(log_file, warn = FALSE)
            err_idx <- grep("^!|Error|not found|Permission", log_lines)
            if (length(err_idx) > 0) {
              keep_idx <- unique(pmin(length(log_lines), c(err_idx, err_idx + 1)))
              log_hint <- paste(" | Log:", paste(head(log_lines[keep_idx], 4), collapse = " ; "))
            }
          }
          message("PDF Render Error: ", e$message, log_hint)
          showNotification(
            paste0("PDF compilation failed: ", e$message, log_hint),
            type = "error",
            duration = 15
          )
        })
      })
    }
  )
  
  # ----------------------------------------------------------------------------
  # Render Dissolved Regional Profile Map (Tab 3)
  # ----------------------------------------------------------------------------
  output$profile_map <- renderMaplibre({
    res <- comparison_results()
    req(res)
    
    polys <- res$regions_poly
    meta  <- res$reg_meta
    
    popups <- vapply(polys$region_id, function(rid) {
      rname <- meta$region_name[meta$region_id == rid]
      build_region_popup(
        rname,
        res$wac_totals |> filter(region_id == rid),
        res$rac_totals |> filter(region_id == rid),
        res$segment_label
      )
    }, character(1))
    
    polys$popup_html <- popups
    
    m <- maplibre(style = carto_style("positron"), bounds = as.numeric(st_bbox(polys)))
    
    for (i in seq_len(nrow(polys))) {
      sub_poly <- polys[i, ]
      rid <- sub_poly$region_id
      m <- m |>
        add_fill_layer(
          id = paste0("profile_fill_", rid),
          source = sub_poly,
          fill_color = sub_poly$region_color,
          fill_opacity = 0.6,
          popup = "popup_html",
          tooltip = "region_name",
          hover_options = list(fill_opacity = 0.85)
        ) |>
        add_line_layer(
          id = paste0("profile_line_", rid),
          source = sub_poly,
          line_color = "#0f172a",
          line_width = 2
        )
    }
    
    m |>
      add_legend(
        legend_title = "Custom Regions (Click for RAC/WAC Profile)",
        values = unname(meta$region_name),
        colors = unname(meta$region_color),
        type = "categorical",
        position = "bottom-right"
      ) |>
      add_navigation_control(position = "top-right") |>
      add_fullscreen_control(position = "top-right") |>
      add_scale_control(position = "bottom-left")
  })
  
  # ----------------------------------------------------------------------------
  # Render Origin-Destination (OD) Commuting Flows (Tab 4)
  # ----------------------------------------------------------------------------
  output$od_ui <- renderUI({
    res <- comparison_results()
    if (is.null(res)) {
      return(div(
        style = "padding: 40px; text-align: center; color: #64748b;",
        h4("No Commuting Data Generated Yet"),
        p("Build your custom regions in Tab 1 and click 'Fetch RAC, WAC & OD Data' to analyze origin-destination worker flows.")
      ))
    }
    
    tagList(
      div(
        style = "padding: 16px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; margin-bottom: 12px;",
          div(
            h4("Origin-Destination (OD) Commuting Flows", style = "margin: 0; color: #1e3a8a; font-weight: 700;"),
            p(style = "margin: 2px 0 0 0; color: #475569; font-size: 0.92em;",
              paste0("Year: ", res$year, " | Job Type: ", res$job_type,
                     " (Note: LODES OD files cover all workers S000 broken out by age, earnings, and broad industry)"))
          ),
          downloadButton("download_od_csv", "Download OD Flows CSV", class = "btn-outline-primary btn-sm")
        ),
        
        h5("Commuting Origin-Destination Matrix (Residence → Workplace)", style = "color: #0284c7; font-weight: 700;"),
        p(class = "text-muted", style = "font-size: 0.85em;",
          "Rows represent where workers live (Origin); columns represent where they work (Destination)."),
        DTOutput("od_matrix_table"),
        
        hr(),
        
        h5("Detailed Pairwise Commuting Flows by Worker & Industry Group", style = "color: #059669; font-weight: 700;"),
        DTOutput("od_detail_table")
      )
    )
  })
  
  output$od_matrix_table <- renderDT({
    res <- comparison_results()
    req(res)
    meta <- res$reg_meta
    od   <- res$od_flows
    
    all_ids   <- c(meta$region_id, "OUTSIDE")
    all_names <- c(meta$region_name, "Outside Selected Regions")
    n_dest    <- length(all_ids)
    
    matrix_rows <- lapply(seq_len(nrow(meta)), function(i) {
      oid   <- meta$region_id[i]
      oname <- meta$region_name[i]
      sub_od <- od |> filter(origin_id == oid)
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
      
      as_tibble(row_list)
    }) |> bind_rows()
    
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
    
    datatable(
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
      formatCurrency(cnt_cols, currency = "", digits = 0) |>
      formatPercentage(shr_cols, digits = 1)
  })
  
  output$od_detail_table <- renderDT({
    res <- comparison_results()
    req(res)
    
    df <- res$od_flows |>
      select(
        Origin, Destination, Total_Jobs,
        Share_of_Origin_Workers, Share_of_Dest_Jobs,
        Age_Under_30, Age_30_to_54, Age_55_Plus,
        Earn_Low, Earn_Mid, Earn_High,
        Ind_Goods, Ind_Trade, Ind_Services
      ) |>
      arrange(desc(Total_Jobs))
    
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
    
    datatable(
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
      formatCurrency(num_cols, currency = "", digits = 0) |>
      formatPercentage(c("Share_of_Origin_Workers", "Share_of_Dest_Jobs"), digits = 1)
  })
  
  output$download_od_csv <- downloadHandler(
    filename = function() {
      paste0("lodes_od_commuting_flows_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- comparison_results()
      req(res)
      out <- res$od_flows |>
        select(
          Origin, Destination, Total_Jobs,
          Share_of_Origin_Workers, Share_of_Dest_Jobs,
          Age_Under_30, Age_30_to_54, Age_55_Plus,
          Earn_Low, Earn_Mid, Earn_High,
          Ind_Goods, Ind_Trade, Ind_Services
        ) |>
        arrange(desc(Total_Jobs))
      write_csv(out, file)
    }
  )
}

shinyApp(ui, server)