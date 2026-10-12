# ==============================================================================
# LODES Residence Area Characteristics (RAC) Explorer
#
# PACKAGE DEPLOYMENT NOTES:
# Ensure the following packages are added to the 'Imports' section of your
# package's DESCRIPTION file:
# Imports: shiny, dplyr, readr, tigris, sf, freestiler, mapgl, bslib, classInt, scales, httpuv, servr
# ==============================================================================

library(shiny)
library(dplyr)
library(readr)
library(tigris)
library(sf)
library(freestiler)
library(mapgl)
library(bslib)
library(classInt)
library(scales)
library(httpuv)
library(servr)

# ------------------------------------------------------------------------------
# Global Options & Initializations
# ------------------------------------------------------------------------------
options(tigris_use_cache = TRUE)
options(tigris_keep_resolution = FALSE)
options(timeout = 300)

# Stop any orphaned tile servers from previous R/Shiny sessions
try(servr::daemon_stop(), silent = TRUE)

get_data_cache_dir <- function(persistent = TRUE) {
  if (persistent) {
    dir_path <- tools::R_user_dir("lodes_rac_cache", which = "cache")
  } else {
    dir_path <- file.path(tempdir(), "lodes_temp_cache")
  }
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  return(dir_path)
}

persistent_cache_dir <- get_data_cache_dir(persistent = TRUE)

# Dynamically select an available TCP port to prevent port conflicts
tile_server <- tryCatch({
  serve_tiles(persistent_cache_dir, port = httpuv::randomPort())
}, error = function(e) {
  serve_tiles(persistent_cache_dir)
})

# Register graceful cleanup on app shutdown
onStop(function() {
  try(servr::daemon_stop(), silent = TRUE)
})

# ------------------------------------------------------------------------------
# Static Lookups & Dictionaries
# ------------------------------------------------------------------------------
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

geo_id_lookup <- c(
  "ctyname"     = "cty",
  "ctycsubname" = "ctycsub",
  "stplcname"   = "stplc",
  "trctname"    = "trct",
  "bgrp"        = "bgrp",
  "stcd119name" = "stcd119",
  "stslduname"  = "stsldu",
  "stsldlname"  = "stsldl"
)

var_labels <- c(
  "total_jobs"       = "Total Segment Resident Workers",
  "age_under_30"     = "Age: Under 30",
  "age_30_to_54"     = "Age: 30 to 54",
  "age_over_55"      = "Age: 55 and older",
  "earn_high"        = "Earnings: Average (Greater than $3,333/mo)",
  "earn_mid"         = "Earnings: Low ($1,250 to $3,333/mo)",
  "earn_low"         = "Earnings: Very Low ($1,250 or less/mo)",
  "race_white"       = "Race: White Alone",
  "race_black"       = "Race: Black or African American",
  "race_native"      = "Race: American Indian or Alaska Native",
  "race_asian"       = "Race: Asian Alone",
  "race_pacific"     = "Race: Native Hawaiian or Pacific Islander",
  "race_two_plus"    = "Race: Two or More Races",
  "eth_hispanic"     = "Ethnicity: Hispanic or Latino",
  "eth_not_hispanic" = "Ethnicity: Not Hispanic or Latino",
  "edu_less_hs"      = "Education: Less than High School",
  "edu_hs"           = "Education: High School or Equivalent",
  "edu_some_college" = "Education: Some College or Associate Degree",
  "edu_bachelors"    = "Education: Bachelor's Degree or Higher",
  "sex_male"         = "Sex: Male",
  "sex_female"       = "Sex: Female",
  "ind_service"      = "Industry: Services (Broad)",
  "ind_trade"        = "Industry: Trade and Transportation (Broad)",
  "ind_goods"        = "Industry: Goods Producing (Broad)",
  "cns01"            = "NAICS 11: Agriculture, Forestry, Fishing, and Hunting",
  "cns02"            = "NAICS 21: Mining, Oil and Gas Extraction",
  "cns03"            = "NAICS 22: Utilities",
  "cns04"            = "NAICS 23: Construction",
  "cns05"            = "NAICS 31-33: Manufacturing",
  "cns06"            = "NAICS 42: Wholesale Trade",
  "cns07"            = "NAICS 44-45: Retail Trade",
  "cns08"            = "NAICS 48-49: Transportation and Warehousing",
  "cns09"            = "NAICS 51: Information",
  "cns10"            = "NAICS 52: Finance and Insurance",
  "cns11"            = "NAICS 53: Real Estate and Rental/Leasing",
  "cns12"            = "NAICS 54: Professional, Scientific, and Tech",
  "cns13"            = "NAICS 55: Management of Companies",
  "cns14"            = "NAICS 56: Administrative and Waste Management",
  "cns15"            = "NAICS 61: Educational Services",
  "cns16"            = "NAICS 62: Health Care and Social Assistance",
  "cns17"            = "NAICS 71: Arts, Entertainment, and Recreation",
  "cns18"            = "NAICS 72: Accommodation and Food Services",
  "cns19"            = "NAICS 81: Other Services (except Public Admin)",
  "cns20"            = "NAICS 92: Public Administration"
)

# ------------------------------------------------------------------------------
# Map Helpers
# ------------------------------------------------------------------------------
get_dynamic_props <- function(data, column_name, n_classes = 6) {
  vals <- data[[column_name]]

  calc_breaks <- quantile(vals, probs = seq(0, 1, length.out = n_classes), na.rm = TRUE)
  breaks <- unique(as.numeric(calc_breaks))
  if (length(breaks) < 2) breaks <- c(0, max(vals, 1, na.rm = TRUE))

  base_colors <- c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")
  pal <- colorRampPalette(base_colors)(length(breaks))

  return(list(breaks = breaks, colors = pal))
}

build_popup_html <- function(selected_var, segment_label = "All Workers") {
  label_text <- var_labels[[selected_var]]

  dynamic_header <- ""
  if (selected_var != "total_jobs") {
    dynamic_header <- paste0(
      "<div style='font-size: 1.2em; font-weight: 600; color: #0284c7; margin-bottom: 6px;'>",
      label_text, ": <b>{", selected_var, "_fmt}</b> <i>({", selected_var, "_pct})</i></div>"
    )
  }

  paste0("
    <div style='font-family: system-ui, -apple-system, sans-serif; min-width: 460px; max-height: 625px; overflow-y: auto; padding-right: 6px; line-height: 1.4;'>
      <div style='font-size: 1.4em; font-weight: 700; color: #1e3a8a; margin-bottom: 2px;'>{display_name}</div>
      <div style='font-size: 1.2em; color: #4b5563; margin-bottom: 4px;'><b>Workforce Segment:</b> ", segment_label, "</div>
      <div style='font-size: 1.2em; color: #111827; margin-bottom: 2px;'>Total Segment Workers: <b>{total_jobs_fmt}</b></div>
      ", dynamic_header, "
      <div style='font-size: 1.2em; color: #6b7280; margin-bottom: 2px;'><b>Workforce Segment Details</b></div>

      <hr style='margin: 8px 0; border: 0; border-top: 1px solid #e5e7eb;'/>

      <div style='display: flex; gap: 16px;'>
        <div style='flex: 1;'>
          <b>By Age</b><br/>
          Under 30: {age_under_30_fmt} <i>({age_under_30_pct})</i><br/>
          30 to 54: {age_30_to_54_fmt} <i>({age_30_to_54_pct})</i><br/>
          55 and older: {age_over_55_fmt} <i>({age_over_55_pct})</i><br/>

          <hr style='margin: 6px 0; border: 0; border-top: 1px solid #e5e7eb;'/>
          <b>By Earnings</b><br/>
          Very Low ($1,250 or less/mo): {earn_low_fmt} <i>({earn_low_pct})</i><br/>
          Low ($1,250 to $3,333/mo): {earn_mid_fmt} <i>({earn_mid_pct})</i><br/>
          Average (Greater than $3,333/mo): {earn_high_fmt} <i>({earn_high_pct})</i><br/>

          <hr style='margin: 6px 0; border: 0; border-top: 1px solid #e5e7eb;'/>
          <b>By Education</b><br/>
          Less than High School: {edu_less_hs_fmt} <i>({edu_less_hs_pct})</i><br/>
          High School or Equivalent: {edu_hs_fmt} <i>({edu_hs_pct})</i><br/>
          Some College or Associate Degree: {edu_some_college_fmt} <i>({edu_some_college_pct})</i><br/>
          Bachelor's Degree or Higher: {edu_bachelors_fmt} <i>({edu_bachelors_pct})</i>
        </div>

        <div style='flex: 1;'>
          <b>By Broad Industry Sector</b><br/>
          Goods Producing: {ind_goods_fmt} <i>({ind_goods_pct})</i><br/>
          Trade & Transportation: {ind_trade_fmt} <i>({ind_trade_pct})</i><br/>
          Services: {ind_service_fmt} <i>({ind_service_pct})</i><br/>

          <hr style='margin: 6px 0; border: 0; border-top: 1px solid #e5e7eb;'/>
          <b>By Race</b><br/>
          White Alone: {race_white_fmt} <i>({race_white_pct})</i><br/>
          Black or African American: {race_black_fmt} <i>({race_black_pct})</i><br/>
          American Indian or Alaska Native: {race_native_fmt} <i>({race_native_pct})</i><br/>
          Asian Alone: {race_asian_fmt} <i>({race_asian_pct})</i><br/>
          Native Hawaiian or Pacific Islander: {race_pacific_fmt} <i>({race_pacific_pct})</i><br/>
          Two or More Races: {race_two_plus_fmt} <i>({race_two_plus_pct})</i><br/>

          <hr style='margin: 6px 0; border: 0; border-top: 1px solid #e5e7eb;'/>
          <b>By Ethnicity</b><br/>
          Hispanic or Latino: {eth_hispanic_fmt} <i>({eth_hispanic_pct})</i><br/>
          Not Hispanic or Latino: {eth_not_hispanic_fmt} <i>({eth_not_hispanic_pct})</i><br/>

          <hr style='margin: 6px 0; border: 0; border-top: 1px solid #e5e7eb;'/>
          <b>By Sex</b><br/>
          Male: {sex_male_fmt} <i>({sex_male_pct})</i><br/>
          Female: {sex_female_fmt} <i>({sex_female_pct})</i>
        </div>
      </div>
    </div>
  ")
}

get_legend_title_text <- function(selected_var, metric_type) {
  if (selected_var == "total_jobs") {
    return("Total Segment Workers")
  }
  base_label <- var_labels[[selected_var]]
  if (metric_type == "share") {
    return(paste0(base_label, " (Share)"))
  }
  return(base_label)
}

# ------------------------------------------------------------------------------
# User Interface
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "LODES Residence Area Characteristics (RAC) Explorer",
  sidebar = sidebar(
    width = "380px",
    padding = c("8px", "12px"),
    gap = "6px",

    tags$head(
      tags$style(HTML("
        .sidebar .form-group { margin-bottom: 4px !important; }
        .sidebar hr { margin-top: 8px !important; margin-bottom: 8px !important; }
        .sidebar .text-muted { margin-bottom: 4px !important; line-height: 1.2; }
        .sidebar .control-label { margin-bottom: 2px !important; font-weight: 600; }
        .sidebar .btn { margin-top: 4px; }

        /* MapLibre Popup Container Styling */
        .maplibregl-popup, .mapboxgl-popup {
          max-width: none !important;
          z-index: 1000 !important;
        }
        .maplibregl-popup-content, .mapboxgl-popup-content {
          padding: 12px 16px !important;
          border-radius: 8px !important;
          box-shadow: 0 4px 16px rgba(0,0,0,0.2) !important;
        }
      "))
    ),

    # 1. Data Processing Inputs
    p(class = "text-muted", style = "font-size: 0.82em;",
      "Changing geography or source data will download new data."),

    selectInput("state", "State:", choices = state_choices, selected = "NV"),
    selectInput("year", "Year:", choices = seq(2023, 2002), selected = 2023),
    selectInput("job_type", "Job Type:", choices = c(
      "All Jobs" = "JT00",
      "Primary Jobs" = "JT01",
      "All Private Jobs" = "JT02",
      "Private Primary Jobs" = "JT03",
      "All Federal Jobs" = "JT04",
      "Federal Primary Jobs" = "JT05"
    ), selected = "JT00"),
    selectInput("segment", "Workforce Segment (Sub-population):", choices = segment_choices, selected = "S000"),
    selectInput(
      "geo_level",
      "Geography Level:",
      choices = c(
        "County" = "ctyname",
        "County Subdivision" = "ctycsubname",
        "Census Place" = "stplcname",
        "Census Tract" = "trctname",
        "Block Group" = "bgrp",
        "Congressional District" = "stcd119name",
        "Senate District" = "stslduname",
        "Assembly District" = "stsldlname"
      ),
      selected = "ctyname"
    ),

    actionButton("update_map", "Fetch Data & Rebuild Map", class = "btn-primary w-100"),

    hr(),

    # 2. Real-Time Rendering Inputs
    p(class = "text-muted", style = "font-size: 0.82em;",
      "Data analysis: updates instantly using currently loaded data."),

    selectInput(
      "map_var",
      "Select Data to Map (Residence Location):",
      choices = list(
        "General" = c("Total Segment Workers" = "total_jobs"),
        "Demographics - Age" = c(
          "Age: Under 30" = "age_under_30",
          "Age: 30-54" = "age_30_to_54",
          "Age: 55 and older" = "age_over_55"
        ),
        "Demographics - Earnings" = c(
          "Earnings: Average (> $3,333/mo)" = "earn_high",
          "Earnings: Low ($1,250 to $3,333/mo)" = "earn_mid",
          "Earnings: Very Low (<= $1,250/mo)" = "earn_low"
        ),
        "Demographics - Race" = c(
          "White Alone" = "race_white",
          "Black or African American" = "race_black",
          "American Indian or Alaska Native" = "race_native",
          "Asian Alone" = "race_asian",
          "Native Hawaiian or Pacific Islander" = "race_pacific",
          "Two or More Races" = "race_two_plus"
        ),
        "Demographics - Ethnicity" = c(
          "Hispanic or Latino" = "eth_hispanic",
          "Not Hispanic or Latino" = "eth_not_hispanic"
        ),
        "Demographics - Education" = c(
          "Less than High School" = "edu_less_hs",
          "High School or Equivalent" = "edu_hs",
          "Some College or Associate Degree" = "edu_some_college",
          "Bachelor's Degree or Higher" = "edu_bachelors"
        ),
        "Demographics - Sex" = c(
          "Male" = "sex_male",
          "Female" = "sex_female"
        ),
        "Broad Industry Groups" = c(
          "Goods Producing"        = "ind_goods",
          "Trade and Transportation" = "ind_trade",
          "Services"               = "ind_service"
        ),
        "2-Digit NAICS Sectors" = c(
          "11: Agriculture, Forestry, Fishing, Hunting" = "cns01",
          "21: Mining, Oil and Gas Extraction"          = "cns02",
          "22: Utilities"                               = "cns03",
          "23: Construction"                            = "cns04",
          "31-33: Manufacturing"                        = "cns05",
          "42: Wholesale Trade"                         = "cns06",
          "44-45: Retail Trade"                         = "cns07",
          "48-49: Transportation and Warehousing"       = "cns08",
          "51: Information"                             = "cns09",
          "52: Finance and Insurance"                   = "cns10",
          "53: Real Estate and Rental/Leasing"          = "cns11",
          "54: Professional, Scientific, and Tech"      = "cns12",
          "55: Management of Companies"                 = "cns13",
          "56: Administrative and Waste Management"     = "cns14",
          "61: Educational Services"                    = "cns15",
          "62: Health Care and Social Assistance"       = "cns16",
          "71: Arts, Entertainment, and Recreation"     = "cns17",
          "72: Accommodation and Food Services"         = "cns18",
          "81: Other Services (except Public Admin)"    = "cns19",
          "92: Public Administration"                   = "cns20"
        )
      )
    ),
    radioButtons(
      "metric_type",
      "Display Metric:",
      choices = c(
        "Absolute Numbers" = "absolute",
        "Share of Total Segment Workers" = "share"
      ),
      selected = "absolute"
    ),

    sliderInput(
      "jobs_filter",
      "Filter by Mapped Data:",
      min = 0,
      max = 100000,
      value = c(0, 100000),
      sep = ","
    ),

    downloadButton("download_data", "Download Current Data (CSV)", class = "btn-outline-primary w-100"),

    hr(),

    # 3. Environment/Cache Settings
    p(class = "text-muted", style = "font-size: 0.82em;",
      "Storage: Persist downloaded data & vector tiles across app restarts."),
    checkboxInput("use_persistent_cache", "Enable Local File Cache", value = TRUE)
  ),
  card(
    full_screen = TRUE,
    card_body(
      padding = 0,
      style = "position: relative;",
      # Dynamic overlay box managed cleanly by Shiny's DOM engine
      div(
        style = "position: absolute; top: 12px; left: 12px; z-index: 500; pointer-events: none;",
        uiOutput("map_title_overlay")
      ),
      maplibreOutput("map")
    )
  )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive state storing spatial data, PMTiles URL, and metadata
  map_state <- reactiveVal(NULL)

  # Evaluate the proper dataset column mapping based on variable and absolute/share inputs
  active_map_column <- reactive({
    if (input$map_var == "total_jobs") return("total_jobs")
    if (input$metric_type == "share") return(paste0(input$map_var, "_share"))
    return(input$map_var)
  })

  # Reactive output generating a single, non-duplicating overlay title box
  output$map_title_overlay <- renderUI({
    req(map_state())
    active <- map_state()

    job_type_labels <- c(
      "JT00" = "All Jobs",
      "JT01" = "Primary Jobs",
      "JT02" = "All Private Jobs",
      "JT03" = "Private Primary Jobs",
      "JT04" = "All Federal Jobs",
      "JT05" = "Federal Primary Jobs"
    )
    job_type_str <- job_type_labels[[active$job_type]]
    var_label <- var_labels[[input$map_var]]

    metric_str <- if (input$map_var != "total_jobs" && input$metric_type == "share") " (Share of Segment)" else " (Absolute Count)"
    if (input$map_var == "total_jobs") metric_str <- ""

    HTML(paste0(
      "<div style='background: rgba(255, 255, 255, 0.95); padding: 12px 16px; border-radius: 8px; ",
      "box-shadow: 0 4px 12px rgba(0,0,0,0.18); font-family: system-ui, -apple-system, sans-serif; ",
      "backdrop-filter: blur(6px); border: 1px solid rgba(0,0,0,0.1); max-width: 440px; line-height: 1.45; pointer-events: auto;'>",
      "  <div style='font-size: 1.4em; font-weight: 700; color: #1e3a8a; margin-bottom: 4px;'>",
      active$state_name, " (", active$year, ")",
      "  </div>",
      "  <div style='font-size: 1.05em; color: #374151; margin-bottom: 4px;'>",
      "    <b>Jobs:</b> ", job_type_str, " | <b>Segment:</b> ", active$segment_label,
      "  </div>",
      "  <div style='font-size: 1.15em; font-weight: 600; color: #0284c7;'>",
      "    <b>Mapped Data:</b> ", var_label, metric_str,
      "  </div>",
      "</div>"
    ))
  })

  # Dynamically update the slider range whenever map state, selected variable, or display metric changes
  observeEvent(c(map_state(), input$map_var, input$metric_type), {
    req(map_state())
    active <- map_state()
    col_to_map <- active_map_column()
    vals <- active$sf[[col_to_map]]

    is_share <- (input$map_var != "total_jobs" && input$metric_type == "share")

    if (is_share) {
      min_val <- round(min(vals, na.rm = TRUE) * 100, 1)
      max_val <- round(max(vals, na.rm = TRUE) * 100, 1)
      if (is.infinite(min_val) || is.na(min_val)) min_val <- 0
      if (is.infinite(max_val) || is.na(max_val)) max_val <- 100
      if (min_val == max_val) max_val <- min_val + 0.1

      lbl <- paste0("Filter by ", var_labels[[input$map_var]], " (Share %):")

      updateSliderInput(
        session,
        inputId = "jobs_filter",
        label = lbl,
        min = min_val,
        max = max_val,
        value = c(min_val, max_val),
        step = 0.1
      )
    } else {
      min_val <- floor(min(vals, na.rm = TRUE))
      max_val <- ceiling(max(vals, na.rm = TRUE))
      if (is.infinite(min_val) || is.na(min_val)) min_val <- 0
      if (is.infinite(max_val) || is.na(max_val)) max_val <- 100
      if (min_val == max_val) max_val <- min_val + 1

      lbl <- if (input$map_var == "total_jobs") {
        "Filter by Total Segment Workers:"
      } else {
        paste0("Filter by ", var_labels[[input$map_var]], " (Count):")
      }

      updateSliderInput(
        session,
        inputId = "jobs_filter",
        label = lbl,
        min = min_val,
        max = max_val,
        value = c(min_val, max_val),
        step = 1
      )
    }
  })

  # Base Map/Data Pipeline
  observeEvent(input$update_map, ignoreNULL = FALSE, {

    # Isolate inputs to prevent reactive triggering during configuration
    state_val      <- isolate(input$state)
    year_val       <- isolate(input$year)
    job_type_val   <- isolate(input$job_type)
    segment_val    <- isolate(input$segment)
    geo_name_col   <- isolate(input$geo_level)
    use_persistent <- isolate(input$use_persistent_cache)

    geo_id_col     <- geo_id_lookup[[geo_name_col]]
    segment_label  <- names(segment_choices)[segment_choices == segment_val]
    state_full_name<- names(state_choices)[state_choices == state_val]

    withProgress(message = paste("Loading", state_val, "RAC Data..."), value = 0.1, {

      cache_dir <- get_data_cache_dir(use_persistent)
      state_lower <- tolower(state_val)

      # Census Dataset Endpoints
      rac_url <- sprintf("https://lehd.ces.census.gov/data/lodes/LODES8/%s/rac/%s_rac_%s_%s_%s.csv.gz",
                         state_lower, state_lower, segment_val, job_type_val, year_val)
      xwalk_url <- sprintf("https://lehd.ces.census.gov/data/lodes/LODES8/%s/%s_xwalk.csv.gz",
                           state_lower, state_lower)

      rac_dest <- file.path(cache_dir, basename(rac_url))
      xwalk_dest <- file.path(cache_dir, basename(xwalk_url))

      if (!file.exists(rac_dest)) {
        incProgress(0.1, detail = paste("Downloading RAC Data (Segment:", segment_val, ")"))
        download.file(rac_url, rac_dest, mode = "wb", quiet = TRUE)
      }
      if (!file.exists(xwalk_dest)) {
        incProgress(0.1, detail = "Downloading Crosswalk")
        download.file(xwalk_url, xwalk_dest, mode = "wb", quiet = TRUE)
      }

      incProgress(0.1, detail = "Reading CSV Files")
      rac_data <- read_csv(rac_dest, col_types = cols(.default = "d", h_geocode = "c"))
      xwalk <- read_csv(xwalk_dest, col_types = cols(.default = "c"))

      incProgress(0.2, detail = paste("Aggregating to", geo_name_col))

      # Pre-compute cross-tabulation sums
      rac_summary <- rac_data |>
        group_by(h_geocode) |>
        summarise(
          total_jobs       = sum(C000, na.rm = TRUE),
          age_under_30     = sum(CA01, na.rm = TRUE),
          age_30_to_54     = sum(CA02, na.rm = TRUE),
          age_over_55      = sum(CA03, na.rm = TRUE),
          earn_low         = sum(CE01, na.rm = TRUE),
          earn_mid         = sum(CE02, na.rm = TRUE),
          earn_high        = sum(CE03, na.rm = TRUE),
          ind_goods        = sum(CNS01 + CNS02 + CNS03 + CNS04 + CNS05, na.rm = TRUE),
          ind_trade        = sum(CNS06 + CNS07 + CNS08, na.rm = TRUE),
          ind_service      = sum(CNS09 + CNS10 + CNS11 + CNS12 + CNS13 + CNS14 +
                                   CNS15 + CNS16 + CNS17 + CNS18 + CNS19 + CNS20, na.rm = TRUE),
          cns01            = sum(CNS01, na.rm = TRUE),
          cns02            = sum(CNS02, na.rm = TRUE),
          cns03            = sum(CNS03, na.rm = TRUE),
          cns04            = sum(CNS04, na.rm = TRUE),
          cns05            = sum(CNS05, na.rm = TRUE),
          cns06            = sum(CNS06, na.rm = TRUE),
          cns07            = sum(CNS07, na.rm = TRUE),
          cns08            = sum(CNS08, na.rm = TRUE),
          cns09            = sum(CNS09, na.rm = TRUE),
          cns10            = sum(CNS10, na.rm = TRUE),
          cns11            = sum(CNS11, na.rm = TRUE),
          cns12            = sum(CNS12, na.rm = TRUE),
          cns13            = sum(CNS13, na.rm = TRUE),
          cns14            = sum(CNS14, na.rm = TRUE),
          cns15            = sum(CNS15, na.rm = TRUE),
          cns16            = sum(CNS16, na.rm = TRUE),
          cns17            = sum(CNS17, na.rm = TRUE),
          cns18            = sum(CNS18, na.rm = TRUE),
          cns19            = sum(CNS19, na.rm = TRUE),
          cns20            = sum(CNS20, na.rm = TRUE),
          race_white       = sum(CR01, na.rm = TRUE),
          race_black       = sum(CR02, na.rm = TRUE),
          race_native      = sum(CR03, na.rm = TRUE),
          race_asian       = sum(CR04, na.rm = TRUE),
          race_pacific     = sum(CR05, na.rm = TRUE),
          race_two_plus    = sum(CR07, na.rm = TRUE),
          eth_not_hispanic = sum(CT01, na.rm = TRUE),
          eth_hispanic     = sum(CT02, na.rm = TRUE),
          edu_less_hs      = sum(CD01, na.rm = TRUE),
          edu_hs           = sum(CD02, na.rm = TRUE),
          edu_some_college = sum(CD03, na.rm = TRUE),
          edu_bachelors    = sum(CD04, na.rm = TRUE),
          sex_male         = sum(CS01, na.rm = TRUE),
          sex_female       = sum(CS02, na.rm = TRUE),
          .groups = "drop"
        )

      # For Block Group, format display_name using Tract name if available
      if (geo_name_col == "bgrp") {
        agg_data <- rac_summary |>
          inner_join(xwalk, by = c("h_geocode" = "tabblk2020")) |>
          group_by(
            join_id = bgrp,
            display_name = if ("trctname" %in% names(xwalk)) paste0("Block Group ", substr(bgrp, 12, 12), ", ", trctname) else bgrp
          ) |>
          summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      } else {
        agg_data <- rac_summary |>
          inner_join(xwalk, by = c("h_geocode" = "tabblk2020")) |>
          group_by(
            join_id = !!sym(geo_id_col),
            display_name = !!sym(geo_name_col)
          ) |>
          summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      }

      agg_data <- agg_data |>
        mutate(
          across(c(age_under_30:sex_female),
                 ~ if_else(total_jobs > 0, as.numeric(. / total_jobs), 0),
                 .names = "{.col}_share"),
          across(c(age_under_30:sex_female),
                 ~ scales::percent(if_else(total_jobs > 0, . / total_jobs, 0), accuracy = 0.1),
                 .names = "{.col}_pct"),
          across(total_jobs:sex_female, ~scales::comma(.), .names = "{.col}_fmt")
        )

      # Fetch Vector Geometries
      incProgress(0.2, detail = "Loading Spatial Boundaries")
      geom <- switch(geo_name_col,
                     "ctyname"     = counties(state = state_val, cb = TRUE, year = 2023),
                     "ctycsubname" = county_subdivisions(state = state_val, cb = TRUE, year = 2023),
                     "stplcname"   = places(state = state_val, cb = TRUE, year = 2023),
                     "trctname"    = tracts(state = state_val, cb = TRUE, year = 2023),
                     "bgrp"        = block_groups(state = state_val, cb = TRUE, year = 2023),
                     "stcd119name" = congressional_districts(state = state_val, cb = TRUE, year = 2023),
                     "stslduname"  = state_legislative_districts(state = state_val, house = "upper", cb = TRUE, year = 2023),
                     "stsldlname"  = state_legislative_districts(state = state_val, house = "lower", cb = TRUE, year = 2023)
      )

      geo_sf <- geom |>
        inner_join(agg_data, by = c("GEOID" = "join_id")) |>
        st_transform(4326)

      # Manage Vector Map Caching
      pmtiles_filename <- paste0(state_lower, "_rac_", segment_val, "_", job_type_val, "_", year_val, "_", geo_name_col, ".pmtiles")
      pmtiles_path <- file.path(cache_dir, pmtiles_filename)

      if (!file.exists(pmtiles_path)) {
        incProgress(0.2, detail = "Generating Vector PMTiles (First Time)")
        freestile(
          input = geo_sf,
          output = pmtiles_path,
          layer_name = "jobs",
          min_zoom = 0,
          max_zoom = 14
        )
      } else {
        incProgress(0.2, detail = "Loading Cached PMTiles")
      }

      target_pmtiles_path <- file.path(persistent_cache_dir, pmtiles_filename)
      if (cache_dir != persistent_cache_dir && file.exists(pmtiles_path)) {
        file.copy(pmtiles_path, target_pmtiles_path, overwrite = TRUE)
      }

      # Expose final objects to the reactive environment
      base_url <- sub("/$", "", tile_server$url)
      map_state(list(
        sf = geo_sf,
        url = paste0(base_url, "/", pmtiles_filename),
        segment_label = segment_label,
        state_name = state_full_name,
        year = year_val,
        job_type = job_type_val,
        state_lower = state_lower
      ))
    })
  })

  # Rendering the Initial Map Instance
  output$map <- renderMaplibre({
    req(map_state())
    active <- map_state()

    col_to_map <- isolate(active_map_column())
    init_props <- get_dynamic_props(active$sf, col_to_map)
    popup_html <- build_popup_html(isolate(input$map_var), active$segment_label)
    legend_title <- get_legend_title_text(isolate(input$map_var), isolate(input$metric_type))

    is_share <- (isolate(input$map_var) != "total_jobs" && isolate(input$metric_type) == "share")
    legend_vals <- if (is_share) {
      scales::percent(init_props$breaks, accuracy = 0.1)
    } else {
      scales::comma(round(init_props$breaks))
    }

    maplibre(
      style = carto_style("positron"),
      bounds = st_bbox(active$sf)
    ) |>
      add_pmtiles_source(
        id = "jobs_src",
        url = active$url,
        promote_id = "GEOID"
      ) |>
      add_fill_layer(
        id = "jobs_fill",
        source = "jobs_src",
        source_layer = "jobs",
        fill_color = interpolate(
          column = col_to_map,
          values = init_props$breaks,
          stops = init_props$colors
        ),
        fill_opacity = 0.7,
        hover_options = list(fill_opacity = 1),
        popup = popup_html
      ) |>
      add_legend(
        legend_title = legend_title,
        values = legend_vals,
        filter_values = init_props$breaks,
        colors = init_props$colors,
        type = "continuous",
        width = "350px",
        position = "bottom-left"
      ) |>
      add_navigation_control(position = "top-right") |>
      add_fullscreen_control(position = "top-right") |>
      add_screenshot_control(
        position = "top-right",
        filename = paste0(active$state_lower, "_lodes_rac_map"),
        button_title = "Download PNG Screenshot of Map View",
        image_scale = 2
      ) |>
      add_scale_control(position = "bottom-left")
  })

  # Proxied Instant Recoloring & Dynamic Column Range Filtering
  observeEvent(c(input$map_var, input$metric_type, input$jobs_filter), {
    req(map_state(), input$jobs_filter)
    active <- map_state()

    col_to_map <- active_map_column()

    new_props <- get_dynamic_props(active$sf, col_to_map)
    new_popup <- build_popup_html(input$map_var, active$segment_label)
    new_legend_title <- get_legend_title_text(input$map_var, input$metric_type)

    is_share <- (input$map_var != "total_jobs" && input$metric_type == "share")
    new_legend_vals <- if (is_share) {
      scales::percent(new_props$breaks, accuracy = 0.1)
    } else {
      scales::comma(round(new_props$breaks))
    }

    # Scale filter thresholds depending on whether mapping shares (decimals 0-1) or absolute counts
    if (is_share) {
      min_val <- input$jobs_filter[1] / 100
      max_val <- input$jobs_filter[2] / 100
    } else {
      min_val <- input$jobs_filter[1]
      max_val <- input$jobs_filter[2]
    }

    maplibre_proxy("map") |>
      set_paint_property(
        layer_id = "jobs_fill",
        name = "fill-color",
        value = interpolate(
          column = col_to_map,
          values = new_props$breaks,
          stops = new_props$colors
        )
      ) |>
      set_popup(
        layer_id = "jobs_fill",
        popup = new_popup
      ) |>
      set_filter(
        layer_id = "jobs_fill",
        filter = list(
          "all",
          list(">=", list("get", col_to_map), min_val),
          list("<=", list("get", col_to_map), max_val)
        )
      ) |>
      add_legend(
        legend_title = new_legend_title,
        values = new_legend_vals,
        filter_values = new_props$breaks,
        colors = new_props$colors,
        type = "continuous",
        width = "350px",
        position = "bottom-left"
      )
  }, ignoreInit = TRUE)

  # CSV Data Downloader
  output$download_data <- downloadHandler(
    filename = function() {
      req(map_state())
      state_val   <- isolate(input$state)
      segment_val <- isolate(input$segment)
      geo_name    <- isolate(input$geo_level)
      paste0(tolower(state_val), "_lodes_rac_", segment_val, "_", geo_name, ".csv")
    },
    content = function(file) {
      req(map_state())
      active <- map_state()

      data_export <- active$sf |>
        st_drop_geometry() |>
        mutate(STATE_NAME = active$state_name) |>
        select(
          STATE_NAME,
          display_name,
          ends_with("_fmt"),
          ends_with("_pct")
        )

      write_csv(data_export, file)
    }
  )
}

shinyApp(ui, server)
