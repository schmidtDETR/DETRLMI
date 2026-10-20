# ==============================================================================
# LODES Regional RAC Explorer
#
# PACKAGE DEPLOYMENT NOTES:
# Ensure the following packages are added to the 'Imports' section of your
# package's DESCRIPTION file:
# Imports: shiny, dplyr, readr, tigris, sf, freestiler, mapgl, bslib, classInt,
#          scales, httpuv, servr, htmlwidgets
# Suggests: rmapshaper
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
library(htmlwidgets)

# ------------------------------------------------------------------------------
# Global Options & Initializations
# ------------------------------------------------------------------------------
options(tigris_use_cache = TRUE)
options(tigris_keep_resolution = FALSE)
options(timeout = 300)

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

tile_server <- tryCatch({
  serve_tiles(persistent_cache_dir, port = httpuv::randomPort())
}, error = function(e) {
  serve_tiles(persistent_cache_dir)
})

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
  "cns01"            = "NAICS 11: Agriculture, Forestry, Fishing, Hunting",
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
      <div style='font-size: 0.85em; color: #6b7280; font-style: italic; margin-bottom: 6px;'>Note: Totals reflect the complete selected workforce segment for this area, not the slider-filtered subset.</div>
      ", dynamic_header, "
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
          <b>By Industry Sector (Broad)</b><br/>
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
  if (selected_var == "total_jobs") return("Total Segment Workers")
  base_label <- var_labels[[selected_var]]
  if (metric_type == "share") return(paste0(base_label, " (Share)"))
  return(base_label)
}

get_map_title_html <- function(region_name, year, job_type, segment_label, map_var, metric_type) {
  job_type_labels <- c(
    "JT00" = "All Jobs", "JT01" = "Primary Jobs", "JT02" = "All Private Jobs",
    "JT03" = "Private Primary Jobs", "JT04" = "All Federal Jobs", "JT05" = "Federal Primary Jobs"
  )
  job_type_str <- job_type_labels[[job_type]]
  var_label <- var_labels[[map_var]]
  metric_str <- if (map_var != "total_jobs" && metric_type == "share") " (Share of Segment)" else " (Absolute Count)"
  if (map_var == "total_jobs") metric_str <- ""

  paste0(
    "<div style='background: rgba(255, 255, 255, 0.95); padding: 12px 16px; border-radius: 8px; ",
    "box-shadow: 0 4px 12px rgba(0,0,0,0.18); font-family: system-ui, -apple-system, sans-serif; ",
    "backdrop-filter: blur(6px); border: 1px solid rgba(0,0,0,0.1); max-width: 440px; line-height: 1.45; pointer-events: auto;'>",
    "  <div style='font-size: 1.4em; font-weight: 700; color: #1e3a8a; margin-bottom: 4px;'>",
    region_name, " (", year, ")",
    "  </div>",
    "  <div style='font-size: 1.05em; color: #374151; margin-bottom: 4px;'>",
    "    <b>Jobs:</b> ", job_type_str, " | <b>Segment:</b> ", segment_label,
    "  </div>",
    "  <div style='font-size: 1.15em; font-weight: 600; color: #0284c7;'>",
    "    <b>Mapped Data:</b> ", var_label, metric_str,
    "  </div>",
    "</div>"
  )
}

# ------------------------------------------------------------------------------
# User Interface
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "LODES Regional RAC Explorer",
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

        .maplibregl-popup, .mapboxgl-popup { max-width: none !important; z-index: 1000 !important; }
        .maplibregl-popup-content, .mapboxgl-popup-content {
          padding: 12px 16px !important; border-radius: 8px !important;
          box-shadow: 0 4px 16px rgba(0,0,0,0.2) !important;
        }

        .bslib-card .card-body {
          position: relative;
          padding: 0 !important;
          overflow: hidden !important;
          height: 100% !important;
        }
        #map {
          position: absolute;
          top: 0;
          left: 0;
          width: 100% !important;
          height: 100% !important;
        }
        .splash-overlay {
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: #ffffff;
          z-index: 600;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          text-align: center;
          padding: 40px;
        }
      "))
    ),

    # --- STAGE 1: Configuration View ---
    conditionalPanel(
      condition = "!output.map_ready",
      p(class = "text-muted", style = "font-size: 0.82em;",
        "Select regions and data boundaries to build the cache and geometry."),

      selectizeInput("states", "States (Multiple Allowed):", choices = state_choices, selected = "NV", multiple = TRUE),
      selectInput("year", "Year:", choices = seq(2023, 2002), selected = 2023),
      selectInput("job_type", "Job Type:", choices = c(
        "All Jobs" = "JT00", "Primary Jobs" = "JT01", "All Private Jobs" = "JT02",
        "Private Primary Jobs" = "JT03", "All Federal Jobs" = "JT04", "Federal Primary Jobs" = "JT05"
      ), selected = "JT00"),
      selectInput("segment", "Workforce Segment:", choices = segment_choices, selected = "S000"),
      selectInput("geo_level", "Geography Level:", choices = c(
        "County" = "ctyname", "County Subdivision" = "ctycsubname", "Census Place" = "stplcname",
        "Census Tract" = "trctname", "Block Group" = "bgrp", "Congressional District" = "stcd119name",
        "Senate District" = "stslduname", "Assembly District" = "stsldlname"
      ), selected = "ctyname"),

      actionButton("update_map", "Fetch Data & Build Map", class = "btn-primary w-100"),
      hr(),
      p(class = "text-muted", style = "font-size: 0.82em;", "Storage: Persist downloaded data locally."),
      checkboxInput("use_persistent_cache", "Enable Local File Cache", value = TRUE)
    ),

    # --- STAGE 2: Real-Time Map Styling View ---
    conditionalPanel(
      condition = "output.map_ready",
      actionButton("reset_map", "← Modify Data Selection", class = "btn-secondary w-100"),
      hr(),
      p(class = "text-muted", style = "font-size: 0.82em;", "Updates instantly using currently loaded data."),

      selectInput(
        "map_var",
        "Select Data to Map (Residence Location):",
        choices = list(
          "General" = c("Total Segment Workers" = "total_jobs"),
          "Demographics - Age" = c("Age: Under 30" = "age_under_30", "Age: 30-54" = "age_30_to_54", "Age: 55 and older" = "age_over_55"),
          "Demographics - Earnings" = c("Earnings: Average (> $3,333/mo)" = "earn_high", "Earnings: Low ($1,250 to $3,333/mo)" = "earn_mid", "Earnings: Very Low (<= $1,250/mo)" = "earn_low"),
          "Demographics - Race" = c("White Alone" = "race_white", "Black or African American" = "race_black", "American Indian or Alaska Native" = "race_native", "Asian Alone" = "race_asian", "Native Hawaiian or Pacific Islander" = "race_pacific", "Two or More Races" = "race_two_plus"),
          "Demographics - Ethnicity" = c("Hispanic or Latino" = "eth_hispanic", "Not Hispanic or Latino" = "eth_not_hispanic"),
          "Demographics - Education" = c("Less than High School" = "edu_less_hs", "High School or Equivalent" = "edu_hs", "Some College or Associate Degree" = "edu_some_college", "Bachelor's Degree or Higher" = "edu_bachelors"),
          "Demographics - Sex" = c("Male" = "sex_male", "Female" = "sex_female"),
          "Broad Industry Groups" = c("Goods Producing" = "ind_goods", "Trade and Transportation" = "ind_trade", "Services" = "ind_service"),
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
      radioButtons("metric_type", "Display Metric:", choices = c("Absolute Numbers" = "absolute", "Share of Total Segment Workers" = "share"), selected = "absolute"),
      sliderInput("jobs_filter", "Filter by Mapped Data:", min = 0, max = 100000, value = c(0, 100000), sep = ","),

      div(
        class = "d-grid gap-2",
        downloadButton("download_html", "Download Standalone Map (.html)", class = "btn-success w-100"),
        downloadButton("download_data", "Download Current Data (CSV)", class = "btn-outline-primary w-100")
      )
    )
  ),
  card(
    full_screen = TRUE,
    card_body(
      padding = 0,
      maplibreOutput("map", height = "100%", width = "100%"),
      div(
        style = "position: absolute; top: 12px; left: 12px; z-index: 500; pointer-events: none;",
        uiOutput("map_title_overlay")
      ),
      conditionalPanel(
        condition = "!output.map_ready",
        class = "splash-overlay",
        div(
          h2("LODES Regional Explorer", style = "color: #1e3a8a; font-weight: 700;"),
          p("Select one or more states and your desired dataset parameters in the sidebar, then click 'Fetch Data & Build Map' to begin.",
            style = "font-size: 1.15em; color: #4b5563; max-width: 480px; margin: 12px auto;")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  map_state <- reactiveVal(NULL)

  output$map_ready <- reactive({ !is.null(map_state()) })
  outputOptions(output, "map_ready", suspendWhenHidden = FALSE)

  observeEvent(input$reset_map, {
    map_state(NULL)
  })

  active_map_column <- reactive({
    if (input$map_var == "total_jobs") return("total_jobs")
    if (input$metric_type == "share") return(paste0(input$map_var, "_share"))
    return(input$map_var)
  })

  output$map_title_overlay <- renderUI({
    req(map_state())
    active <- map_state()
    HTML(get_map_title_html(
      active$region_name, active$year, active$job_type,
      active$segment_label, input$map_var, input$metric_type
    ))
  })

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

      updateSliderInput(session, inputId = "jobs_filter", label = lbl, min = min_val, max = max_val, value = c(min_val, max_val), step = 0.1)
    } else {
      min_val <- floor(min(vals, na.rm = TRUE))
      max_val <- ceiling(max(vals, na.rm = TRUE))
      if (is.infinite(min_val) || is.na(min_val)) min_val <- 0
      if (is.infinite(max_val) || is.na(max_val)) max_val <- 100
      if (min_val == max_val) max_val <- min_val + 1
      lbl <- if (input$map_var == "total_jobs") "Filter by Total Segment Workers:" else paste0("Filter by ", var_labels[[input$map_var]], " (Count):")

      updateSliderInput(session, inputId = "jobs_filter", label = lbl, min = min_val, max = max_val, value = c(min_val, max_val), step = 1)
    }
  })

  # --- MULTI-STATE PIPELINE ---
  observeEvent(input$update_map, {
    req(input$update_map > 0)
    req(length(input$states) > 0)

    selected_states <- isolate(input$states)
    year_val        <- isolate(input$year)
    job_type_val    <- isolate(input$job_type)
    segment_val     <- isolate(input$segment)
    geo_name_col    <- isolate(input$geo_level)
    use_persistent  <- isolate(input$use_persistent_cache)

    geo_id_col    <- geo_id_lookup[[geo_name_col]]
    segment_label <- names(segment_choices)[segment_choices == segment_val]

    if (length(selected_states) > 3) {
      region_name_str <- paste0(length(selected_states), " Selected States")
    } else {
      full_names <- names(state_choices)[match(selected_states, state_choices)]
      region_name_str <- paste(full_names, collapse = ", ")
    }

    withProgress(message = "Processing Regional Data...", value = 0, {

      cache_dir <- get_data_cache_dir(use_persistent)
      n_states <- length(selected_states)

      # 1. Iterate and Summarize State-by-State
      state_summaries <- lapply(seq_along(selected_states), function(i) {
        st <- selected_states[i]
        st_lower <- tolower(st)
        incProgress(0.5 / n_states, detail = paste("Fetching & Aggregating", st))

        rac_url <- sprintf("https://lehd.ces.census.gov/data/lodes/LODES8/%s/rac/%s_rac_%s_%s_%s.csv.gz", st_lower, st_lower, segment_val, job_type_val, year_val)
        xwalk_url <- sprintf("https://lehd.ces.census.gov/data/lodes/LODES8/%s/%s_xwalk.csv.gz", st_lower, st_lower)
        rac_dest <- file.path(cache_dir, basename(rac_url))
        xwalk_dest <- file.path(cache_dir, basename(xwalk_url))

        if (!file.exists(rac_dest)) download.file(rac_url, rac_dest, mode = "wb", quiet = TRUE)
        if (!file.exists(xwalk_dest)) download.file(xwalk_url, xwalk_dest, mode = "wb", quiet = TRUE)

        st_rac <- read_csv(rac_dest, col_types = cols(.default = "d", h_geocode = "c"))
        st_xwalk <- read_csv(xwalk_dest, col_types = cols(.default = "c"))

        st_summary <- st_rac |>
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

        if (geo_name_col == "bgrp") {
          st_summary |>
            inner_join(st_xwalk, by = c("h_geocode" = "tabblk2020")) |>
            group_by(join_id = bgrp, display_name = if ("trctname" %in% names(st_xwalk)) paste0("Block Group ", substr(bgrp, 12, 12), ", ", trctname) else bgrp) |>
            summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
            mutate(STATE_NAME = names(state_choices)[state_choices == st])
        } else {
          st_summary |>
            inner_join(st_xwalk, by = c("h_geocode" = "tabblk2020")) |>
            group_by(join_id = !!sym(geo_id_col), display_name = !!sym(geo_name_col)) |>
            summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
            mutate(STATE_NAME = names(state_choices)[state_choices == st])
        }
      })

      # 2. Combine all state summaries into one dataframe
      incProgress(0.1, detail = "Merging regional boundaries")
      agg_data <- bind_rows(state_summaries) |>
        group_by(join_id, display_name, STATE_NAME) |>
        summarise(across(total_jobs:sex_female, \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
        mutate(
          across(c(age_under_30:sex_female), ~ if_else(total_jobs > 0, as.numeric(. / total_jobs), 0), .names = "{.col}_share"),
          across(c(age_under_30:sex_female), ~ scales::percent(if_else(total_jobs > 0, . / total_jobs, 0), accuracy = 0.1), .names = "{.col}_pct"),
          across(total_jobs:sex_female, ~scales::comma(.), .names = "{.col}_fmt")
        )

      # 3. Fetch Tigris Geometries for all states
      geom_list <- lapply(selected_states, function(st) {
        switch(geo_name_col,
               "ctyname"     = counties(state = st, cb = TRUE, year = 2023),
               "ctycsubname" = county_subdivisions(state = st, cb = TRUE, year = 2023),
               "stplcname"   = places(state = st, cb = TRUE, year = 2023),
               "trctname"    = tracts(state = st, cb = TRUE, year = 2023),
               "bgrp"        = block_groups(state = st, cb = TRUE, year = 2023),
               "stcd119name" = congressional_districts(state = st, cb = TRUE, year = 2023),
               "stslduname"  = state_legislative_districts(state = st, house = "upper", cb = TRUE, year = 2023),
               "stsldlname"  = state_legislative_districts(state = st, house = "lower", cb = TRUE, year = 2023)
        )
      })
      geom <- do.call(rbind, geom_list)
      geo_sf <- geom |> inner_join(agg_data, by = c("GEOID" = "join_id")) |> st_transform(4326)

      # 4. Generate Regional PMTiles File
      incProgress(0.2, detail = "Generating Regional Vector Tiles")

      state_hash_str <- paste(sort(tolower(selected_states)), collapse = "")
      if (nchar(state_hash_str) > 30) state_hash_str <- substr(state_hash_str, 1, 30)

      pmtiles_filename <- paste0("multi_", state_hash_str, "_rac_", segment_val, "_", job_type_val, "_", year_val, "_", geo_name_col, ".pmtiles")
      pmtiles_path <- file.path(cache_dir, pmtiles_filename)

      if (!file.exists(pmtiles_path)) {
        freestile(input = geo_sf, output = pmtiles_path, layer_name = "jobs", min_zoom = 0, max_zoom = 14)
      }

      target_pmtiles_path <- file.path(persistent_cache_dir, pmtiles_filename)
      if (cache_dir != persistent_cache_dir && file.exists(pmtiles_path)) {
        file.copy(pmtiles_path, target_pmtiles_path, overwrite = TRUE)
      }

      base_url <- sub("/$", "", tile_server$url)
      map_state(list(
        sf = geo_sf,
        url = paste0(base_url, "/", pmtiles_filename),
        segment_label = segment_label,
        region_name = region_name_str,
        year = year_val,
        job_type = job_type_val,
        hash = state_hash_str
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
    legend_vals <- if (is_share) scales::percent(init_props$breaks, accuracy = 0.1) else scales::comma(round(init_props$breaks))

    maplibre(style = carto_style("positron"), bounds = st_bbox(active$sf)) |>
      add_pmtiles_source(id = "jobs_src", url = active$url, promote_id = "GEOID") |>
      add_fill_layer(
        id = "jobs_fill", source = "jobs_src", source_layer = "jobs",
        fill_color = interpolate(column = col_to_map, values = init_props$breaks, stops = init_props$colors),
        fill_opacity = 0.7, hover_options = list(fill_opacity = 1), popup = popup_html
      ) |>
      add_legend(legend_title = legend_title, values = legend_vals, filter_values = init_props$breaks, colors = init_props$colors, type = "continuous", width = "350px", position = "bottom-left") |>
      add_navigation_control(position = "top-right") |>
      add_fullscreen_control(position = "top-right") |>
      add_screenshot_control(position = "top-right", filename = paste0("regional_lodes_rac_map"), button_title = "Download PNG Screenshot", image_scale = 2) |>
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
    new_legend_vals <- if (is_share) scales::percent(new_props$breaks, accuracy = 0.1) else scales::comma(round(new_props$breaks))

    if (is_share) {
      min_val <- input$jobs_filter[1] / 100
      max_val <- input$jobs_filter[2] / 100
    } else {
      min_val <- input$jobs_filter[1]
      max_val <- input$jobs_filter[2]
    }

    maplibre_proxy("map") |>
      set_paint_property(layer_id = "jobs_fill", name = "fill-color", value = interpolate(column = col_to_map, values = new_props$breaks, stops = new_props$colors)) |>
      set_popup(layer_id = "jobs_fill", popup = new_popup) |>
      set_filter(layer_id = "jobs_fill", filter = list("all", list(">=", list("get", col_to_map), min_val), list("<=", list("get", col_to_map), max_val))) |>
      add_legend(legend_title = new_legend_title, values = new_legend_vals, filter_values = new_props$breaks, colors = new_props$colors, type = "continuous", width = "350px", position = "bottom-left")
  }, ignoreInit = TRUE)

  # --- STANDALONE SELF-CONTAINED HTML EXPORTER ---
  output$download_html <- downloadHandler(
    filename = function() {
      req(map_state())
      paste0("standalone_lodes_rac_", tolower(isolate(input$segment)), "_", isolate(input$geo_level), "_", isolate(input$map_var), ".html")
    },
    content = function(file) {
      req(map_state())
      active <- map_state()
      col_to_map <- active_map_column()
      is_share <- (input$map_var != "total_jobs" && input$metric_type == "share")

      withProgress(message = "Generating Standalone HTML Snapshot...", value = 0.2, {

        # 1. Filter export features by the current slider range
        if (is_share) {
          min_val <- input$jobs_filter[1] / 100
          max_val <- input$jobs_filter[2] / 100
        } else {
          min_val <- input$jobs_filter[1]
          max_val <- input$jobs_filter[2]
        }

        export_sf <- active$sf |>
          filter(!is.na(.data[[col_to_map]]),
                 .data[[col_to_map]] >= min_val,
                 .data[[col_to_map]] <= max_val)

        incProgress(0.3, detail = "Pruning attributes & simplifying geometry")

        # 2. Prune unused columns to reduce GeoJSON payload size
        popup_fields <- c(
          "display_name", "total_jobs_fmt",
          "age_under_30_fmt", "age_under_30_pct", "age_30_to_54_fmt", "age_30_to_54_pct", "age_over_55_fmt", "age_over_55_pct",
          "earn_low_fmt", "earn_low_pct", "earn_mid_fmt", "earn_mid_pct", "earn_high_fmt", "earn_high_pct",
          "edu_less_hs_fmt", "edu_less_hs_pct", "edu_hs_fmt", "edu_hs_pct", "edu_some_college_fmt", "edu_some_college_pct", "edu_bachelors_fmt", "edu_bachelors_pct",
          "ind_goods_fmt", "ind_goods_pct", "ind_trade_fmt", "ind_trade_pct", "ind_service_fmt", "ind_service_pct",
          "race_white_fmt", "race_white_pct", "race_black_fmt", "race_black_pct", "race_native_fmt", "race_native_pct", "race_asian_fmt", "race_asian_pct", "race_pacific_fmt", "race_pacific_pct", "race_two_plus_fmt", "race_two_plus_pct",
          "eth_hispanic_fmt", "eth_hispanic_pct", "eth_not_hispanic_fmt", "eth_not_hispanic_pct",
          "sex_male_fmt", "sex_male_pct", "sex_female_fmt", "sex_female_pct",
          paste0(input$map_var, "_fmt"), paste0(input$map_var, "_pct")
        )
        keep_cols <- unique(c(col_to_map, popup_fields))
        export_sf <- export_sf |> select(any_of(keep_cols))

        # 3. Simplify geometry to keep file size lightweight
        if (requireNamespace("rmapshaper", quietly = TRUE)) {
          export_sf <- rmapshaper::ms_simplify(export_sf, keep = 0.15, keep_shapes = TRUE)
        } else {
          export_sf <- sf::st_simplify(export_sf, preserveTopology = TRUE, dTolerance = 0.001)
        }
        export_sf <- st_transform(export_sf, 4326)

        incProgress(0.3, detail = "Rendering interactive HTML map")

        # 4. Construct the standalone widget (embedding GeoJSON directly)
        new_props <- get_dynamic_props(export_sf, col_to_map)
        new_popup <- build_popup_html(input$map_var, active$segment_label)
        new_legend_title <- get_legend_title_text(input$map_var, input$metric_type)
        new_legend_vals <- if (is_share) scales::percent(new_props$breaks, accuracy = 0.1) else scales::comma(round(new_props$breaks))

        title_html <- paste0(
          "<style>",
          ".maplibregl-popup, .mapboxgl-popup { max-width: none !important; z-index: 1000 !important; }",
          ".maplibregl-popup-content, .mapboxgl-popup-content { padding: 12px 16px !important; border-radius: 8px !important; box-shadow: 0 4px 16px rgba(0,0,0,0.2) !important; }",
          "</style>",
          get_map_title_html(active$region_name, active$year, active$job_type, active$segment_label, input$map_var, input$metric_type)
        )

        standalone_map <- maplibre(
          style = carto_style("positron"),
          bounds = st_bbox(export_sf)
        ) |>
          add_fill_layer(
            id = "jobs_fill",
            source = export_sf,
            fill_color = interpolate(
              column = col_to_map,
              values = new_props$breaks,
              stops = new_props$colors
            ),
            fill_opacity = 0.7,
            hover_options = list(fill_opacity = 1),
            popup = new_popup
          ) |>
          add_legend(
            legend_title = new_legend_title,
            values = new_legend_vals,
            filter_values = new_props$breaks,
            colors = new_props$colors,
            type = "continuous",
            width = "350px",
            position = "bottom-left"
          ) |>
          add_navigation_control(position = "top-right") |>
          add_fullscreen_control(position = "top-right") |>
          add_scale_control(position = "bottom-left") |>
          add_control(html = title_html, position = "top-left")

        incProgress(0.2, detail = "Saving self-contained file")

        # 5. Save out via temporary file to prevent connection errors
        temp_html <- file.path(tempdir(), paste0("export_", as.numeric(Sys.time()), ".html"))
        saveWidget(standalone_map, file = temp_html, selfcontained = TRUE)
        file.copy(temp_html, file, overwrite = TRUE)
        unlink(temp_html)
      })
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      req(map_state())
      paste0("regional_lodes_rac_", isolate(input$segment), "_", isolate(input$geo_level), ".csv")
    },
    content = function(file) {
      req(map_state())
      data_export <- map_state()$sf |>
        st_drop_geometry() |>
        select(STATE_NAME, display_name, ends_with("_fmt"), ends_with("_pct"))
      write_csv(data_export, file)
    }
  )
}

shinyApp(ui, server)
