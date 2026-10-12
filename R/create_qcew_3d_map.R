#' Create 3D QCEW Employment Map
#'
#' @param industry_code Character. The NAICS industry code to pull from BLS.
#' @param area_type Character. The geographic level to map. Options: "msa", "county", "state".
#' @param qtr_select Character. The quarter to filter by (e.g., "1", "2", "3", "4"). Defaults to "2".
#' @param target_max_height Numeric. The visual height scale for the largest polygon, where 1 equals 100,000 Mapbox meters. Defaults to 8.
#' @param cap_quantile Numeric. A value between 0 and 1 representing the percentile at which to cap extreme values. Defaults to 0.99. Set to 1 for no cap.
#' @param save_path Character. Optional file path to save the HTML widget (e.g., "output.html").
#' @param ... Additional arguments passed on to `BLSloadR::get_qcew()`.
#' @return A maplibre interactive 3D map object.
#' @importFrom rlang .data
#' @export
create_qcew_3d_map <- function(industry_code, area_type = c("msa", "county", "state"), qtr_select = "2", target_max_height = 8, cap_quantile = 0.99, save_path = NULL, ...) {

  # 1. Validate inputs and set BLS/Shapefile variables
  user_area_type <- tolower(match.arg(area_type))

  message("Downloading QCEW data...")
  industry_download <- BLSloadR::get_qcew(industry_code = industry_code, ...)

  message(sprintf("Downloading shapefiles for %s...", user_area_type))

  if (user_area_type == "msa") {
    bls_area_type <- "Metropolitan Statistical Area"
    shapes <- tigris::core_based_statistical_areas(cb = TRUE) |>
      dplyr::mutate(GEOID = paste0("C", substr(.data$CBSAFP, 1, 4)))

  } else if (user_area_type == "county") {
    bls_area_type <- "County"
    shapes <- tigris::counties(cb = TRUE)

  } else if (user_area_type == "state") {
    bls_area_type <- "State"
    shapes <- tigris::states(cb = TRUE) |>
      dplyr::mutate(GEOID = paste0(.data$GEOID, "000"))
  }

  # 2. Process data and merge
  message("Processing data and merging geometries...")
  industry_data <- industry_download |>
    dplyr::filter(
      .data$area_type == bls_area_type,
      .data$disclosure_code != "N",
      .data$qtr == qtr_select,
      .data$own_code == "5"
    )

  if (nrow(industry_data) == 0) {
    stop("Error: No data available for the specified criteria after filtering.")
  }

  industry_data <- industry_data |>
    dplyr::mutate(
      avgemp = round((.data$month1_emplvl + .data$month2_emplvl + .data$month3_emplvl) / 3, 0),
      oty_avgemp_pct = round((.data$oty_month1_emplvl_pct_chg + .data$oty_month2_emplvl_pct_chg + .data$oty_month3_emplvl_pct_chg) / 3, 0),

      capped_emp = pmin(.data$avgemp, stats::quantile(.data$avgemp, cap_quantile, na.rm = TRUE)),

      scale_factor = (target_max_height * 100000) / max(.data$capped_emp, na.rm = TRUE),
      height_meters = .data$capped_emp * .data$scale_factor,

      label_text = paste0(
        "<b>Area:</b> ", .data$area_title, "<br>",
        "<b>Avg Employment:</b> ", format(.data$avgemp, big.mark = ","), "<br>",
        "<b>YoY Change:</b> ", .data$oty_avgemp_pct, "%"
      )
    ) |>
    dplyr::left_join(shapes, by = c("area_fips" = "GEOID")) |>
    sf::st_as_sf()

  # Select only what is needed for the map
  industry_select <- industry_data |>
    dplyr::select(
      .data$area_title, .data$industry_title, .data$industry_code,
      .data$year, .data$qtr, .data$avgemp, .data$oty_avgemp_pct,
      .data$height_meters, .data$label_text
    )

  # 3. Calculate DYNAMIC Color Scale Bounds
  color_p10 <- unname(stats::quantile(industry_select$oty_avgemp_pct, 0.10, na.rm = TRUE))
  color_p90 <- unname(stats::quantile(industry_select$oty_avgemp_pct, 0.90, na.rm = TRUE))

  # If data crosses zero, anchor at 0. Otherwise, center on the median.
  if (color_p10 < 0 && color_p90 > 0) {
    color_mid <- 0
  } else {
    color_mid <- unname(stats::median(industry_select$oty_avgemp_pct, na.rm = TRUE))
  }

  # Safety catch: mapgl requires strictly increasing stop values
  if (color_mid <= color_p10) color_mid <- color_p10 + 0.1
  if (color_p90 <= color_mid) color_p90 <- color_mid + 0.1

  # 4. Create the Title Block
  ind_title <- unique(industry_select$industry_title)[1]
  title_year <- unique(industry_select$year)[1]

  title_html <- paste0(
    "<div style='background: rgba(30, 30, 30, 0.85); color: white; padding: 15px; border-radius: 8px; font-family: sans-serif; border: 1px solid #555;'>",
    "<h3 style='margin: 0; font-size: 18px;'>", ind_title, "</h3>",
    "<p style='margin: 5px 0 0 0; font-size: 14px; color: #ccc;'>Q", qtr_select, " ", title_year, " Employment & YoY Change</p>",
    "</div>"
  )

  # 5. Render the Map
  message("Rendering 3D Map...")
  out_map <- mapgl::maplibre(style = mapgl::carto_style("dark-matter"), pitch = 45) |>
    mapgl::fit_bounds(industry_select) |>
    mapgl::add_control(html = title_html, position = "top-left") |>
    mapgl::add_fill_extrusion_layer(
      id = "qcew_3d_emp",
      source = industry_select,
      fill_extrusion_height = mapgl::get_column("height_meters"),
      fill_extrusion_color = mapgl::interpolate(
        column = "oty_avgemp_pct",
        values = c(color_p10, color_mid, color_p90),
        stops = c("#fc8d59", "#e0e0e0", "#91bfdb")
      ),
      fill_extrusion_opacity = 0.85,
      tooltip = "label_text",
      hover_options = list(
        fill_extrusion_color = "yellow",
        fill_extrusion_opacity = 1
      )
    ) |>
    mapgl::add_legend(
      legend_title = "YoY Emp % Change",
      values = c(
        paste0(round(color_p10, 1), "% or worse"),
        paste0(round(color_mid, 1), "%"),
        paste0(round(color_p90, 1), "% or better")
      ),
      colors = c("#fc8d59", "#e0e0e0", "#91bfdb"),
      position = "bottom-left"
    )

  # 6. Optional HTML Export
  if (!is.null(save_path)) {
    message(sprintf("Saving map to %s...", save_path))
    htmlwidgets::saveWidget(
      widget = out_map,
      file = save_path,
      selfcontained = TRUE
    )
  }

  return(out_map)
}
