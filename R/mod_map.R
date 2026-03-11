# =============================================================================
# mod_map.R
# Shiny module: interactive Leaflet site map.
# Highlights the selected site-visit in green; all others in grey.
# Clicking a marker selects the corresponding row in the summary table.
# =============================================================================

#' Map module UI
#' @param id Module namespace ID.
#' @export
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "margin-bottom:8px;",
      shiny::actionButton(ns("refresh_map"),
                          label = shiny::tagList(
                            shiny::icon("compress-arrows-alt"), " Reset View"
                          ),
                          class = "btn-sm btn-default")
    ),
    leaflet::leafletOutput(ns("site_map"), height = 580)
  )
}

#' Map module server
#'
#' @param id            Module namespace ID.
#' @param filtered_data A `reactive` returning the filtered data frame.
#' @param selected_group A `reactive` returning the selected `parent_activity_id`,
#'   or NULL.  Provided by [mod_summary_server()].
#' @param summary_proxy A `DT::dataTableProxy` for the summary table, used to
#'   sync row selection when a map marker is clicked.
#'
#' @export
mod_map_server <- function(id, filtered_data, selected_group,
                            summary_proxy) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Helpers
    # -------------------------------------------------------------------------
    .make_popup <- function(site_id, loc, last_dt, n_vis, n_rec,
                             n_par, miss_pct, n_trans) {
      paste0(
        "<div style='font-size:14px;min-width:230px;'>",
        "<strong style='font-size:15px;color:#1976d2;'>", loc, "</strong><br>",
        "<span style='color:#888;font-size:12px;'>", site_id, "</span>",
        "<hr style='margin:6px 0;'>",
        "<strong>Last Visit:</strong> ", last_dt, "<br>",
        "<strong>Total Visits:</strong> ", n_vis, "<br>",
        "<strong>Records:</strong> ", format(n_rec, big.mark = ","), "<br>",
        "<strong>Parameters:</strong> ", n_par, "<br>",
        "<strong>Missing Values:</strong> ", miss_pct, "%<br>",
        "<strong>Transects:</strong> ", n_trans, " / 11",
        "</div>"
      )
    }

    .build_coords <- function(d) {
      d |>
        dplyr::filter(!is.na(.data[[COL_LAT]]), !is.na(.data[[COL_LON]])) |>
        dplyr::group_by(.data[[COL_SITE]], .data[[COL_LAT]], .data[[COL_LON]]) |>
        dplyr::summarize(
          loc_name    = if (COL_LOC_NAME %in% names(d))
            dplyr::first(.data[[COL_LOC_NAME]]) else dplyr::first(.data[[COL_SITE]]),
          last_visit  = {
            dv <- .data[[COL_DATE]][!is.na(.data[[COL_DATE]])]
            if (length(dv) == 0) "Unknown" else format(max(dv), "%Y-%m-%d")
          },
          n_visits    = dplyr::n_distinct(.data[[COL_GROUP]], na.rm = TRUE),
          n_records   = dplyr::n(),
          parameters  = dplyr::n_distinct(.data[[COL_PARAM]], na.rm = TRUE),
          missing_pct = round(sum(is.na(.data[[COL_VALUE]])) / dplyr::n() * 100, 1),
          n_transects = dplyr::n_distinct(
            .data[[COL_TRANSECT]][!is.na(.data[[COL_TRANSECT]])], na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        dplyr::rename(site = 1, lat = 2, lon = 3) |>
        dplyr::mutate(popup = as.character(mapply(
          .make_popup, site, loc_name, last_visit,
          n_visits, n_records, parameters, missing_pct, n_transects,
          SIMPLIFY = TRUE
        )))
    }

    # -------------------------------------------------------------------------
    # Render map — re-renders on data or selection change
    # -------------------------------------------------------------------------
    output$site_map <- leaflet::renderLeaflet({
      d    <- tryCatch(filtered_data(), error = function(e) NULL)
      base <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      if (is.null(d) || nrow(d) == 0 ||
          !all(c(COL_LAT, COL_LON, COL_SITE) %in% names(d)))
        return(base |> leaflet::setView(lng = -97, lat = 36, zoom = 6))

      coords <- tryCatch(.build_coords(d), error = function(e) NULL)
      if (is.null(coords) || nrow(coords) == 0)
        return(base |> leaflet::setView(lng = -97, lat = 36, zoom = 6))

      # Determine selected site from selected_group reactive
      sel_site <- NULL
      sel_grp  <- tryCatch(selected_group(), error = function(e) NULL)
      if (!is.null(sel_grp) && length(sel_grp) > 0) {
        site_row <- d |>
          dplyr::filter(.data[[COL_GROUP]] == sel_grp,
                        !is.na(.data[[COL_LAT]]), !is.na(.data[[COL_LON]])) |>
          dplyr::slice(1)
        if (nrow(site_row) > 0) sel_site <- site_row[[COL_SITE]]
      }

      grey <- if (!is.null(sel_site)) coords[coords$site != sel_site, ] else coords
      m    <- base

      if (nrow(grey) > 0)
        m <- m |> leaflet::addCircleMarkers(
          data        = grey, lng = ~lon, lat = ~lat, layerId = ~site,
          radius = 6, color = "#555555", fillColor = "#888888",
          fillOpacity = 0.5, stroke = TRUE, weight = 1,
          label        = ~paste0(loc_name, " \u2014 ", site),
          labelOptions = leaflet::labelOptions(direction = "auto", sticky = FALSE),
          popup        = ~popup
        )

      if (!is.null(sel_site)) {
        sc <- coords[coords$site == sel_site, ]
        if (nrow(sc) > 0)
          m <- m |> leaflet::addCircleMarkers(
            data        = sc, lng = ~lon, lat = ~lat,
            layerId     = ~paste0("sel_", site),
            radius = 14, color = "#006400", fillColor = "#00cc00",
            fillOpacity = 0.85, stroke = TRUE, weight = 3,
            label        = ~paste0(loc_name, " (selected)"),
            labelOptions = leaflet::labelOptions(direction = "auto", sticky = FALSE),
            popup        = ~popup
          )
        m <- m |> leaflet::setView(
          lng  = coords$lon[coords$site == sel_site][1],
          lat  = coords$lat[coords$site == sel_site][1],
          zoom = 10
        )
      } else {
        m <- m |> leaflet::fitBounds(
          min(coords$lon) - 0.1, min(coords$lat) - 0.1,
          max(coords$lon) + 0.1, max(coords$lat) + 0.1
        )
      }
      m
    })

    # -------------------------------------------------------------------------
    # Reset View button
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$refresh_map, {
      d <- tryCatch(filtered_data(), error = function(e) NULL)
      if (is.null(d) || nrow(d) == 0) return()
      coords <- tryCatch(.build_coords(d), error = function(e) NULL)
      if (is.null(coords) || nrow(coords) == 0) return()
      leaflet::leafletProxy(ns("site_map")) |>
        leaflet::fitBounds(
          min(coords$lon) - 0.1, min(coords$lat) - 0.1,
          max(coords$lon) + 0.1, max(coords$lat) + 0.1
        )
      shiny::showNotification("View reset", type = "message", duration = 2)
    })

    # -------------------------------------------------------------------------
    # Marker click → sync summary table row selection
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$site_map_marker_click, {
      shiny::req(filtered_data())
      clicked_site <- sub("^sel_", "", input$site_map_marker_click$id)
      d         <- filtered_data()
      group_ids <- d |>
        dplyr::group_by(.data[[COL_GROUP]]) |>
        dplyr::summarize(site = dplyr::first(.data[[COL_SITE]]),
                         .groups = "drop")
      match_row <- which(group_ids$site == clicked_site)
      if (length(match_row) > 0)
        DT::dataTableProxy(summary_proxy) |>
          DT::selectRows(match_row[1])
    })
  })
}
