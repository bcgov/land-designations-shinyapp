# Copyright 2016 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source("server_functions.R")

shinyServer(function(input, output, session) {
  # Reactive values list to keep track of clicked polygons
  click_ids <- reactiveValues(ecoreg_ids = character(0),
                              bec_ids = character(0))

  # Keep track of current clicked polygon and previous. Store in reactive values list
  observeEvent(input$bc_ecoreg_map_shape_click$id, {
    prev_click_id <- click_ids$ecoreg_ids[length(click_ids$ecoreg_ids)]
    click_ids$ecoreg_ids <- c(prev_click_id, input$bc_ecoreg_map_shape_click$id)
  })

  observeEvent(input$reset_bc_ecoreg, {
    prev_click_id <- click_ids$ecoreg_ids[length(click_ids$ecoreg_ids)]
    click_ids$ecoreg_ids <- c(prev_click_id, "BC")
  })

  ## Ecoregion leaflet map - draw all polygons once at startup
  output$bc_ecoreg_map <- renderLeaflet({
    leaflet(ecoregions) %>%
      bc_view() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(layerId = ecoregions$CRGNCD, color = "#00441b", fillColor = "#006d2c",
                  weight = 1, fillOpacity = 0.2)
  })

  # Observer for highlighting ecoregion polygon on click
  observe({
    clicked_polys <- click_ids$ecoreg_ids

    output$reset_bc_ecoreg <- renderText(clicked_polys)

    # subset the ecoregions to be updated to those that are either currently clicked
    # or previously clicked. Using match subsets AND sorts the SPDF according to the order of
    # clicked_polys, as the first one is the previously clicked one, and the second
    # is the currently clicked. The polygons must be in the same order as the aesthetics
    # (col, fill, weight, etc)
    ecoreg_subset <- ecoregions[na.omit(match(clicked_polys, ecoregions$CRGNCD)), ]

    ecoreg_proxy(data = ecoreg_subset) %>%
      highlight_clicked_poly(clicked_polys, class = "ecoreg")
  })

  # Observers for clearing old and adding new popups to ecoregion leaflet map
  observeEvent(input$bc_ecoreg_map_shape_mouseover$id, {
    reg_id <- input$bc_ecoreg_map_shape_mouseover$id
    reg_name <- ecoreg_nms[ecoreg_ids == reg_id]
    ecoreg_proxy() %>%
      addControl(reg_name, position = "topright", layerId = "ecoreg_label")
  })

  observeEvent(input$bc_ecoreg_map_shape_mouseout$id, {
    ecoreg_proxy() %>%
      removeControl(layerId = "ecoreg_label")
  })

  ## Subset map of ecoregion with land designations
  output$ecoreg_map <- renderPlot({
    ecoreg_code <- click_ids$ecoreg_ids[length(click_ids$ecoreg_ids)]
    if (length(ecoreg_code) == 0) ecoreg_code <- "BC"

    gg_ld_class(class = "ecoreg", ecoreg_code)
  })

  ## Bar chart of land designations for selected ecoregion
  output$ecoreg_barchart <- renderggiraph({
    ecoreg_code <- click_ids$ecoreg_ids[length(click_ids$ecoreg_ids)]
    if (length(ecoreg_code) == 0 || ecoreg_code == "BC") {
      ecoreg_code <- "BC"
      df <- bc_ld_summary
      type <- "British Columbia"
    } else {
      df <- ld_ecoreg_summary[ld_ecoreg_summary$CRGNCD == ecoreg_code, ]
      type <- "ecoregion"
    }

    ggiraph_barchart(df, type)
  })

  output$ecoreg_table <- DT::renderDataTable({
    ecoreg_code <- click_ids$ecoreg_ids[length(click_ids$ecoreg_ids)]
    if (length(ecoreg_code) == 0 || ecoreg_code == "BC") {
      df <- ld_ecoreg_summary
    } else {
      df <- ld_ecoreg_summary[ld_ecoreg_summary$CRGNCD == ecoreg_code, ]
    }
    summarize_ecoreg(df) %>%
      make_dt()
  })

  #### BEC #####################################################################

  # Keep track of current clicked polygon and previous. Store in reactive values list
  observeEvent(input$bc_bec_map_shape_click$id, {
    prev_click_id <- click_ids$bec_ids[length(click_ids$bec_ids)]
    click_ids$bec_ids <- c(prev_click_id, input$bc_bec_map_shape_click$id)
  })


  observeEvent(input$reset_bc_bec, {
    prev_click_id <- click_ids$bec_ids[length(click_ids$bec_ids)]
    click_ids$bec_ids <- c(prev_click_id, "BC")
  })
  # Render initial BEC map
  output$bc_bec_map <- renderLeaflet({
    leaflet(bec_zones) %>%
      bc_view() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(layerId = bec_zones$ZONE, color = "",
                  fillColor = unname(bec_colors), fillOpacity = 0.7)
  })

  # Observer for highlighting bec polygon on click
  observe({
    clicked_polys <- click_ids$bec_ids

    output$reset_bc_bec <- renderText(clicked_polys)
    # subset the bec polygons to be updated to those that are either currently clicked
    # or previously clicked. Using match subsets AND sorts the SPDF according to the order of
    # clicked_polys, as the first one is the previously clicked one, and the second
    # is the currently clicked. The polygons must be in the same order as the aesthetics
    # (col, fill, weight, etc)
    bec_subset <- bec_zones[na.omit(match(clicked_polys, bec_zones$ZONE)), ]

    bec_proxy(data = bec_subset) %>%
      highlight_clicked_poly(clicked_polys, class = "bec")
  })

  ## Observers for clearing old and adding new popups to BEC leaflet map
  observeEvent(input$bc_bec_map_shape_mouseover$id, {
    reg_id <- input$bc_bec_map_shape_mouseover$id
    reg_name <- bec_nms[reg_id]
    bec_proxy() %>%
      addControl(htmlize(reg_name), position = "topright", layerId = "bec_label")
  })

  observeEvent(input$bc_bec_map_shape_mouseout$id, {
    bec_proxy() %>%
      removeControl(layerId = "bec_label")
  })

  ## BEC map and barchart

  ## Subset map of bec zone with land designations
  output$bec_map <- renderPlot({
    bec_code <- click_ids$bec_ids[length(click_ids$bec_ids)]
    if (length(bec_code) == 0) bec_code <- "BC"

    gg_ld_class(class = "bec", bec_code)
  })

  ## Bar chart of land designations for selected bec zone
  output$bec_barchart <- renderggiraph({
    bec_code <- click_ids$bec_ids[length(click_ids$bec_ids)]
    if (length(bec_code) == 0 || bec_code == "BC") {
      bec_code <- "BC"
      df <- bc_ld_summary
      type <- "British Columbia"
    } else {
      df <- ld_bec_summary %>%
        filter(ZONE == bec_code) %>%
        group_by(prot_rollup, cons_cat) %>%
        summarize(area_des = sum(area_des, na.rm = TRUE),
                  bec_area = sum(bec_area, na.rm = TRUE),
                  percent_des = area_des / bec_area * 100,
                  area_des_ha = sum(area_des_ha, na.rm = TRUE))

      type <- "biogeoclimatic zone"
    }

    ggiraph_barchart(df, type)
  })

  output$bec_table <- DT::renderDataTable({
    bec_code <- click_ids$bec_ids[length(click_ids$bec_ids)]
    if (length(bec_code) == 0 || bec_code == "BC") {
      df <- summarize_bec(ld_bec_summary)
    } else {
      df <- ld_bec_summary %>%
        filter(ZONE == bec_code) %>%
        summarize_bec()
    }
    make_dt(df)
  })

})
