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

library(shiny)
library(leaflet)
library(feather)
library(ggplot2)
library(ggthemes)
library(plotly)

bc_bound <- read_feather("data/gg_bc_bound.feather")
ecoregions <- readRDS("data/ecoregions_t_leaflet.rds")
gg_ld_x_ecoreg <- read_feather("data/gg_ld_ecoreg.feather")
gg_ecoreg <- read_feather("data/gg_ecoreg.feather")
ld_ecoreg_summary <- read_feather("data/ld_ecoreg_summary.feather")
ecoreg_ids <- ecoregions$CRGNCD
ecoreg_nms <- ecoregions$CRGNNM
ecoregion_centroids <- as.data.frame(coordinates(ecoregions))
names(ecoregion_centroids) <- c("long", "lat")
rownames(ecoregion_centroids) <- ecoreg_ids

## TODO:
bec_zones <- readRDS("data/bec_leaflet.rds")
gg_ld_x_bec <- read_feather("data/gg_ld_bec.feather")
gg_bec <- read_feather("data/gg_bec.feather")
# ld_bec_summary <-
bec_ids <- bec_zones$ZONE
bec_nms <- c(BAFA = "Boreal Altai Fescue Alpine",
             SWB = "Spruce--Willow--Birch",
             BWBS = "Boreal White & Black Spruce",
             ESSF = "Engelmann Spruce--Subalpine Fir",
             CMA = "Coastal Mountain-heather Alpine",
             SBS = "Sub-Boreal Spruce",
             MH = "Mountain Hemlock",
             CWH = "Coastal Western Hemlock",
             ICH = "Interior Cedar--Hemlock",
             IMA = "Interior Mountain-heather Alpine",
             SBPS = "Sub-Boreal Pine--Spruce",
             MS = "Montane Spruce",
             IDF = "Interior Douglas-fir",
             BG = "Bunchgrass",
             PP = "Ponderosa Pine",
             CDF = "Coastal Douglas-fir")
bec_colors <- c(BAFA = "#E5D8B1", SWB = "#A3D1AB", BWBS = "#ABE7FF",
                ESSF = "#9E33D3", CMA = "#E5C7C7", SBS = "#2D8CBD",
                MH = "#A599FF", CWH = "#208500", ICH = "#85A303",
                IMA = "#B2B2B2", SBPS = "#36DEFC", MS = "#FF46A3",
                IDF = "#FFCF00", BG = "#FF0000", PP = "#DE7D00",
                CDF = "#FFFF00")[bec_ids]

gg_ld_class <- function(class, reg_cd) {
  if (reg_cd != "BC") {
    if (class == "ecoreg") {
      ld_df <- gg_ld_x_ecoreg[gg_ld_x_ecoreg$CRGNCD == reg_cd,]
      class_df <- gg_ecoreg[gg_ecoreg$CRGNCD == reg_cd, ]
      title <- ecoreg_nms[ecoreg_ids == reg_cd]
    } else if (class == "bec") {
      ld_df <- gg_ld_x_bec[gg_ld_x_bec$ZONE == reg_cd, ]
      class_df <- gg_bec[gg_bec$ZONE == reg_cd, ]
      title <- bec_nms[reg_cd]
    }
  } else {
    ld_df <- gg_ld_x_ecoreg
    class_df <- bc_bound
    title <- "British Columbia"
  }

  ggplot(ld_df, aes(x = long, y = lat, group = group)) +
    geom_polygon(data = class_df, fill = "grey80", colour = "gray80") +
    geom_polygon(aes(fill = cons_cat)) +
    ggtitle(title) +
    coord_fixed() +
    theme_map() +
    guides(fill = "none")
}

plotly_barchart <- function(df) {
  gg <- ggplot(df,
               aes(x = cons_cat, y = percent_des, fill = cons_cat,
                   text = paste0("Area: ", round(area_des_ha), " ha (",
                                 round(percent_des, 1), "%)"))) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip() +
    labs(x = "Designation type", y = "Percent of ecoregion designated", fill = NULL) +
    guides(fill = "none")

  ggplotly(gg, tooltip = "text") %>% layout(showlegend = FALSE)
}

ecoreg_proxy <- function(...) leafletProxy("bc_ecoreg_map", ...)
bec_proxy <- function(...) leafletProxy("bc_bec_map", ...)

bc_view <- function(map) setView(map, lng = -126.5, lat = 54.5, zoom = 5)

htmlize <- function(x) {
  x <- gsub("\\b&\\b", "&amp;", x, useBytes = TRUE)
  x <- gsub("--", "&mdash;", x, useBytes = TRUE)
  x
}

highlight_clicked_poly <- function(map, clicked_polys) {
  if (length(clicked_polys) > 1) {
    wts <- c(1, 2)
    opac <- c(0.2, 0.8)
  } else {
    wts <- 2
    opac <- 0.8
  }
  addPolygons(map, layerId = clicked_polys,
              color = "#00441b", fillColor = "#006d2c",
              weight = wts, fillOpacity = opac)
}

shinyServer(function(input, output, session) {
  # Reactive values list to keep track of clicked polygons
  ecoreg_click_ids <- reactiveValues(ids = character(0))

  # Keep track of current clicked polygon and previous
  observeEvent(input$bc_ecoreg_map_shape_click$id, {
    prev_click_id <- ecoreg_click_ids$ids[length(ecoreg_click_ids$ids)]
    ecoreg_click_ids$ids <- c(prev_click_id, input$bc_ecoreg_map_shape_click$id)
  })

  # output$click_ids <- renderText(ecoreg_click_ids$ids)

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
    clicked_polys <- ecoreg_click_ids$ids
    ecoreg_subset <- ecoregions[match(clicked_polys, ecoregions$CRGNCD), ]

    ecoreg_proxy(data = ecoreg_subset) %>%
      highlight_clicked_poly(clicked_polys)
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

  ## Ecoregion map and barchart
  # ecoreg_re <- reactive({
  #   code <- input$bc_ecoreg_map_shape_click$id
  #   if (is.null(code)) return("BC")
  #   code
  # })

  ## Subset map of ecoregion with land designations
  output$ecoreg_map <- renderPlot({
    ecoreg_code <- ecoreg_click_ids$ids[length(ecoreg_click_ids$ids)]
    if (length(ecoreg_code) == 0) ecoreg_code <- "BC"

    gg_ld_class(class = "ecoreg", ecoreg_code)
  })

  ## Bar chart of land designations for selected ecoregion
  output$ecoreg_barchart <- renderPlotly({
    ecoreg_code <- ecoreg_click_ids$ids[length(ecoreg_click_ids$ids)]
    if (length(ecoreg_code) == 0) ecoreg_code <- "BC"

    df <- ld_ecoreg_summary[ld_ecoreg_summary$CRGNCD == ecoreg_code, ]

    plotly_barchart(df)
})

  # ## BEC Reactives
  ## Highlighting BEC polygons on click is too slow
  # add_bec_polys <- reactive({
  #   bec_code <- input$bc_bec_map_shape_click$id
  #   opac <- rep(0.6, length(bec_ids))
  #   names(opac) <- bec_ids
  #   if (!is.null(bec_code)) {
  #     opac[bec_code] <- 0.8
  #   }
  #
  #   function(mapid) {
  #     addPolygons(mapid, layerId = bec_zones$ZONE, color = "",
  #                 fillColor = unname(bec_colors), fillOpacity = unname(opac))
  #   }
  # })
  #
  # add_bec_popup <- reactive({
  #   reg_id <- input$bc_bec_map_shape_mouseover$id
  #   lat <- bec_centroids[reg_id, "lat"]
  #   lng <- bec_centroids[reg_id, "long"]
  #   reg_name <- reg_id
  #
  #   function(map_id) addPopups(map_id, lat = lat, lng = lng, reg_name,
  #                              options = popupOptions(closeButton = FALSE, className = 'ecoreg-popup'))
  # })
  #
  # ## BEC leaflet map
  output$bc_bec_map <- renderLeaflet({
    leaflet(bec_zones) %>%
      bc_view() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(layerId = bec_zones$ZONE, color = "",
                  fillColor = unname(bec_colors), fillOpacity = 0.7)
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

  ## Observer for highlighting polygon on click
  # observe({
  #   add_polygons <- add_bec_polys()
  #   bec_proxy() %>%
  #     add_polygons()
  # })
  #
  # ## BEC map and barchart
  bec_re <- reactive({
    code <- input$bc_bec_map_shape_click$id
    if (is.null(code)) return("BC")
    code
  })

  ## Subset map of bec zone with land designations
  output$bec_map <- renderPlot({
    bec_code <- bec_re()

    gg_ld_class(class = "bec", bec_code)
  })

})
