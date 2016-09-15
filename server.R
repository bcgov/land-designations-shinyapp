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
centroids <- as.data.frame(coordinates(ecoregions))
names(centroids) <- c("long", "lat")
rownames(centroids) <- ecoreg_ids

gg_ld_ecoreg <- function(ecoreg_cd) {
  if (ecoreg_cd != "BC") {
    ld_df <- gg_ld_x_ecoreg[gg_ld_x_ecoreg$CRGNCD == ecoreg_cd,]
    ecoreg_df <- gg_ecoreg[gg_ecoreg$CRGNCD == ecoreg_cd, ]
    title <- tools::toTitleCase(tolower(ecoreg_df$CRGNNM[1]))
  } else {
    ld_df <- gg_ld_x_ecoreg
    ecoreg_df <- bc_bound
    title <- "British Columbia"
  }

  ggplot(ld_df, aes(x = long, y = lat, group = group)) +
    geom_polygon(data = ecoreg_df, fill = "grey85", colour = "gray40") +
    geom_polygon(aes(fill = cons_cat)) +
    ggtitle(title) +
    coord_fixed() +
    theme_map() +
    guides(fill = "none")
}

plotly_barchart <- function(ecoreg_cd) {
  gg <- ggplot(ld_ecoreg_summary[ld_ecoreg_summary$CRGNCD == ecoreg_cd, ],
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

shinyServer(function(input, output, session) {

  add_polys <- reactive({
    er_code <- input$bc_ecoreg_map_shape_click$id

    wts <- rep(1, length(ecoreg_ids))
    opac <- rep(0.2, length(ecoreg_ids))
    names(wts) <- names(opac) <- ecoreg_ids
    if (!is.null(er_code)) {
      wts[er_code] <- 2
      opac[er_code] <- 0.8
    }

    function(mapid) {
      addPolygons(mapid, layerId = ecoregions$CRGNCD, color = "#00441b", fillColor = "#006d2c",
                weight = unname(wts), fillOpacity = unname(opac))
    }
  })

  add_popup <- reactive({
    pointId <- input$bc_ecoreg_map_shape_mouseover$id
    lat <- centroids[pointId, "lat"]
    lng <- centroids[pointId, "long"]

    function(map_id) addPopups(map_id, lat = lat, lng = lng, as.character(pointId),
                               options = popupOptions(closeButton = FALSE, className = 'popup'))
  })

  output$bc_ecoreg_map <- renderLeaflet({
    leaflet(ecoregions) %>%
      fitBounds(-139, 48, -114, 60) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      )
  })

  observeEvent(input$bc_ecoreg_map_shape_mouseout$id, {
    leafletProxy("bc_ecoreg_map") %>% clearPopups()
  })

  observe({
    add_eco_popup <- add_popup()
    leafletProxy("bc_ecoreg_map") %>% add_eco_popup()
  })

  observe({
    add_polygons <- add_polys()
    leafletProxy("bc_ecoreg_map", data = ecoregions) %>%
      add_polygons()
  })

  ecoreg_re <- reactive({
    code <- input$bc_ecoreg_map_shape_click$id
    if (is.null(code)) return("BC")
    code
  })

  output$ecoreg_map <- renderPlot({
    ecoreg_code <- ecoreg_re()

    gg_ld_ecoreg(ecoreg_code)
  })

  output$ecoreg_barchart <- renderPlotly({
    ecoreg_code <- ecoreg_re()

    plotly_barchart(ecoreg_code)
  })
})
