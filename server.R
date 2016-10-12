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
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggiraph)
library(DT)

bc_bound <- read_feather("data/gg_bc_bound.feather")
bc_ld_summary <- read_feather("data/bc_ld_summary.feather")

## Ecoregion data
ecoregions <- readRDS("data/ecoregions_t_leaflet.rds")
gg_ld_x_ecoreg <- read_feather("data/gg_ld_ecoreg.feather")
gg_ecoreg <- read_feather("data/gg_ecoreg.feather")
ld_ecoreg_summary <- read_feather("data/ld_ecoreg_summary.feather")
ecoreg_ids <- ecoregions$CRGNCD
ecoreg_nms <- ecoregions$CRGNNM
# ecoregion_centroids <- as.data.frame(coordinates(ecoregions))
# names(ecoregion_centroids) <- c("long", "lat")
# rownames(ecoregion_centroids) <- ecoreg_ids

# BEC Data
bec_zones <- readRDS("data/bec_leaflet.rds")
gg_ld_x_bec <- read_feather("data/gg_ld_bec.feather")
gg_bec <- read_feather("data/gg_bec.feather")
ld_bec_summary <- read_feather("data/ld_bec_summary.feather")
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
                CDF = "#FFFF00")[bec_ids] # index by bec_ids to put in order

## Plot selected ecoregion/bec zone with designations
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

## Interactive bar chart for % designated in selected ecoregion/zone
ggiraph_barchart <- function(df, type) {
  tooltip_css = "background-color:white;
  padding:5px;
  border-radius:10px;"

  hover_css <- "opacity:0.5;stroke:white;"

  df$hovertip <- paste0("Area: ",
                        format_ha(df$area_des_ha),
                        " ha (",
                        format_percent(df$percent_des), "%)")
  gg <- ggplot(df,
               aes(x = cons_cat, y = percent_des)) +
    geom_bar_interactive(stat = "identity",
                         aes(fill = cons_cat, tooltip = hovertip, data_id = hovertip)) +
    theme_minimal(base_size = 16) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    coord_flip() +
    labs(x = "Designation type", y = paste0("Percent of ", type, " designated")) +
    guides(fill = "none")

  ggiraph(code = print(gg), width = 1, height_svg = 3,
          tooltip_extra_css = tooltip_css, tooltip_opacity = 0.75,
          hover_css = hover_css, tooltip_offx = -20)
}

## Shortcuts functions to initialize modifying ecoregion and bec leaflet maps
## (e.g., hover tips, highlight etc)
ecoreg_proxy <- function(...) leafletProxy("bc_ecoreg_map", ...)
bec_proxy <- function(...) leafletProxy("bc_bec_map", ...)

## Set view to BC
bc_view <- function(map) setView(map, lng = -126.5, lat = 54.5, zoom = 5)

## Convert & and emdashes to html strings for representing on the map
htmlize <- function(x) {
  x <- gsub("\\b&\\b", "&amp;", x, useBytes = TRUE)
  x <- gsub("--", "&mdash;", x, useBytes = TRUE)
  x
}

format_ha <- function(x) format(x, digits = 0, big.mark = ",", scientific = FALSE)
format_percent <- function(x) round(x, 1)

highlight_clicked_poly <- function(map, clicked_polys, class) {

  if (class == "ecoreg") {
    colr <- rep("#00441b",2)
    fill <- "#006d2c"
    opac <- c(0.2, 0.8)
  } else if (class == "bec") {
    colr <- c("", "#2F4F4F")
    fill <- unname(bec_colors[clicked_polys])
    opac <- c(0.7, 0.9)
  }

  wts <- c(1, 2)

  if (length(clicked_polys) == 1) {
    wts <- wts[2]
    opac <- opac[2]
    colr <- colr[2]
  }

  addPolygons(map, layerId = clicked_polys,
              color = colr, fillColor = fill,
              weight = wts, fillOpacity = opac)
}

summarize_bec <- function(df) {
  df %>%
  group_by(`BGC Label` = MAP_LABEL, Subzone = SBZNNM, Variant = VRNTNM,
           `Conservation Category` = cons_cat) %>%
    summarize(`Area designated (ha)` = format_ha(sum(area_des_ha, na.rm = TRUE)),
              `Percent Designated` = format_percent((sum(area_des, na.rm = TRUE) /
                                                       sum(bec_area, na.rm = TRUE)) * 100))
}

shinyServer(function(input, output, session) {
  # Reactive values list to keep track of clicked polygons
  click_ids <- reactiveValues(ecoreg_ids = character(0),
                              bec_ids = character(0))

  # Keep track of current clicked polygon and previous. Store in reactive values list
  observeEvent(input$bc_ecoreg_map_shape_click$id, {
    prev_click_id <- click_ids$ecoreg_ids[length(click_ids$ecoreg_ids)]
    click_ids$ecoreg_ids <- c(prev_click_id, input$bc_ecoreg_map_shape_click$id)
  })

  output$click_ids <- renderText(click_ids$ecoreg_ids) # For debugging click

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
    # subset the ecoregions to be updated to those that are either currently clicked
    # or previously clicked. Using match subsets AND sorts the SPDF according to the order of
    # clicked_polys, as the first one is the previously clicked one, and the second
    # is the currently clicked. The polygons must be in the same order as the aesthetics
    # (col, fill, weight, etc)
    ecoreg_subset <- ecoregions[match(clicked_polys, ecoregions$CRGNCD), ]

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
    if (length(ecoreg_code) == 0) {
      ecoreg_code <- "BC"
      df <- bc_ld_summary
      type <- "British Columbia"
    } else {
      df <- ld_ecoreg_summary[ld_ecoreg_summary$CRGNCD == ecoreg_code, ]
      type <- "ecoregion"
    }

    ggiraph_barchart(df, type)
  })

  #### BEC #####################################################################

  # Keep track of current clicked polygon and previous. Store in reactive values list
  observeEvent(input$bc_bec_map_shape_click$id, {
    prev_click_id <- click_ids$bec_ids[length(click_ids$bec_ids)]
    click_ids$bec_ids <- c(prev_click_id, input$bc_bec_map_shape_click$id)
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
    # subset the bec polygons to be updated to those that are either currently clicked
    # or previously clicked. Using match subsets AND sorts the SPDF according to the order of
    # clicked_polys, as the first one is the previously clicked one, and the second
    # is the currently clicked. The polygons must be in the same order as the aesthetics
    # (col, fill, weight, etc)
    bec_subset <- bec_zones[match(clicked_polys, bec_zones$ZONE), ]

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
    if (length(bec_code) == 0) {
      bec_code <- "BC"
      df <- bc_ld_summary
      type <- "British Columbia"
    } else {
      df <- ld_bec_summary %>%
        filter(ZONE == bec_code) %>%
        group_by(cons_cat) %>%
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
    if (length(bec_code) == 0) {
      df <- ld_bec_summary %>%
        summarize_bec()
    } else {
      df <- ld_bec_summary %>%
        filter(ZONE == bec_code) %>%
        summarize_bec()
    }
    datatable(df, filter = "top", options = list(pageLength = 25)) %>%
      formatStyle('Percent Designated',
                  background = styleColorBar(df$`Percent Designated`, 'green')) %>%
      formatStyle('Conservation Category', target = "cell",
                  backgroundColor = styleEqual(unique(ld_bec_summary$cons_cat),
                                               c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", 'lightgrey')),
                  fillOpacity = 0.7)
  })

})
