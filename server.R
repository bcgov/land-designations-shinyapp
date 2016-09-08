library(shiny)
library(leaflet)
library(feather)
library(ggplot2)
library(ggthemes)
library(plotly)

ecoregions <- readRDS("data/terr_ecoreg.rds")
gg_ld_x_ecoreg <- read_feather("data/gg_ld_ecoreg.feather")
gg_ecoreg <- read_feather("data/gg_ecoreg.feather")
ld_ecoreg_summary <- read_feather("data/ld_ecoreg_summary.feather")
ecoreg_ids <- ecoregions$ECOREGION_CODE

gg_ld_ecoreg <- function(ecoreg_cd, ld_df, ecoreg_df) {
  ld_df_sub <- ld_df[ld_df$CRGNCD == ecoreg_cd,]
  ecoreg_df_sub <- ecoreg_df[ecoreg_df$CRGNCD == ecoreg_cd, ]
  ggplot(ld_df_sub, aes(x = long, y = lat, group = group)) +
    geom_polygon(data = ecoreg_df_sub, fill = "grey85", colour = "gray40") +
    geom_polygon(aes(fill = cons_cat)) +
    ggtitle(tools::toTitleCase(tolower(ecoreg_df_sub$CRGNNM[1]))) +
    coord_fixed() +
    theme_map() +
    guides(fill = "none")
}

shinyServer(function(input, output, session) {

  # data <- reactiveValues(clickedMarker = NULL)

  output$bc_ecoreg_map <- renderLeaflet({
    leaflet(ecoregions) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(layerId = ecoreg_ids, color = "#00441b", weight = 1,
                  fillColor = "#c7e9c0")
  })

  ecoreg_re <- reactive({
    code <- input$bc_ecoreg_map_shape_click$id
    if (is.null(code)) return("BC")
    code
  })

  output$ecoreg_map <- renderPlot({
    ecoreg_code <- ecoreg_re()

    gg_ld_ecoreg(ecoreg_code, gg_ld_x_ecoreg, gg_ecoreg)
  })

  output$ecoreg_barchart <- renderPlotly({
    ecoreg_code <- ecoreg_re()

    gg <- ggplot(ld_ecoreg_summary[ld_ecoreg_summary$CRGNCD == ecoreg_code, ],
           aes(x = cons_cat, y = percent_des, fill = cons_cat,
               text = paste0("Area: ", round(area_des_ha), " ha (",
                            round(percent_des, 1), "%)"))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      coord_flip() +
      labs(x = "Designation type", y = "Percent of ecoregion designated", fill = NULL) +
      guides(fill = "none")

    ggplotly(gg, tooltip = "text") %>% layout(showlegend = FALSE)
  })
})
