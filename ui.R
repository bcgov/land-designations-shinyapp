
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Protection of ecoregions and Biogeoclimatic Zones"),
  tabsetPanel(
    tabPanel(
      "Ecoregions",
             fluidRow(column(5, leafletOutput("bc_ecoreg_map", height = 600)),
                      column(7,
                             plotOutput(outputId = "ecoreg_map"),
                             plotlyOutput(outputId = "ecoreg_barchart",
                                        height = "200px")))),
    tabPanel("BEC","content")
  )


))
