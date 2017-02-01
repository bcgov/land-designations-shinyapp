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
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  #tags$body(
  #  img(width = "200px", src = "ER_Logo.png")
  #),

  titlePanel("Explore Land Designations by Biogeoclimatic Zones & Ecoregions Across B.C."),
  tabsetPanel(
    tabPanel(
      "Biogeoclimatic Zones",
      # fluidRow(column(12, h3("Representation by Biogeoclimatic Zone"))),
      fluidRow(column(6,
                      div(class = "plot-container",
                          tags$img(src = "spinner.gif",
                                   class = "loading-spinner"),
                          leafletOutput("bc_bec_map", height = 600)),
                      br(),
                      actionButton(inputId = "reset_bc_bec", "Click to reset to B.C."),
                      downloadButton(outputId = "download_bec_data", "Download csv",
                                     class = "dl-button")),
                      # verbatimTextOutput("isbc")), # for debugging click
               column(6,
                      h4(htmlOutput("bec_title")),
                      conditionalPanel(
                        condition = "output.becisbc == false",
                        div(class = "plot-container",
                            tags$img(src = "spinner.gif", class = "loading-spinner"),
                            plotOutput(outputId = "bec_map", height = 500)
                        ),
                        plotlyOutput(outputId = "bec_barchart", height = 200)),
                      conditionalPanel(
                        condition = "output.becisbc == true",
                        plotlyOutput("bec_summary_plot", height = 700)))),
      br(),
      fluidRow(column(12,
                      dataTableOutput("bec_table")))),
    tabPanel(
      "Ecoregions",
      # fluidRow(column(12, h3("Representation by Ecoregion"))),
      fluidRow(column(6,
                      div(class = "plot-container",
                          tags$img(src = "spinner.gif",
                                   class = "loading-spinner"),
                          leafletOutput("bc_ecoreg_map", height = 600)),
                      br(),
                      actionButton(inputId = "reset_bc_ecoreg", "Click to reset to B.C."),
                      downloadButton(outputId = "download_ecoreg_data", "Download csv",
                                     class = "dl-button")),
               # textOutput("reset_bc_ecoreg")), # for debugging click
               column(6,
                      h4(htmlOutput("ecoreg_title")),
                      conditionalPanel(
                        condition = "output.ecoregisbc == false",
                        div(class = "plot-container",
                            tags$img(src = "spinner.gif", class = "loading-spinner"),
                            plotOutput(outputId = "ecoreg_map", height = 500)
                        ),
                        plotlyOutput(outputId = "ecoreg_barchart", height = 200)),
                      conditionalPanel(
                        condition = "output.ecoregisbc == true",
                        plotlyOutput("ecoreg_summary_plot", height = 700)))),
      br(),
      fluidRow(column(12,
                      dataTableOutput("ecoreg_table"))))


)))
