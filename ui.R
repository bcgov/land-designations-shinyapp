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

shinyUI(fixedPage(
  theme = shinytheme("yeti"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tags$body(
    img(width = "200px", src = "ER_Logo.png")
  ),

  titlePanel("Land Designations that Contribute to Conservation"),
  tabsetPanel(
    tabPanel(
      "Ecoregions",
      fixedRow(column(12, h2("Representation by Ecoregion"))),
      fixedRow(column(6, leafletOutput("bc_ecoreg_map", height = 600),
               actionButton(inputId = "reset_bc_ecoreg", "Click to reset to B.C."),
               textOutput("reset_bc_ecoreg")), # for debugging click
               column(6,
                      plotOutput(outputId = "ecoreg_map"),
                      ggiraphOutput(outputId = "ecoreg_barchart",
                                    height = "100%"))),
      fixedRow(column(12,
                      dataTableOutput("ecoreg_table")))),
    tabPanel(
      "BEC",
      fixedRow(column(12, h2("Representation by Biogeoclimatic Zone"))),
      fixedRow(column(6, leafletOutput("bc_bec_map", height = 600),
                      actionButton(inputId = "reset_bc_bec", "Click to reset to B.C."),
                      textOutput("reset_bc_bec")), # for debugging click
               column(6,
                      plotOutput(outputId = "bec_map", height = 400),
                      ggiraphOutput(outputId = "bec_barchart",
                                    height = 200))),
      fixedRow(column(12,
                      dataTableOutput("bec_table")))))


))
