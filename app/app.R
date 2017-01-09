library('shiny')
library('leaflet')
library('feather')
library('dplyr')
library('ggplot2')
library('ggthemes')
library('ggpolypath')
library('ggiraph')
library('DT')

library('shinythemes')

options(shiny.sanitize.errors = FALSE)

source("server_functions.R", local = TRUE)
source("server_objects.R", local = TRUE)

ui <- fixedPage(
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
                      br(),
                      actionButton(inputId = "reset_bc_ecoreg", "Click to reset to B.C."),
                      downloadButton(outputId = "download_ecoreg_data", "Download csv",
                                     class = "dl-button")),
               # textOutput("reset_bc_ecoreg")), # for debugging click
               column(6,
                      h4(htmlOutput("ecoreg_title")),
                      plotOutput(outputId = "ecoreg_map", height = 500),
                      ggiraphOutput(outputId = "ecoreg_barchart",
                                    height = "100%"))),
      br(),
      fixedRow(column(12,
                      dataTableOutput("ecoreg_table")))),
    tabPanel(
      "Biogeoclimatic Zones",
      fixedRow(column(12, h2("Representation by Biogeoclimatic Zone"))),
      fixedRow(column(6, leafletOutput("bc_bec_map", height = 600),
                      br(),
                      actionButton(inputId = "reset_bc_bec", "Click to reset to B.C."),
                      downloadButton(outputId = "download_bec_data", "Download csv",
                                     class = "dl-button")),
               # textOutput("reset_bc_bec")), # for debugging click
               column(6,
                      h4(htmlOutput("bec_title")),
                      plotOutput(outputId = "bec_map", height = 500),
                      ggiraphOutput(outputId = "bec_barchart",
                                    height = 200))),
      br(),
      fixedRow(column(12,
                      dataTableOutput("bec_table")))))


)

server <- function(input, output, session) {
  # Reactive values list to keep track of clicked polygons
  ecoreg_reactives <- reactiveValues(ecoreg_ids = character(0),
                                     ecoreg_summary = ld_ecoreg_summary)

  # Keep track of current clicked polygon and previous. Store in reactive values list
  observeEvent(input$bc_ecoreg_map_shape_click$id, {
    clicked_id <- input$bc_ecoreg_map_shape_click$id
    prev_click_id <- ecoreg_reactives$ecoreg_ids[length(ecoreg_reactives$ecoreg_ids)]
    ecoreg_reactives$ecoreg_ids <- c(prev_click_id, clicked_id)

    ecoreg_reactives$ecoreg_summary <- ld_ecoreg_summary %>%
      filter(CRGNCD == clicked_id)
  })

  observeEvent(input$reset_bc_ecoreg, {
    prev_click_id <- ecoreg_reactives$ecoreg_ids[length(ecoreg_reactives$ecoreg_ids)]
    ecoreg_reactives$ecoreg_ids <- c(prev_click_id, "BC")

    ecoreg_reactives$ecoreg_summary <- ld_ecoreg_summary
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
    clicked_polys <- ecoreg_reactives$ecoreg_ids

    # output$reset_bc_ecoreg <- renderText(clicked_polys)

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

  output$ecoreg_title <- renderText({
    ecoreg_code <- ecoreg_reactives$ecoreg_ids[length(ecoreg_reactives$ecoreg_ids)]
    if (length(ecoreg_code) == 0) return("British Columbia")
    ecoreg_nms[ecoreg_code]
  })

  ## Subset map of ecoregion with land designations
  output$ecoreg_map <- renderPlot({
    ecoreg_code <- ecoreg_reactives$ecoreg_ids[length(ecoreg_reactives$ecoreg_ids)]
    if (length(ecoreg_code) == 0) ecoreg_code <- "BC"

    gg_ld_class(class = "ecoreg", ecoreg_code)
  })

  ## Bar chart of land designations for selected ecoregion
  output$ecoreg_barchart <- renderggiraph({
    ecoreg_code <- ecoreg_reactives$ecoreg_ids[length(ecoreg_reactives$ecoreg_ids)]

    if (length(ecoreg_code) == 0 || ecoreg_code == "BC") {
      ecoreg_code <- "BC"
      type <- "British Columbia"
      df <- bc_ld_summary
    } else {
      type <- "ecoregion"
      df <- ecoreg_reactives$ecoreg_summary
    }

    ggiraph_barchart(df, type)
  })

  output$ecoreg_table <- DT::renderDataTable({
    ecoreg_code <- ecoreg_reactives$ecoreg_ids[length(ecoreg_reactives$ecoreg_ids)]
    df <- ecoreg_reactives$ecoreg_summary

    summarize_ecoreg(df) %>%
      make_dt()
  })

  output$download_ecoreg_data <- downloadHandler(
    filename = function() "ecoregion.csv",
    content = function(file) {
      write.csv(ecoreg_reactives$ecoreg_summary, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  #### BEC #####################################################################

  bec_reactives <- reactiveValues(bec_ids = character(0),
                                  bec_summary = ld_bec_summary)

  # Keep track of current clicked polygon and previous. Store in reactive values list
  observeEvent(input$bc_bec_map_shape_click$id, {
    clicked_id <- input$bc_bec_map_shape_click$id
    prev_click_id <- bec_reactives$bec_ids[length(bec_reactives$bec_ids)]
    bec_reactives$bec_ids <- c(prev_click_id, clicked_id)

    bec_reactives$bec_summary <- ld_bec_summary %>%
      filter(ZONE == clicked_id)
  })

  observeEvent(input$reset_bc_bec, {
    prev_click_id <- bec_reactives$bec_ids[length(bec_reactives$bec_ids)]
    bec_reactives$bec_ids <- c(prev_click_id, "BC")

    bec_reactives$bec_summary <- ld_bec_summary
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
    clicked_polys <- bec_reactives$bec_ids

    # output$reset_bc_bec <- renderText(clicked_polys)

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

  output$bec_title <- renderText({
    bec_code <- bec_reactives$bec_ids[length(bec_reactives$bec_ids)]
    if (length(bec_code) == 0) return("British Columbia")
    htmlize(bec_nms[bec_code])
  })

  ## Subset map of bec zone with land designations
  output$bec_map <- renderPlot({
    bec_code <- bec_reactives$bec_ids[length(bec_reactives$bec_ids)]
    if (length(bec_code) == 0) bec_code <- "BC"

    gg_ld_class(class = "bec", bec_code)
  })

  ## Bar chart of land designations for selected bec zone
  output$bec_barchart <- renderggiraph({
    bec_code <- bec_reactives$bec_ids[length(bec_reactives$bec_ids)]
    if (length(bec_code) == 0 || bec_code == "BC") {
      bec_code <- "BC"
      df <- bc_ld_summary
      type <- "British Columbia"
    } else {
      df <- bec_reactives$bec_summary %>%
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
    bec_code <- bec_reactives$bec_ids[length(bec_reactives$bec_ids)]
    df <- bec_reactives$bec_summary

    summarize_bec(df) %>%
      make_dt()
  })

  output$download_bec_data <- downloadHandler(
    filename = function() "bec.csv",
    content = function(file) {
      write.csv(bec_reactives$bec_summary, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

}

shinyApp(ui = ui, server = server)
