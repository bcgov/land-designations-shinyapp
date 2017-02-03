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
    }
  } else {
    ld_df <- gg_ld_x_ecoreg
    class_df <- bc_bound
  }

  ggplot(ld_df, aes(x = long, y = lat, group = group)) +
    geom_polypath(data = class_df, fill = "grey80", colour = "gray80") +
    geom_polypath(aes(fill = category)) +
    scale_fill_manual(values = des_cols) +
    coord_fixed(expand = FALSE) +
    theme_map() +
    guides(fill = "none")
}

## Interactive bar chart for % designated in selected ecoregion/zone
des_barchart <- function(df, type) {
  df <- df[!is.na(df$category), ]
  df$category_lab <- des_labels[df$category]
  df$prot_rollup <- prot_rollup_labels[as.character(df$prot_rollup)]
  p <- plot_ly(df, x = ~round(percent_des, 1), y = ~prot_rollup,
               type = "bar", color = ~category, colors = des_cols,
               hoverinfo = "x", alpha = 1)

  p <- layout(p, barmode = "stack", showlegend = FALSE,
              yaxis = list(autotick = FALSE, zeroline = FALSE,
                           tickfont = list(size = 12),
                           title = "",
                           categoryarray = rev(prot_rollup_labels),
                           categoryorder = "array"),
              xaxis = list(zeroline = FALSE, title = "Percent Designated"),
              font = list(family = '"Myriad-Pro",sans-serif', color = '#494949'),
              hovermode = "closest",
              margin = list(l = 150)) %>%
    config(displayModeBar = FALSE, collaborate = FALSE)
  p
}

## Shortcuts functions to initialize modifying ecoregion and bec leaflet maps
## (e.g., hover tips, highlight etc)
ecoreg_proxy <- function(...) leafletProxy("bc_ecoreg_map", ...)
bec_proxy <- function(...) leafletProxy("bc_bec_map", ...)

## Set view to BC
bc_view <- function(map) setView(map, lng = -126.5, lat = 54.5, zoom = 5)

add_tiles <- function(map) {
  addProviderTiles(map, "CartoDB.PositronNoLabels",
                   options = providerTileOptions(noWrap = TRUE))
}

## Convert & and emdashes to html strings for representing on the map
htmlize <- function(x) {
  x <- gsub("\\b&\\b", "&amp;", x, useBytes = TRUE)
  x <- gsub("--", "&mdash;", x, useBytes = TRUE)
  x
}

format_ha <- function(x) round(x, 0)
format_ha_comma <- function(x) format(x, digits = 0, big.mark = ",", scientific = FALSE)
format_percent <- function(x) round(x, 1)

highlight_clicked_poly <- function(map, clicked_polys, class) {

  if (class == "ecoreg") {
    colr <- rep("#00441b",2)
    fill <- "#006d2c"
    opac <- c(0.2, 0.8)
  } else if (class == "bec") {
    colr <- c("", "#2F4F4F")
    fill <- unname(na.omit(bec_colors[clicked_polys]))
    opac <- c(0.7, 0.9)
  }

  wts <- c(1, 2)

  if (length(clicked_polys) == 2 && clicked_polys[2] == "BC") {
    clicked_polys <- clicked_polys[1]
    wts <- wts[1]
    opac <- opac[1]
    colr <- colr[1]
  } else if (length(clicked_polys) == 2 && clicked_polys[1] == "BC") {
    clicked_polys <- clicked_polys[2]
    wts <- wts[2]
    opac <- opac[2]
    colr <- colr[2]
  } else if (length(clicked_polys) == 1) {
    wts <- wts[2]
    opac <- opac[2]
    colr <- colr[2]
  }

  addPolygons(map, layerId = clicked_polys,
              color = colr, fillColor = fill,
              weight = wts, fillOpacity = opac)
}

summarize_bec <- function(df, by_zone) {
  if (by_zone) {
    df <- df %>%
      group_by(Zone = ZONE, `Category` = category)
  } else {
    df <- df %>%
      group_by(Zone = ZONE, Subzone = SUBZONE_NAME, Variant = VARIANT_NAME, `BGC Label` = MAP_LABEL,
               `Category` = category)
  }
  df %>%
    summarize(`Area Designated (ha)` = format_ha(sum(area_des_ha, na.rm = TRUE)),
              `BGC Unit Area (ha)` = format_ha(sum(bec_area, na.rm = TRUE) * 1e-4),
              `Percent Designated` = format_percent((sum(area_des, na.rm = TRUE) /
                                                       sum(bec_area, na.rm = TRUE)) * 100))
}

summarize_ecoreg <- function(df) {
  df$Ecoregion <- ecoreg_nms[df$ecoregion_code]
  df %>%
    group_by(Ecoregion, `Category` = category) %>%
    summarize(`Area Designated (ha)` = format_ha(sum(area_des_ha, na.rm = TRUE)),
              `Ecoregion Area (ha)` = format_ha(ecoreg_area * 1e-4),
              `Percent Designated` = format_percent((sum(area_des, na.rm = TRUE) /
                                                       sum(ecoreg_area, na.rm = TRUE)) * 100))
}

format_if_exists <- function(dt, column) {
  if (column %in% names(dt$x$data)) {
    dt <- formatCurrency(dt, column, currency = "", digits = 0)
  }
  dt
}

make_dt <- function(df) {
  df[["Category"]] <- des_labels[df[["Category"]]]
  categories <- unique(df[["Category"]])
  cat_colours <- des_cols
  if (anyNA(categories)) cat_colours <- c(cat_colours, 'lightgrey')

  datatable(df, filter = "top", rownames = FALSE, options = list(pageLength = 10),
            fillContainer = FALSE) %>%
    formatStyle('Percent Designated',
                background = styleColorBar(df[["Percent Designated"]], 'green')) %>%
    formatStyle('Category', target = "cell", fontWeight = 'bold',
                color = styleEqual(categories, cat_colours)) %>%
    formatCurrency('Percent Designated', currency = "%", before = FALSE, digits = 1) %>%
    formatCurrency('Area Designated (ha)', currency = "", digits = 0) %>%
    format_if_exists('Ecoregion Area (ha)') %>%
    format_if_exists('BGC Unit Area (ha)')
}

rollup_category <- function(category) {
  factor(ifelse(category %in% c("01_PPA", "02_Protected_Other"),
                "Prot", category),
         levels = c("04_Managed", "03_Exclude_1_2_Activities", "Prot"), ordered = TRUE)
}

reverse_factor <- function(x) {
  if (!is.factor(x)) x <- factor(x)
  ordered(x, levels = rev(levels(x)))
}

plotly_class <- function(data, class, cat, highlight = NULL) {

  if (class == "bec") {
    percent <- "Percent Designated"
    prot_rollup <- "prot_rollup"
    code_col <- "Zone"
    category_col <- "Category"
    title <- "Biogeoclimatic Zone"
  } else if (class == "ecoreg") {
    percent <- "percent_des"
    prot_rollup <- "prot_rollup"
    code_col <- "ecoregion_code"
    category_col <- "category"
    title <- "Ecoregion Code"
  }

  max_percent <- max(data[[percent]], na.rm = TRUE)
  data <- data[data[[prot_rollup]] == cat, ]

  if (!is.null(highlight)) {
    high_dat <- data[data[[code_col]] == highlight, ]
    data <- data[data[[code_col]] != highlight, ]
  }

  as_formula <- function(var, ticks = FALSE) {
    if (ticks) var <- paste0("`", var, "`")
    as.formula(paste0("~",var))
  }

  p <- plot_ly(data, x = as_formula(percent, TRUE),
               y = as_formula(code_col, TRUE)) %>%
    add_bars(color = as_formula(category_col, TRUE), colors = des_cols,
             text = as_formula(paste0("paste0(round(`", percent, "`, 1), ' %')")),
             hoverinfo = "text", opacity = 0.6)

  hoverlabel <- NULL

  if (!is.null(highlight)) {
    p <- add_bars(p, data = high_dat, color = as_formula(category_col, TRUE),
                  colors = des_cols, opacity = 1,
                  hoverinfo = "skip")
    ref <- switch(cat, "Prot" = "",
                  "03_Exclude_1_2_Activities" = "2",
                  "04_Managed" = "3")
    hoverlabel <- list(x = sum(high_dat[[percent]]), y = high_dat[[code_col]],
                       text = paste0(round(high_dat[[percent]], 1), "%", collapse = ", "),
                       showarrow = TRUE, arrowhead = 0, ax = 10, ay = 2,
                       arrowwidth = 1,
                       xref = paste0("x", ref), xanchor = "left",
                       yref = paste0("y", ref), yanchor = "top",
                       bgcolor = "rgb(229,229,229)",
                       bordercolor = "rgb(179,179,179)",
                       align = "right")
  }

  layout(p, barmode = "stack", showlegend = FALSE,
         yaxis = list(autotick = FALSE, zeroline = FALSE,
                      tickfont = list(size = 10.5),
                      title = title),
         xaxis = list(zeroline = FALSE, title = "Percent Designated",
                      range = c(0, ceiling(max_percent/10) * 10)),
         font = list(family = '"Myriad-Pro",sans-serif', color = '#494949'),
         hovermode = "closest", annotations = hoverlabel,
         margin = list(t = 40))
}

subplotly <- function(data, which, highlight_id = NULL, by) {

  if (by == "rows") {
    n_rows <- 3
    share_x <- TRUE
    which_ax <- "yaxis"
    x_pos <- 0.5
    x_anchor = "center"
  } else if ( by == "cols" ) {
    n_rows <- 1
    share_x <- FALSE
    which_ax <- "xaxis"
    y_pos <- 1
    x_anchor = "left"
  }

  sp <- subplot(plotly_class(data, class = which, cat = "Prot",
                             highlight = highlight_id),
                plotly_class(data, class = which, cat = "03_Exclude_1_2_Activities",
                             highlight = highlight_id),
                plotly_class(data, class = which, cat = "04_Managed",
                             highlight = highlight_id),
                nrows = n_rows,
                shareX = share_x,
                shareY = !share_x)

  if (is.null(sp$x$layout$annotations)) {
    sp$x$layout$annotations <- list()
  }

  for (i in seq_along(prot_rollup_labels)) {
    a_i <- length(sp$x$layout$annotations) + i
    ax <- ifelse(i == 1, which_ax, paste0(which_ax, i))
    ax_pos <- max(sp$x$layout[[ax]]$domain)

    if (by == "rows") {
      y_pos <- ax_pos
    } else if ( by == "cols" ) {
      x_pos <- ax_pos - 0.33
    }

    sp$x$layout$annotations[[a_i]] <- list(x = x_pos, y = y_pos,
                                           text = sub("\\s+", "\n", prot_rollup_labels[i]),
                                           showarrow = FALSE,
                                           xanchor = x_anchor, yanchor = "bottom",
                                           xref = 'paper', yref = 'paper',
                                           align = "left")
  }
  sp %>% config(displayModeBar = FALSE, collaborate = FALSE)
}

