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
library(ggpolypath)
library(ggiraph)
library(DT)

rollup_category <- function(category) {
  factor(ifelse(category %in% c("A", "B"),
                "Prot", category),
         levels = c("Prot", "C", "D"), ordered = TRUE)
}

bc_bound <- read_feather("data/gg_bc_bound.feather")
bc_ld_summary <- read_feather("data/bc_ld_summary.feather")

bc_ld_summary$prot_rollup <- rollup_category(bc_ld_summary$cons_cat)

## Ecoregion data
ecoregions <- readRDS("data/ecoregions_t_leaflet.rds")
gg_ld_x_ecoreg <- read_feather("data/gg_ld_ecoreg.feather")
gg_ecoreg <- read_feather("data/gg_ecoreg.feather")
ld_ecoreg_summary <- read_feather("data/ld_ecoreg_summary.feather")
ecoreg_ids <- ecoregions$CRGNCD
ecoreg_nms <- structure(ecoregions$CRGNNM, names = ecoreg_ids)
# ecoregion_centroids <- as.data.frame(coordinates(ecoregions))
# names(ecoregion_centroids) <- c("long", "lat")
# rownames(ecoregion_centroids) <- ecoreg_ids

ld_ecoreg_summary$prot_rollup <- rollup_category(ld_ecoreg_summary$cons_cat)

# BEC Data
bec_zones <- readRDS("data/bec_leaflet.rds") # SpatialPolygonsDataFrame
gg_ld_x_bec <- read_feather("data/gg_ld_bec.feather")
gg_bec <- read_feather("data/gg_bec.feather")
ld_bec_summary <- read_feather("data/ld_bec_summary.feather")

ld_bec_summary$prot_rollup <- rollup_category(ld_bec_summary$cons_cat)

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

des_cols <- c("A" = "#00441b",
              "B" = "#006d2c",
              "C" = "#a6d96a",
              "D" = "#fdbf6f")


des_labels <- c("A" = "Parks and Protected Areas",
                "B" = "Other Protected Lands",
                "C" = "Exclude 1 or 2 Actitivies",
                "D" = "Managed Lands")

prot_rollup_labels <- c("Prot" = "Protected",
                        "C"    = "Exclude 1 or 2 Actitivies",
                        "D"    = "Managed Lands")

# des_cols <- c("01_PPA"                    = "#00441b",
#               "02_Protected_Other"        = "#006d2c",
#               "03_Exclude_1_2_Activities" = "#a6d96a",
#               "04_Managed"                = "#fdbf6f")
#
# des_labels = c("01_PPA"                    = "Parks and Protected Areas",
#                "02_Protected_Other"        = "Other Protected Lands",
#                "03_Exclude_1_2_Activities" = "Exclude 1 or 2 Actitivies",
#                "04_Managed"                = "Managed Lands")
#
# prot_rollup_labels <- c("Protected"                 = "Protected",
#                         "03_Exclude_1_2_Activities" = "Exclude 1 or 2 Actitivies",
#                         "04_Managed"                = "Managed Lands")
