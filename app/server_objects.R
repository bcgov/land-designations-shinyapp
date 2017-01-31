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

bc_bound <- read_feather("data/gg_bc_bound.feather")
bc_ld_summary <- read_feather("data/bc_ld_summary.feather") %>%
  mutate(prot_rollup = rollup_category(category))

## Ecoregion data
ecoregions <- readRDS("data/ecoregions_t_leaflet.rds")
gg_ld_x_ecoreg <- read_feather("data/gg_ld_ecoreg.feather")

gg_ecoreg <- read_feather("data/gg_ecoreg.feather")
ld_ecoreg_summary <- read_feather("data/ld_ecoreg_summary.feather") %>%
  mutate(prot_rollup = rollup_category(category))

ecoreg_nms <- c(
  TPC = "Transitional Pacific",
  HCS = "Hecate Continental Shelf",
  COG = "Coastal Gap",
  EHM = "Eastern Hazelton Mountains",
  OPS = "Outer Pacific Shelf",
  NRM = "Northern Canadian Rocky Mountains",
  OKH = "Okanogan Highland",
  CMI = "Chugach Mountains and Icefields",
  STE = "St Elias Mountains",
  BOU = "Boundary Ranges",
  YSL = "Yukon Southern Lakes",
  NUP = "Northern Alberta Upland",
  PEM = "Pelly Mountains",
  LIB = "Liard Basin",
  HHI = "Hyland Highland",
  HSL = "Hay-Slave Lowland",
  BMP = "Boreal Mountains and Plateaus",
  MPL = "Muskwa Plateau",
  CAU = "Central Alberta Upland",
  CRM = "Central Canadian Rocky Mountains",
  PRB = "Peace River Basin",
  OMM = "Omineca Mountains",
  SKM = "Skeena Mountains",
  FAB = "Fraser Basin",
  NRA = "Nass Ranges",
  SBC = "Sub-Arctic Pacific",
  FAP = "Fraser Plateau",
  WRA = "Western Continental Ranges",
  GWH = "Gwaii Haanas",
  CHR = "Chilcotin Ranges",
  SRT = "Southern Rocky Mountain Trench",
  IPS = "Inner Pacific Shelf",
  ITR = "Interior Transition Ranges",
  PAC = "Pacific Ranges",
  TOP = "Thompson-Okanagan Plateau",
  NCM = "Northern Columbia Mountains",
  NCR = "Northern Cascade Ranges",
  EVI = "Eastern Vancouver Island",
  PTR = "Purcell Transitional Ranges",
  NCD = "Northern Continental Divide",
  LOM = "Lower Mainland",
  WVI = "Western Vancouver Island",
  GPB = "Georgia-Puget Basin",
  YSH = "Yukon-Stikine Highlands",
  COH = "Columbia Highlands",
  SBF = "Selkirk-Bitterroot Foothills",
  SAU = "Southern Alberta Upland",
  ECR = "Eastern Continental Ranges"
)
ecoreg_ids <- names(ecoreg_nms)

# ecoregion_centroids <- as.data.frame(coordinates(ecoregions))
# names(ecoregion_centroids) <- c("long", "lat")
# rownames(ecoregion_centroids) <- ecoreg_ids

# BEC Data
bec_zones <- readRDS("data/bec_leaflet.rds") # SpatialPolygonsDataFrame
gg_ld_x_bec <- read_feather("data/gg_ld_bec.feather")

gg_bec <- read_feather("data/gg_bec.feather")
ld_bec_summary <- read_feather("data/ld_bec_summary.feather") %>%
  mutate(prot_rollup = rollup_category(category))

bec_zone_summary <- summarize_bec(ld_bec_summary, by_zone = TRUE) %>%
  mutate(prot_rollup = rollup_category(Category))

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
bec_ids <- sort(names(bec_nms))

bec_colors <- c(BAFA = "#E5D8B1", SWB = "#A3D1AB", BWBS = "#ABE7FF",
                ESSF = "#9E33D3", CMA = "#E5C7C7", SBS = "#2D8CBD",
                MH = "#A599FF", CWH = "#208500", ICH = "#85A303",
                IMA = "#B2B2B2", SBPS = "#36DEFC", MS = "#FF46A3",
                IDF = "#FFCF00", BG = "#FF0000", PP = "#DE7D00",
                CDF = "#FFFF00")[bec_ids] # index by bec_ids to put in order

des_cols <- c("01_PPA"                    = "#00441b",
              "02_Protected_Other"        = "#006d2c",
              "03_Exclude_1_2_Activities" = "#a6d96a",
              "04_Managed"                = "#fdbf6f")

des_labels = c("01_PPA"                    = "Parks & Protected Areas",
               "02_Protected_Other"        = "Other Protected Lands",
               "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
               "04_Managed"                = "Managed Areas")

prot_rollup_labels <- c("Prot"                      = "Protected Lands",
                        "03_Exclude_1_2_Activities" = "Resource Exclusion Areas",
                        "04_Managed"                = "Managed Areas")
