### ----------------------------------------------
### ----------------------------------------------
### This script loads data and text 
### ----------------------------------------------
### ----------------------------------------------

### ----------------------------------
### App text -------------------------
### ----------------------------------

text <- read_csv("./text/app_text.csv")

### ----------------------------------
### App data -------------------------
### ----------------------------------

# EEZ raster
eez_raster_10km <- raster::brick("./data/eezs_v10_raster_10km.tif")

# Aquaculture production by country (FAO)
production_current_dat <- readRDS("./data/1950_2017_fao_aquaculture_data.Rds")





