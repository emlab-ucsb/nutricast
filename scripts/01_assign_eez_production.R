### ----------------------------------------------
### ----------------------------------------------
### This script contains takes the gridded aquaculture results by species and creates a master list
### Need: All EEZ codes and species cooresponding to each sovereign state
### ----------------------------------------------
### ----------------------------------------------

### -----
### Setup
### -----

library(tidyverse)
library(raster)

results_dir <- here::here("app", "data/")


### -----------------------------------------------------------
### Section 1) Get EEZ codes cooresponding to each x/y position
### -----------------------------------------------------------
eez_raster <- raster::brick(here::here("app", "data", "eezs_v10_raster_10km.tif"))

eez_info <- raster::as.data.frame(eez_raster, xy=T) %>%
  mutate(x = round(x, 0),
         y = round(y, 0)) %>%
  rename(eez_id = eezs_v10_raster_10km)
  
### -----------------------------------------------
### Section 2) Match aquaculture production to EEZs
### -----------------------------------------------

# dat_files <- list.files(path = here::here("app", "data", "raw"),
#                         pattern = "*.Rds",
#                         full.names = T)

# Starting with RCP 2.6
rcp26_dat_files <- list.files(path = here::here("app", "data", "raw"),
                              pattern = "RCP26",
                              full.names = T)

# Make blank data frame
rcp_26_dat_all <- data.frame()

for(i in 1:length(rcp26_dat_files)){
  
  text <- str_split(str_replace(rcp26_dat_files[i], ".Rds", ""), "_")
  group <- text[[1]][2]
  species <- paste(text[[1]][3], text[[1]][4])
  
  foo <- readRDS(rcp26_dat_files[i]) %>%
    mutate(x = round(x, 0),
           y = round(y, 0)) %>%
    left_join(eez_info, by = c("x", "y")) %>%
    mutate(x_y_id = paste0(x, "-", y)) %>%
    group_by(eez_id, year, viable) %>%
    summarize(n_points = n_distinct(x_y_id),
              prod_mt_yr = sum(prod_mt_yr, na.rm = T),
              revenue_usd_yr = sum(revenue_usd_yr, na.rm = T),
              cost_usd_yr = sum(cost_usd_yr, na.rm = T),
              profits_usd_yr = sum(profits_usd_yr, na.rm = T)) %>%
    mutate(group = group,
           species = species)
  
  rcp_26_dat_all <- rcp_26_dat_all %>%
    bind_rows(foo)
  
}

rcp45_dat_files <- list.files(path = here::here("app", "data", "raw"),
                              pattern = "RCP45",
                              full.names = T)

rcp60_dat_files <- list.files(path = here::here("app", "data", "raw"),
                              pattern = "RCP60",
                              full.names = T)

rcp85_dat_files <- list.files(path = here::here("app", "data", "raw"),
                              pattern = "RCP85",
                              full.names = T)


