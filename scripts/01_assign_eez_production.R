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

results_dir <- here::here("app", "data/")


### -----------------------------------------------------------
### Section 1) Get EEZ codes cooresponding to each x/y position
### -----------------------------------------------------------


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

rcp_26_dat_all <- data.frame()

for(i in 1:length(rcp26_dat_files)){
  
  foo <- readRDS(rcp26_dat_files[i]) %>%
    group_by(year, )
  
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


