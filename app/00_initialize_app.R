### ----------------------------------------------
### ----------------------------------------------
### This script loads data and text 
### ----------------------------------------------
### ----------------------------------------------

### ----------------------------------
### App text -------------------------
### ----------------------------------

text <- read_csv("./text/app_text.csv") %>%
  dplyr::filter(!is.na(tab_id))

### ----------------------------------
### Plot theme -------------------------
### ----------------------------------

plot_theme <- theme_linedraw()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.background = element_rect(fill = "#e6eef6", color = "#e6eef6"))
        #plot.background = element_rect(color = "#1a2d3f"))

### ----------------------------------
### App data -------------------------
### ----------------------------------

### Global and National Outlook -------------------

# 1) Population growth
national_pop_dat <- readRDS("./data/WB_UN_1960_2100_human_population_by_country.Rds")

# Get selectable countries for which we have population data
global_national_outlook_countries <- unique(national_pop_dat$country)
global_national_outlook_countries <- sort(countries)

global_pop_dat <- readRDS("./data/WB_UN_1960_2100_human_population_global.Rds") %>%
  mutate(country = "Global",
         iso3 = "Global")

pop_dat <- national_pop_dat %>%
  bind_rows(global_pop_dat)

### 0) Let's see if we can figure out to set up an API to link to files stored on Google Drive (should speed up app hosting significantly)
# Ultimately it would probably be good to make this more secure... 
# google_app <- httr::oauth_app(
#   "google-app",
#   key = "458956698118-dif8ifchgf0ob0pij2n4gcsql6m14e75.apps.googleusercontent.com",
#   secret = "gtCUX5snOXD4otTCJpz0lEzo"
# )
# drive_auth_configure(app = google_app)
# 
# # Set data file path (in google drive)
# drive_file_path <- "~Shared"
# drive_find("RCP26")

### 1) EEZ raster
# eez_raster_10km <- raster::raster("./data/eezs_v10_raster_10km.tif")
# eez_10km_df <- raster::as.data.frame(eez_raster_10km, xy = T)

worldmap <- ne_countries(scale = 'small', 
                         type = 'map_units',
                         returnclass = 'sf')

### 2) Future bivalve/finfish aquaculture production projections by country (Chris Free)
production_future_bivalve_dat <- read.csv("./data/bivalve_mariculture_potential_by_eez_rcp.csv", stringsAsFactors = F) %>%
  mutate(group = "Bivalves",
         feed_scen = NA,
         dev_pattern = "Current development")

production_future_finfish_dat <- read.csv("./data/finfish_mariculture_potential_by_eez_rcp_feed_scenario.csv", stringsAsFactors = F) %>%
  mutate(group = "Finfish") %>%
  dplyr::select(-sov1_iso) %>%
  rename(sov1_iso = sov1_iso3_use)

# Combine
production_future_dat <- production_future_bivalve_dat %>%
  bind_rows(production_future_finfish_dat)

# Create lookup table of territory names/codes and sovereign names/codes
ter_sov_lookup <- production_future_dat %>%
  distinct(ter1_name, ter1_iso, sov1_name, sov1_iso, eez_code)

# [ADD MANUAL CORRECTIONS LATER]
# ter_sov_manual <- data.frame(
#   ter1_name = c("Aruba", "Bahrain", "Bosnia and Herzegovina", "Channel Islands", "China, Hong Kong SAR", "Eritrea", "Grenada", "Jordan", "Kuwait", "Mayotte", "Micronesia, Fed.States of", "Netherlands Antilles", "Palestine", "Qatar", "Serbia and Montenegro", "Slovenia", "Ukraine", "United Arab Emirates"),
#   ter1_iso = c("ABW", "BHR", "BIH", "CI", "HKG", "ERI", ),
#   sov1_name = c("Netherlands"),
#   sov1_iso = c("NLD"))

production_future_sovereign_dat <- production_future_dat %>%
  dplyr::filter(eez_type == "200NM") %>%
  group_by(sov1_name, sov1_iso, year, group, rcp, feed_scen, dev_pattern) %>%
  summarise(quantity_mt = sum(prod_mt, na.rm = T),
            profits_usd = sum(profits_usd, na.rm = T)) %>%
  arrange(sov1_name)

country_choices <- unique(production_future_sovereign_dat$sov1_name)

### 3) Historical aquaculture production by country (FAO)
production_current_dat <- readRDS("./data/1950_2017_fao_aquaculture_data.Rds") %>%
  arrange(country_orig) %>%
  dplyr::filter(environment != "Freshwater") %>% # remove freshwater to be consistant above
  mutate(is_finfish = ifelse(major_group == "Pisces", T, F),
         is_bivalve = ifelse(order == "Bivalvia", T, F)) %>%
  left_join(ter_sov_lookup, by = c("iso3_orig" = "ter1_iso")) %>%
  dplyr::filter(sov1_name %in% country_choices)

# Summarize production dat by species category
production_current_group_dat <- production_current_dat %>%
  group_by(sov1_name, sov1_iso, isscaap, year) %>%
  summarize(quantity_mt = sum(quantity_mt, na.rm = T)) 

# Summarize production dat for bivalves
production_current_sovereign_dat <- production_current_dat %>%
  dplyr::filter(is_bivalve | is_finfish) %>%
  mutate(group = ifelse(is_bivalve, "Bivalves", "Finfish")) %>%
  group_by(sov1_name, sov1_iso, year, group) %>%
  summarize(quantity_mt = sum(quantity_mt, na.rm = T),
            profits_usd = sum(value_usd_t, na.rm = T)*1000) %>%
  mutate(rcp = "Historical",
         feed_scen = NA,
         dev_pattern = "Historical")

# Add them together
production_all_sovereign_dat <- production_current_sovereign_dat %>%
  bind_rows(production_future_sovereign_dat) %>%
  arrange(sov1_name, year)

### 4) Future aquaculture production by EEZ
rcp_26_projections <- read_csv("./data/rcp_26_species_dat_by_eez.csv") %>%
  mutate(scenario = "RCP 2.6")

rcp_45_projections <- read_csv("./data/rcp_45_species_dat_by_eez.csv") %>%
  mutate(scenario = "RCP 4.5")

rcp_60_projections <- read_csv("./data/rcp_60_species_dat_by_eez.csv") %>%
  mutate(scenario = "RCP 6.0")

rcp_85_projections <- read_csv("./data/rcp_85_species_dat_by_eez.csv") %>%
  mutate(scenario = "RCP 8.5")

rcp_projections <- rcp_26_projections %>%
  bind_rows(rcp_45_projections) %>%
  bind_rows(rcp_60_projections) %>%
  bind_rows(rcp_85_projections) %>%
  left_join(ter_sov_lookup, by = c("eez_id" = "eez_code"))

species_choices <- sort(unique(rcp_projections$species))

### 5) Nutrition information
load("./data/Vaitla_etal_2018_nutrient_data.Rdata")

nutrient_dat_max <- nutrient_preds_long %>% 
  group_by(nutrient) %>%
  summarize(value_md_max = max(value_md, na.rm = T))
  
nutrient_dat_pmax <- nutrient_preds_long %>%
  left_join(nutrient_dat_max, by = "nutrient") %>%
  mutate(pmax_fill = (value_md/value_md_max)) %>%
  dplyr::filter(!is.na(pmax_fill)) %>%
  dplyr::select(species, nutrient, pmax_fill) %>% 
  mutate(nutrient=recode(nutrient, 
                         "Omega-3 fatty acids"="Omega-3\nfatty acids",
                         "Omega-6 fatty acids"="Omega-6\nfatty acids")) %>% 
  group_by(species, nutrient) %>%
  summarize(pmax_fill = median(pmax_fill)) %>%
  ungroup() %>%
  spread(key="nutrient", value="pmax_fill")
