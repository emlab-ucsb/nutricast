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
        plot.background = element_rect(fill = "#e6eef6", color = "#e6eef6"),
        legend.background = element_rect(fill = "#e6eef6", color = "#e6eef6"),
        legend.title = element_text(size = 12, face = "bold"))
        #plot.background = element_rect(color = "#1a2d3f"))

map_theme <- theme_linedraw()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        plot.background = element_rect(fill = "#e6eef6", color = "#e6eef6"),
        legend.background = element_rect(fill = "#e6eef6", color = "#e6eef6"),
        legend.title = element_text(size = 12, face = "bold"),
        panel.background = element_rect(color = "#e6eef6"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

world <- ne_countries(scale = 'small', 
                      type = 'map_units',
                      returnclass = 'sf')

world_points <- world %>%
  st_zm() %>%
  st_point_on_surface()

### ----------------------------------
### App data -------------------------
### ----------------------------------

### Global and National Outlook -------------------

# 1) Population growth
national_pop_dat <- readRDS("./data/WB_UN_1960_2100_human_population_by_country.Rds")

global_pop_dat <- readRDS("./data/WB_UN_1960_2100_human_population_global.Rds") %>%
  mutate(country = "Global",
         iso3 = "Global")

pop_dat <- national_pop_dat %>%
  bind_rows(global_pop_dat)

# 2a) Nutrient Deficiencies
national_nutrient_deficiency_dat <- readRDS("./data/nutr_deficiencies_by_cntry_sex_age_2011.Rds") %>%
  ungroup() %>%
  dplyr::select(iso3, country, age, sex, nutrient, ndeficient, nhealthy)

global_nutrient_deficiency_dat <- readRDS("./data/nutr_deficiencies_by_sex_age_2011.Rds") %>%
  ungroup() %>%
  mutate(iso3 = "Global", country = "Global") %>%
  dplyr::select(iso3, country, age, sex, nutrient, ndeficient, nhealthy) 

# Combine data for plot and format
nutrient_deficiency_dat <- national_nutrient_deficiency_dat %>%
  bind_rows(global_nutrient_deficiency_dat) %>%
  filter(sex != "Children") %>% # Remove children (not symmetric)
  gather(key="type", value="npeople", 6:7) %>%  # Gather
  mutate(type=recode_factor(type,
                            "ndeficient"="Deficient",
                            "nhealthy"="Healthy"),
         sex=factor(sex, levels=c("Women", "Men"))) %>% 
  group_by(iso3, country, nutrient) %>%
  mutate(ntotal = sum(npeople, na.rm = T)) %>%
  ungroup() %>%
  mutate(ppeople = (npeople/ntotal)*100) %>%
  mutate(npeople=ifelse(sex=="Men", npeople*-1, npeople),   # Make male values negative for plotting
         ppeople=ifelse(sex=="Men", ppeople*-1, ppeople)) %>%
  mutate(npeople = npeople/1e6)

# 2b) Nutrient Demand
national_nutrient_demand_dat <- readRDS("./data/1960_2100_nutrient_demand_by_country.Rds")
global_nutrient_demand_dat <- readRDS("./data/1960_2100_nutrient_demand_global.Rds") %>%
  mutate(country = "Global",
         iso3 = "Global")

nutrient_demand_dat <- national_nutrient_demand_dat %>%
  bind_rows(global_nutrient_demand_dat)

nutrient_choices <- unique(nutrient_demand_dat$nutrient)

# 3a) Capture and aquaculture production

# Historical capture production by country (FAO)
national_capture_production_dat <- readRDS("./data/1950_2017_fao_landings_by_country_isscaap.Rds") %>%
  dplyr::filter(!(isscaap %in% c("Freshwater molluscs", "Miscellaneous freshwater fishes", "Freshwater crustaceans", "River eels"))) %>% # remove freshwater production
  rename(country = country_use, iso3 = iso3_use) %>%
  mutate(plot_group = case_when(major_group == "Pisces" ~ "Finfish",
                                isscaap %in% c("Clams, cockles, arkshells", "Mussels", "Scallops, pectens", "Oysters") ~ "Bivalves",
                                TRUE ~ "Other")) %>%
  group_by(country, iso3, year, plot_group, prod_type) %>%
  summarize(prod_mt = sum(prod_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(source = "Marine Capture")

global_capture_production_dat <- national_capture_production_dat %>%
  group_by(year, plot_group, prod_type, source) %>%
  summarize(prod_mt = sum(prod_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(country = "Global", iso3 = "Global")

# Historical aquaculture production by country (FAO)
national_aquaculture_production_dat <- readRDS("./data/1950_2017_fao_aquaculture_data.Rds") %>%
  dplyr::filter(environment != "Freshwater") %>% # remove freshwater production
  mutate(plot_group = case_when(major_group == "Pisces" ~ "Finfish",
                                order == "Bivalvia" ~ "Bivalves",
                                TRUE ~ "Other")) %>%
  group_by(country_orig, iso3_orig, year, plot_group) %>%
  summarize(prod_mt = sum(quantity_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(source = "Aquaculture") %>%
  mutate(prod_type = "Landings") %>%
  rename(country = country_orig, iso3 = iso3_orig)
 
global_aquaculture_production_dat <- national_aquaculture_production_dat %>%
  group_by(year, plot_group, prod_type, source) %>%
  summarize(prod_mt = sum(prod_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(country = "Global", iso3 = "Global")

# Output plot dat
production_plot_dat <- national_capture_production_dat %>%
  bind_rows(global_capture_production_dat) %>%
  bind_rows(national_aquaculture_production_dat) %>%
  bind_rows(global_aquaculture_production_dat)

# 3b/c) Protein and Nutrients from Seafood
national_nutrient_from_seafood_dat <- readRDS("./data/genus_pnutrient_seafood_by_cntry_2011.Rds")
national_diet_from_seafood_dat <- readRDS("./data/genus_pdiet_seafood_by_cntry_year.Rds")
  
protein_dat <- national_nutrient_from_seafood_dat %>% 
  filter(nutrient=="Protein") %>% 
  mutate(prop_seafood_cap = pmin(prop_seafood, 0.1))

# Get selectable countries for which we have nutrition data
country_choices <- national_nutrient_from_seafood_dat %>%
  ungroup() %>%
  distinct(country, iso3) 

widget_country_choices <- unique(country_choices$iso3)
names(widget_country_choices) <- unique(country_choices$country)

### National Nutrition Data -------------------

# 3) Nutritional health
national_nutritional_health_dat <- readRDS("./data/genus_nutrient_supplies_by_age_sex_2011_w_us_diet_req.Rds")

nutritional_health_plot_dat <- national_nutritional_health_dat %>%
  mutate(sex=recode_factor(sex, "Children"="Children", "Females"="Women", "Males"="Men"),
         nutrient_type=recode(nutrient_type, 
                              "Macronutrient"="Macro\nnutrient"))

# 4) Nutrient consumption profiles
national_nutrient_supplies_dat <- readRDS("./data/genus_nutrient_supplies_by_cntry_year.Rds")

widget_nutrient_choices_national_nutrition_data <- unique(national_nutrient_supplies_dat$nutrient)

# DRIs data
dri_dat <- readRDS("./data/DRIs_matched_to_genus_age_sex_groups.Rds") %>% 
  filter(nutrient!="Protein")

### Seafood Reforms Data -------------------

# 1) Seafood nutrition content
load("./data/vaitla_etal_2018_finfish_nutrient_data.Rdata")

# 2) MISSING FISHERY REFORMS STUFF

# 3) Mariculture reforms

#Future bivalve/finfish aquaculture production projections by country (Chris Free)
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

# Load climate projection data
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
climate_scenario_choices <- unique(rcp_projections$scenario)

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

# ### 3) Historical aquaculture production by country (FAO)
# production_current_dat <- readRDS("./data/1950_2017_fao_aquaculture_data.Rds") %>%
#   arrange(country_orig) %>%
#   dplyr::filter(environment != "Freshwater") %>% # remove freshwater to be consistant above
#   mutate(is_finfish = ifelse(major_group == "Pisces", T, F),
#          is_bivalve = ifelse(order == "Bivalvia", T, F)) %>%
#   left_join(ter_sov_lookup, by = c("iso3_orig" = "ter1_iso")) %>%
#   dplyr::filter(sov1_name %in% country_choices)

# # Summarize production dat by species category
# production_current_group_dat <- production_current_dat %>%
#   group_by(sov1_name, sov1_iso, isscaap, year) %>%
#   summarize(quantity_mt = sum(quantity_mt, na.rm = T)) 
# 
# # Summarize production dat for bivalves
# production_current_sovereign_dat <- production_current_dat %>%
#   dplyr::filter(is_bivalve | is_finfish) %>%
#   mutate(group = ifelse(is_bivalve, "Bivalves", "Finfish")) %>%
#   group_by(sov1_name, sov1_iso, year, group) %>%
#   summarize(quantity_mt = sum(quantity_mt, na.rm = T),
#             profits_usd = sum(value_usd_t, na.rm = T)*1000) %>%
#   mutate(rcp = "Historical",
#          feed_scen = NA,
#          dev_pattern = "Historical")
# 
# # Add them together
# production_all_sovereign_dat <- production_current_sovereign_dat %>%
#   bind_rows(production_future_sovereign_dat) %>%
#   arrange(sov1_name, year)

# ### 5) Nutrition information
# load("./data/Vaitla_etal_2018_nutrient_data.Rdata")
# 
# nutrient_dat_max <- nutrient_preds_long %>% 
#   group_by(nutrient) %>%
#   summarize(value_md_max = max(value_md, na.rm = T))
#   
# nutrient_dat_pmax <- nutrient_preds_long %>%
#   left_join(nutrient_dat_max, by = "nutrient") %>%
#   mutate(pmax_fill = (value_md/value_md_max)) %>%
#   dplyr::filter(!is.na(pmax_fill)) %>%
#   dplyr::select(species, nutrient, pmax_fill) %>% 
#   mutate(nutrient=recode(nutrient, 
#                          "Omega-3 fatty acids"="Omega-3\nfatty acids",
#                          "Omega-6 fatty acids"="Omega-6\nfatty acids")) %>% 
#   group_by(species, nutrient) %>%
#   summarize(pmax_fill = median(pmax_fill)) %>%
#   ungroup() %>%
#   spread(key="nutrient", value="pmax_fill")
