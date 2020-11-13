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

text_size <- 14

plot_theme <- theme_linedraw()+
  theme(axis.text = element_text(size = text_size),
        axis.title = element_text(size = text_size, face = "bold"),
        plot.background = element_rect(fill = "#ffffff", color = "#1a2d3f", size = 2),
        legend.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.title = element_text(size = text_size, face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.text = element_text(size = text_size, face = "bold"))

plot_theme_tab <- theme_linedraw()+
  theme(axis.text = element_text(size = text_size),
        axis.title = element_text(size = text_size, face = "bold"),
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.title = element_text(size = text_size, face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.text = element_text(size = text_size, face = "bold"))

map_theme <- theme_linedraw()+
  theme(axis.text = element_text(size = text_size),
        axis.title = element_text(size = text_size, face = "bold"),
        plot.background = element_rect(fill = "#ffffff", color = "#1a2d3f", size = 2),
        legend.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.title = element_text(size = text_size, face = "bold"),
        panel.background = element_rect(color = "#ffffff"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

map_theme_tab <- theme_linedraw()+
  theme(axis.text = element_text(size = text_size),
        axis.title = element_text(size = text_size, face = "bold"),
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.title = element_text(size = text_size, face = "bold"),
        panel.background = element_rect(color = "#ffffff"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

world <- ne_countries(scale = 'small', 
                      type = 'map_units',
                      returnclass = 'sf')

world_points <- world %>%
  st_zm() %>%
  st_point_on_surface()

### ----------------------------------
### App data -------------------------
### ----------------------------------

### Tab 1 - Global and National Outlook -------------------

# Plot 1) Population growth
population_growth_plot_data <- readRDS("./data/processed/01_01_population_growth_plot_data.Rds")

# Plot 2a) Nutrient Demand - Nutrient Deficiencies
nutrient_demand_plot_1_data <- readRDS("./data/processed/01_02a_nutrient_demand_plot_data.Rds")
  
# Plot 2b) Nutrient Demand - Projected Nutrient Demands
nutrient_demand_plot_2_data <- readRDS("./data/processed/01_02b_nutrient_demand_plot_data.Rds") %>%
  arrange(nutrient)

nutrient_choices <- unique(nutrient_demand_plot_2_data$nutrient)

# Plot 3a) Marine Seafood as a Source of Nutrients - Fisheries and Mariculture Production
fisheries_mariculture_production_plot_data <- readRDS("./data/processed/01_03a_fisheries_mariculture_production_plot_data.Rds")

# Plot 3b) Marine Seafood as a Source of Nutrients - Protein from Seafood
protein_from_seafood_plot_data <- readRDS("./data/processed/01_03b_protein_from_seafood_plot_data.Rds")

# Plot 3c) Marine Seafood as a Source of Nutrients - Seafood Consumption
seafood_consumption_plot_data_diet <- readRDS("./data/processed/01_03c_seafood_consumption_plot_data_diet.Rds")
seafood_consumption_plot_data_nutrients <- readRDS("./data/processed/01_03c_seafood_consumption_plot_data_nutrients.Rds")

# country_choices <- protein_from_seafood_plot_data %>%
#   ungroup() %>%
#   distinct(country, iso3) 
# 
# widget_country_choices <- unique(country_choices$iso3)
# names(widget_country_choices) <- unique(country_choices$country)

# Plot 4a) Future Seafood Supply and Nutrient Contributions - Forecasted Seafood Supply
forecasted_seafood_supply_plot_data_historical <- readRDS("./data/processed/01_04a_forecasted_seafood_supply_data_historical.Rds") %>%
  mutate(sector = fct_relevel(sector, "Bivalve mariculture", "Finfish mariculture", "Capture fisheries"))
forecasted_seafood_supply_plot_data_projections <- readRDS("./data/processed/01_04a_forecasted_seafood_supply_data_projections.Rds") %>%
  mutate(sector = fct_relevel(sector, "Bivalve mariculture", "Finfish mariculture", "Capture fisheries"))

### DEAL WITH COUNTRY CHOICES ----------
# Get selectable countries for which we have projection data - may need to refine this list further 
country_choices_forecast <- forecasted_seafood_supply_plot_data_projections %>%
  ungroup() %>%
  dplyr::filter(iso3 != "Global") %>%
  distinct(country, iso3) %>%
  arrange(country) %>%
  dplyr::filter(!(iso3 %in% c("CHN/HKG/MAC", "MNP/GUM"))) %>%
  dplyr::filter(!(country %in% c("Antigua and Barbuda", "Falkland / Malvinas Islands", "Line Group", "Micronesia (Federated States of)", "Myanmar (Burma)", "Other nei", "Sao Tome and Principe", "Turks and Caicos Islands", "Ivory Coast", "Galapagos", "Canary Islands", "Faeroe", "Gilbert Islands", "Saint Kitts and Nevis", "Saint Lucia", "Republic of Mauritius", "Madeira", "Federal Republic of Somalia", "East Timor")))

country_choices_nutrition <- protein_from_seafood_plot_data %>%
  ungroup() %>%
  distinct(country, iso3)

country_choices_out <- country_choices_forecast %>%
  inner_join(country_choices_nutrition %>% dplyr::select(iso3), by = "iso3")
  
widget_country_choices <- unique(country_choices_out$iso3)
names(widget_country_choices) <- unique(country_choices_out$country)

# PLOT 4b) NEED 

### National Nutrition Data -------------------

# Plots 1a/b) Same as those from Plot 3b in the previous section

# Plots 2a-c) Same as those from Plot 3c in the previous section

# Plot 3) Nutritional Health
nutritional_health_plot_dat <- readRDS("./data/processed/02_03_nutritional_health_plot_data.Rds") %>%
  mutate(sex=recode_factor(sex, "Children"="Children", "Females"="Women", "Males"="Men"),
         nutrient_type=recode(nutrient_type,
                              "Macronutrient"="Macro\nnutrient")) %>%
  dplyr::filter(nutrient != "Sodium")

# Plot 4) Nutrient Consumption Profiles
nutrient_consumption_plot_data_country_year <- readRDS("./data/processed/02_04_nutrient_consumption_plot_data_country_year.Rds") %>%
  arrange(nutrient)

# Widget choices nutrients
widget_nutrient_choices_national_nutrition_data <- unique(nutrient_consumption_plot_data_country_year$nutrient)

nutrient_consumption_plot_data_age_sex <- readRDS("./data/processed/02_04_nutrient_consumption_plot_data_age_sex.Rds")

# DRIs data
dri_dat <- readRDS("./data/DRIs_matched_to_genus_age_sex_groups.Rds") %>% 
  filter(nutrient != "Protein")

### Seafood Reforms Data -------------------

# 1) Seafood nutrition content
load("./data/vaitla_etal_2018_finfish_nutrient_data.Rdata")

# 2a) Fishery reforms - nutrient content
fish_nutrition_content_dat <- read.csv("./data/GENUS_nutrient_per_100g_by_seafood_group.csv", as.is = T)

# Nutrients of interest
nutrients_keep <- c("calcium_mg", "copper_mg", "folate_mcg", "iron_mg", "magnesium_mg", "niacin_mg", "phosphorus_mg",
             "riboflavin_mg", "thiamin_mg", "vitamin_a_mcg_rae", "vitamin_b6_mg", "vitamin_c_mg", "zinc_mg")

# Reformat for plotting
fish_nutrition_content_plot_dat <- fish_nutrition_content_dat %>% 
  gather(key="nutrient", value="quantity", 2:ncol(.)) %>% 
  group_by(nutrient) %>% 
  mutate(quantity_prop=quantity/max(quantity)) %>%  # Calculate as percent of maximum
  ungroup() %>% 
  mutate(species_group=recode(genus_food_name,  # Recode group names
                              "Demersal Fish"="Fish, demersal",
                              "Pelagic Fish"="Fish, pelagic",
                              "Marine Fish; Other"="Fish, other",
                              "Molluscs; Other"="Bivalves and gastropods")) %>% 
  filter(nutrient %in% nutrients_keep) %>%  # Reduce to nutrients of interest
  mutate(nutrient=recode(nutrient, 
                         "calcium_mg"="Calcium", 
                         "copper_mg"="Copper", 
                         "folate_mcg"="Folate", 
                         "iron_mg"="Iron", 
                         "magnesium_mg"="Magnesium", 
                         "niacin_mg"="Niacin", 
                         "phosphorus_mg"="Phosphorus",
                         "riboflavin_mg"="Riboflavin", 
                         "thiamin_mg"="Thiamin", 
                         "vitamin_a_mcg_rae"="Vit A", 
                         "vitamin_b6_mg"="Vit B6", 
                         "vitamin_c_mg"="Vit C", 
                         "zinc_mg"="Zinc")) %>% 
  dplyr::select(species_group, nutrient, quantity_prop) %>%   # Reduce for plotting
  spread(key="nutrient", value="quantity_prop")

# 2b) Fishery Reforms - edible meat production plot
edible_meat_production_data <- readRDS("./data/Free_etal_2020_capture_meat_proj_by_rcp_mgmt_genus_group.Rds") %>%
  ungroup() %>% 
  mutate(scenario=recode_factor(scenario, 
                                "No Adaptation"="BAU",
                                "Full Adaptation"="Reforms"),
         genus=recode(genus,
                      "Demersal Fish"="Fish, demersal",
                      "Marine Fish; Other"="Fish, other",
                      "Molluscs; Other"="Bivalves and gastropods",
                      "Pelagic Fish"="Fish, pelagic"))

# 2c) Fishery reforms - 
nutrient_demand_edible_meat_production_data <- readRDS("./data/2050_2100_nutr_demand_prop_from_wc_rcp_mgmt_cntry_group.Rds") %>%
  dplyr::filter(scenario == "Reforms")

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
  distinct(ter1_name, ter1_iso, sov1_name, sov1_iso, eez_code) %>%
  arrange(sov1_name)

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

# Set some widget default choices
species_choices <- sort(unique(rcp_projections$species))
climate_scenario_choices <- unique(rcp_projections$scenario)
country_choices <- unique(ter_sov_lookup$sov1_iso)
names(country_choices) <- unique(ter_sov_lookup$sov1_name)

# Now for the nutrient contributions data
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

# # Plot 4) Mariculture Explorer
# mariculture_explorer_data <- readRDS("./data/processed/species_rasters/species_rasters_tibble.Rds")
