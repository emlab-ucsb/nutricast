### --------------------------------------
### --------------------------------------
### This script defines the user-interface
### --------------------------------------
### --------------------------------------

### ----------------------------------
### Initialize app -------------------
### ----------------------------------

rm(list = ls())
set.seed(123)

# Shiny functions
library(shiny) # interactive applications
library(shinyjs) # javascript functionality for shiny
library(shinydashboard) # layout
library(shinyWidgets)
library(rsconnect) # needed to deploy app to shinyapps.io

# Plotting
library(leaflet) # interactive maps 
library(plotly) # interactive charts
library(viridis) # colorblind color scales
library(RColorBrewer) # other color scales
library(cowplot)
library(ggradar)
library(raster)
library(rgeos)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Other functions
#library(lubridate)
library(countrycode) # fix and standardize country codes
library(forcats)
library(tidytext)
library(googledrive)
library(gargle)
library(tidyverse)

# Source file that loads/wrangles data and text
source("00_initialize_app.R")

# The content for each tab is stored in a separate file. Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
    pattern = "^ui_.*\\.R$",
    path = ".",
    full.names = TRUE
),
source)

# Functions needed for tool. Source all .R files in ./scripts/: 
sapply(list.files(
    pattern = "[.]R$",
    path = "./scripts/",
    full.names = TRUE
),
source)

### -----------------------------------
### User Interface (UI) ---------------
### -----------------------------------

# Define UI structure for application 
shinyUI(
    
    dashboardPage(skin = "black",
        
        ### Header ---------------------
        dashboardHeader(
            
            # Title
            title = "",
            titleWidth = 0,
            
            # Title
            tags$li(
              id = "custom-header-container",
              class = "dropdown",
              tags$h3(text$item_label[text$item_id == "app-title"],
                      style = "margin-top:10px;"
              ) # /h3
            ), # /tags$li
            
            # Harvard logo
            tags$li(
                class = "dropdown",
                a(href = 'https://www.hsph.harvard.edu/',
                  img(src = 'harvard_logo.jpg', title = text$item_label[text$item_id == "harvard_logo"], height = "30px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ), # /tags$li
            
            # EDF logo
            tags$li(
                class = "dropdown",
                a(href = 'https://www.edf.org/',
                  img(src = 'edf_logo.jpg', title = text$item_label[text$item_id == "edf_logo"], height = "30px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ), # /tags$li
            
            # emLab logo
            tags$li(
                class = "dropdown",
                a(href = 'http://emlab.msi.ucsb.edu/',
                  img(src = 'emlab_logo_horizontal.png', title = text$item_label[text$item_id == "emlab_logo"], height = "30px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ) # /tags$li
            
        ), # /dashboardHeader
        
        ### Sidebar menu ----------------------
        dashboardSidebar(
            
            # Width of sidebar menu
            width = "350px",
            
            # Want it collapsed by default
            collapsed = F,
            
            # Want it disabled by default
            #disable = T,
            
            # Menu container
            sidebarMenu(
                
                # Variable name for selected menuItem
                id = "menu-items",
                
                ### Overview ---
                menuItem(text$item_label[text$item_id == "overview"],
                         icon = NULL,
                         expandedName = "overview",
                         
                         # Blank placeholder
                         menuSubItem("",
                                     tabName = "overview",
                                     icon = NULL),
                         
                         # Introduction
                         menuSubItem(actionLink("al_introduction", text$item_label[text$item_id == "introduction"]),
                                     tabName = "overview_01",
                                     icon = NULL),
                         
                         # Marine Seafood Production Scenarios
                         menuSubItem(actionLink("al_marine_seafood_production_scenarios", text$item_label[text$item_id == "marine_seafood_production_scenarios"]),
                                  tabName = "overview_02",
                                  icon = NULL),
                         
                         # References & Contact
                         menuSubItem(actionLink("al_references_contact", text$item_label[text$item_id == "references_contact"]),
                                  tabName = "overview_03",
                                  icon = NULL)
                         
                ),
                
                ### Global & National Outlook ---
                menuItem(text$item_label[text$item_id == "global-national-outlook"],
                         icon = NULL,
                         expandedName = "global-national-outlook",
                         
                         # Blank placeholder
                         menuSubItem("",
                                     tabName = "global-national-outlook",
                                     icon = NULL),
                         
                         # Population Growth
                         menuSubItem(actionLink("al_population_growth", text$item_label[text$item_id == "population_growth"]),
                                     tabName = "global-national-outlook-01",
                                     icon = NULL),
                         
                         # Nutrient Demand
                         menuSubItem(actionLink("al_nutrient_demand", text$item_label[text$item_id == "nutrient_demand"]),
                                     tabName = "global-national-outlook-02",
                                     icon = NULL),
                         
                         # Marine Seafood as a Source of Nutrients
                         menuSubItem(actionLink("al_marine_seafood_as_source_nutrients", text$item_label[text$item_id == "marine_seafood_as_source_nutrients"]),
                                     tabName = "global-national-outlook-03",
                                     icon = NULL),
                         
                         # Future Seafood Supply & Nutrient Contributions
                         menuSubItem(actionLink("al_future_seafood_supply", text$item_label[text$item_id == "future_seafood_supply"]),
                                     tabName = "global-national-outlook-04",
                                     icon = NULL)
                         
                ),
                
                ### National Nutrition Data ---
                menuItem(text$item_label[text$item_id == "national-nutrition-data"],
                         icon = NULL,
                         expandedName = "national-nutrition-data",
                         
                         # Blank placeholder
                         menuSubItem("",
                                     tabName = "national-nutrition-data",
                                     icon = NULL),
                         
                         # Protein Intake
                         menuSubItem(actionLink("al_protein_intake", text$item_label[text$item_id == "protein_intake"]),
                                     tabName = "national-nutrition-data-01",
                                     icon = NULL),
                         
                         # Seafood Consumption & Nutrient Contributions
                         menuSubItem(actionLink("al_seafood_consumption", text$item_label[text$item_id == "seafood_consumption"]),
                                     tabName = "national-nutrition-data-02",
                                     icon = NULL),
                         
                         # Nutritional Health
                         menuSubItem(actionLink("al_nutritional_health", text$item_label[text$item_id == "nutritional_health"]),
                                     tabName = "national-nutrition-data-03",
                                     icon = NULL),
                         
                         # Nutrient Consumption Profiles
                         menuSubItem(actionLink("al_nutrient_consumption_profiles", text$item_label[text$item_id == "nutrient_consumption_profiles"]),
                                     tabName = "national-nutrition-data-04",
                                     icon = NULL)
                         
                ),
                
                ### Nutrition-Sensitive Seafood Reforms ---
                menuItem(text$item_label[text$item_id == "seafood-reforms"],
                         icon = NULL,
                         expandedName = "seafood-reforms",
                         
                         # Blank placeholder
                         menuSubItem("",
                                     tabName = "seafood-reforms",
                                     icon = NULL),
                         
                         # Seafood Nutrition Content
                         menuSubItem(actionLink("al_seafood_nutrition_content", text$item_label[text$item_id == "seafood_nutrition_content"]),
                                     tabName = "seafood-reforms-01",
                                     icon = NULL),
                         
                         # Fisheries Reforms
                         menuSubItem(actionLink("al_fisheries_reforms", text$item_label[text$item_id == "fisheries_reforms"]),
                                     tabName = "seafood-reforms-02",
                                     icon = NULL),
                         
                         # Nutrient Consumption Profiles
                         menuSubItem(actionLink("al_aquaculture_reforms", text$item_label[text$item_id == "aquaculture_reforms"]),
                                     tabName = "seafood-reforms-03",
                                     icon = NULL),
                         
                         # Nutrient Consumption Profiles
                         menuSubItem(actionLink("al_mariculture_site_explorer", text$item_label[text$item_id == "mariculture_site_explorer"]),
                                     tabName = "seafood-reforms-04",
                                     icon = NULL)
                         
                )

            ) # /sidebarMenu
        ), #/dashboardSidebar
        
        ### Main panel content ----------------------
        dashboardBody(
          
          # Custom stylesheet
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "NutriCast.css")),
          
          # Custom javascript
          tags$script(src = "NutriCast.js"),
          
            # Tabs
            tabItems(
                
                ### Overview --- 
                tabItem(tabName = "overview",
                        
                        Overview()
                        
                ),
                
                ### Global & National Outlook --- 
                tabItem(tabName = "global-national-outlook",
                        
                        GlobalNationalOutlook(country_choices = widget_country_choices,
                                              nutrient_choices = nutrient_choices)
                        
                ),
                
                ### National Nutrition Data ---
                tabItem(tabName = "national-nutrition-data",
                        
                        NationalNutritionData(country_choices = widget_country_choices,
                                              nutrient_choices = widget_nutrient_choices_national_nutrition_data)
                        
                ),
                
                ### Nutrition-Sensitive Seafood Reforms ---
                tabItem(tabName = "seafood-reforms",
                        
                        SeafoodReforms(country_choices = widget_country_choices,
                                       climate_scenario_choices = climate_scenario_choices,
                                       species_choices = species_choices)
                        
                )

            ) # /tabItems
            
        ) # /dashboardBody
        
    ) # /dashboardPage
) # /shinyUI
        


