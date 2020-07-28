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
library(rsconnect) # needed to deploy app to shinyapps.io

# Plotting
library(leaflet) # interactive maps 
library(plotly) # interactive charts
library(viridis) # colorblind color scales
library(RColorBrewer) # other color scales
library(ggradar)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Other functions
#library(lubridate)
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
            
            # # SFG logo
            # tags$li(
            #     class = "dropdown",
            #     a(href = 'http://sfg.msi.ucsb.edu/',
            #       img(src = 'sfg_logo_b.png', title = text$item_label[text$item_id == "sfg_logo"], height = "30px"), 
            #       style = "padding-top:10px; padding-bottom:10px;"
            #     ) # /a
            # ), # /tags$li
            
            # emLab logo
            tags$li(
                class = "dropdown",
                a(href = 'http://emlab.msi.ucsb.edu/',
                  img(src = 'emlab_logo_horizontal_b.png', title = text$item_label[text$item_id == "emlab_logo"], height = "30px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ) # /tags$li
            
        ), # /dashboardHeader
        
        ### Sidebar menu ----------------------
        dashboardSidebar(
            
            # Width of sidebar menu
            width = "250px",
            
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
                         tabName = "overview", 
                         #icon = NULL,
                         selected = TRUE),
                
                ### Aquaculture potential ---
                menuItem(text$item_label[text$item_id == "aqua-potential"], 
                         tabName = "aqua-potential", 
                         #icon = icon(text$item_icon[text$item_id == "tab-2"]),
                         selected = FALSE),
                
                ### Aquaculture species ---
                menuItem(text$item_label[text$item_id == "aqua-species"],
                         tabName = "aqua-species",
                         #icon = icon(text$item_icon[text$item_id == "tab-3"]),
                         selected = FALSE),
                
                ### Aquaculture site explorer ---
                menuItem(text$item_label[text$item_id == "aqua-explorer"],
                         tabName = "aqua-explorer",
                         #icon = icon(text$item_icon[text$item_id == "tab-3"]),
                         selected = FALSE)
                
            ) # /sidebarMenu
        ), #/dashboardSidebar
        
        ### Main panel content ----------------------
        dashboardBody(
          
          # Custom stylesheet
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app_css.css")),
            
            # Tabs
            tabItems(
                
                ### Overview ---
                tabItem(tabName = "overview",
                        Overview()
                ),
                
                ### Aquaculture potential ---
                tabItem(tabName = "aqua-potential",
                        AquaPotential()
                ),
                
                ### Aquaculture species ---
                tabItem(tabName = "aqua-species",
                        AquaSpecies()
                ),
                
                ### Aquaculture site explorer ---
                tabItem(tabName = "aqua-explorer",
                        AquaExplorer()
                )
                
            ) # /tabItems
            
        ) # /dashboardBody
        
    ) # /dashboardPage
) # /shinyUI
        
        


