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
library(shinymaterial) # extra features
library(rsconnect) # needed to deploy app to shinyapps.io

# Plotting
library(leaflet) # interactive maps 
library(plotly) # interactive charts
library(viridis) # colorblind color scales
library(RColorBrewer) # other color scales

# Other functions
library(lubridate)
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
            title = text$item_label[text$item_id == "title"],
            
            # SFG logo
            tags$li(
                class = "dropdown",
                a(href = 'http://sfg.msi.ucsb.edu/',
                  img(src = 'sfg_logo_b.png', title = text$item_label[text$item_id == "sfg_logo"], height = "30px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ), # /tags$li
            
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
                
                ### Inventory ---
                menuItem(text$item_label[text$item_id == "tab-1"], 
                         tabName = "tab-1", 
                         icon = NULL,
                         selected = TRUE),
                
                ### Spawns ---
                menuItem(text$item_label[text$item_id == "tab-2"], 
                         tabName = "tab-2", 
                         icon = icon(text$item_icon[text$item_id == "tab-2"]),
                         selected = FALSE),
                
                ### About methods and process ---
                menuItem(text$item_label[text$item_id == "tab-3"],
                         tabName = "tab-3",
                         icon = icon(text$item_icon[text$item_id == "tab-3"]),
                         selected = FALSE)
                
            ) # /sidebarMenu
        ), #/dashboardSidebar
        
        ### Main panel content ----------------------
        dashboardBody(
            
            # Tabs
            tabItems(
                
                ### Introduction ---
                tabItem(tabName = "tab-1",
                        Tab1()
                ),
                
                ### Introduction ---
                tabItem(tabName = "tab-2",
                        Tab2()
                ),
                
                ### About methods and process ---
                tabItem(tabName = "tab-3",
                        Tab3()
                )
                
            ) # /tabItems
            
        ) # /dashboardBody
        
    ) # /dashboardPage
) # /shinyUI
        
        


