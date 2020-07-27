### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the aqua-explorer tab
### ----------------------------------------------
### ----------------------------------------------

AquaExplorer <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    # Introduction
    column(12,
           
           # Header
           h3(text$item_label[text$item_id == "aqua-explorer"]),
           
           # Introductory text
           includeHTML("./text/aqua_explorer_intro.html")
           
    ),
    
    # Select country
    column(4,
           
           # Country Widget
           selectizeInput("aqua_explorer_select_species",
                          label = text$item_label[text$item_id == "aqua_explorer_select_species"],
                          choices = species_choices,
                          width = "100%")
           
    ),
    
    column(4,
           
           # Climate scenario widget
           selectizeInput("aqua_explorer_select_scenario",
                          label = text$item_label[text$item_id == "aqua_explorer_select_scenario"],
                          choices = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),
                          selected = "RCP 2.6",
                          width = "100%")
           
    ),
    
    column(4,
           
           # Climate scenario widget
           selectizeInput("aqua_explorer_select_year",
                          label = text$item_label[text$item_id == "aqua_explorer_select_year"],
                          choices = c(2021, 2051, 2100),
                          selected = 2021,
                          width = "100%")
           
    ),
    
    # Future production by species ranked
    column(12,
           
           # # Header
           # tags$h4(text$item_label[text$item_id == "aqua-species-ranked"]),
           # 
           # # Intro
           # includeHTML("./text/aqua_species_bar_plot.html"),
           
           # Plot
           plotOutput("future_species_production_map")
           
    )
    
  )
  
}