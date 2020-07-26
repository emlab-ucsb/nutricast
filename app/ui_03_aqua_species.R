### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the aqua-species tab
### ----------------------------------------------
### ----------------------------------------------

AquaSpecies <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    # Introduction
    column(12,
           
           # Header
           h3(text$item_label[text$item_id == "aqua-species"]),
           
           # Introductory text
           includeHTML("./text/aqua_potential_intro.html")
           
    ),
    
    # Select country
    column(6,
           
           # Country Widget
           selectizeInput("aqua_species_select_country",
                          label = text$item_label[text$item_id == "aqua_species_select_country"],
                          choices = country_choices,
                          width = "100%")
           
    ),
    
    column(6,
           
           # Climate scenario widget
           selectizeInput("aqua_species_select_scenario",
                          label = text$item_label[text$item_id == "aqua_species_select_scenario"],
                          choices = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),
                          width = "100%")
           
    ),
    
    # Future production by species ranked
    column(12,
           
           # Header
           tags$h4(text$item_label[text$item_id == "aqua-species-ranked"]),
           
           # Intro
           includeHTML("./text/aqua_potential_historical.html"),
           
           # Plot
           plotOutput("future_species_production_plot")
           
    ),
    
    # Nutrition information by species ranked
    column(12,
           
           # Header
           tags$h4(text$item_label[text$item_id == "aqua-species-nutrition"]),
           
           # Intro
           includeHTML("./text/aqua_potential_historical.html"),
           
           # Plot
           plotOutput("future_species_nutrition")
           
    )
    
  )
  
}