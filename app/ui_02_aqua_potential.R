### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the aqua-potential tab
### ----------------------------------------------
### ----------------------------------------------

AquaPotential <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    # Introduction
    column(12,
           
           # Header
           h3(text$item_label[text$item_id == "aqua-potential"]),
           
           # Introductory text
           includeHTML("./text/aqua_potential_intro.html")
           
    ),
    
    # Select country
    column(12,
           
           # Widget
           selectizeInput("aqua_potential_select_country",
                          label = text$item_label[text$item_id == "aqua_potential_select_country"],
                          choices = country_choices,
                          width = "50%")
           
    ),
    
    # Current production
    column(12,
           
           # Header
           tags$h4(text$item_label[text$item_id == "aqua-potential-historical"]),
           
           # Intro
           includeHTML("./text/aqua_potential_historical_plot.html"),
        
           # Plot
           plotOutput("historical_production_plot")
        
    ),
    
    # Future production
    column(12,
           
           # Header
           tags$h4(text$item_label[text$item_id == "aqua-potential-future"]),
           
           # Intro
           includeHTML("./text/aqua_potential_future_plot.html"),
           
           # Plot
           plotOutput("future_production_plot")
           
    )
           
  )
  
}