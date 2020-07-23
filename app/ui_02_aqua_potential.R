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
                          label = "Select a Country",
                          choices = country_choices,
                          width = "50%")
           
    ),
    
    # Current production
    column(12,
           
           # Header
           tags$h4("Historical aquaculture production"),
           
           # Intro
           includeHTML("./text/aqua_potential_historical.html"),
        
           # Plot
           plotOutput("historical_production_plot")
        
    ),
    
    
    # Future production under climate change
    box(width = 12,
        #height = 100,
        #solidHeader = T,
        collapsible = T,
        title = "Projected production under climate change"
        
        # Intro
        
        # Plot
        #plotOutput("current_production_plot")
        
    )
           
  )
  
}