### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

NationalNutritionData <- function(country_choices,
                                  nutrient_choices){
  
  fluidPage(
    
      # Page style
      style = "padding-bottom: 40px;",
      
      # Header
      column(12, id = "header-div",
             
             h3(text$item_label[text$item_id == "national-nutrition-data"])
             
      ),
      
      # Introductory text
      column(12, id = "tb-spaced-div-underline",
             
             column(8, id = "lr-spaced-div",
                    
                    # Page text
                    includeHTML("./text/03-national-nutrition-data/instructions.html")
                    
             ),
             
             column(4, id = "lr-spaced-div",
                    
                    selectizeInput("w_national_nutrition_data_country",
                                   label = text$item_label[text$item_id == "w_national_nutrition_data_country"],
                                   choices = country_choices,
                                   selected = "USA",
                                   width = "100%")
             )
      
      ),
      
      # Story text 1
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_1.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "protein_intake",
             ""
      ),
      
      # Content box 1: Protein Intake
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    # Section title
                    tags$h4(text$item_label[text$item_id == "protein_intake"]),
                    
                    # Reactive description text
                    uiOutput("protein_intake_text")
                    
                    # includeHTML("./text/03-national-nutrition-data/protein_intake_1.html") ### Make reactive
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "protein_intake_tabs", width = 12,
                           
                           tabPanel(value = "protein-intake-tabs-1",
                                    text$item_label[text$item_id == "protein-intake-tabs-1"],
                                    
                                    ### PLOT PLACEHOLDER
                                    plotOutput("protein_intake_plot_1", height = "500px", width = "auto")
                           ),
                           
                           tabPanel(value = "protein-intake-tabs-2",
                                    text$item_label[text$item_id == "protein-intake-tabs-2"],
                                    
                                    ### PLOT PLACEHOLDER
                                    plotOutput("protein_intake_plot_2", height = "500px", width = "auto")
                           )
                    )
                    
             )
             
      ),
      
      # Story text 2
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_2.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "seafood_consumption",
             ""
      ),
      
      # Content box 2: Seafood Consumption & Nutrient Contributions
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "seafood_consumption_tabs", width = 12,
                           
                           tabPanel(value = "seafood-consumption-tabs-1",
                                   text$item_label[text$item_id == "seafood-consumption-tabs-1"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("seafood_consumption_plot_1", height = "500px", width = "auto")
                           ),
                           
                           tabPanel(value = "seafood-consumption-tabs-2",
                                   text$item_label[text$item_id == "seafood-consumption-tabs-2"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("seafood_consumption_plot_2", height = "500px", width = "auto")
                           ),
                           
                           tabPanel(value = "seafood-consumption-tabs-3",
                                    text$item_label[text$item_id == "seafood-consumption-tabs-3"],
                                    
                                    ### PLOT PLACEHOLDER
                                    plotOutput("seafood_consumption_plot_3", height = "500px", width = "auto")
                           )
                    )
                    
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    # Section title
                    tags$h4(text$item_label[text$item_id == "seafood_consumption"]),
                    
                    # Reactive description text
                    uiOutput("seafood_consumption_text")
                    # includeHTML("./text/03-national-nutrition-data/seafood_consumption_1.html") #### MAKE REACTIVE
                    
             )
             
      ),
      
      # Story text 3
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_3.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "nutritional_health",
             ""
      ),
      
      # Content box 3: Nutritional Health
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "nutritional_health"]),
                    
                    includeHTML("./text/03-national-nutrition-data/nutritional_health.html")
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    ### PLOT PLACEHOLDER
                    plotOutput("nutritional_health_plot", height = "500px", width = "auto")
                    
             )
             
      ),
      
      # Story text 4
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_4.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "nutrient_consumption_profiles",
             ""
      ),
      
      # Content box 4: Nutrient Consumption Profiles
      column(12, id = "shaded-div", style = "padding-left: 2vw;",
             
             column(9, style = "border: 2px solid #1a2d3f;",
                    
                    ### Plot
                    plotOutput("nutrient_consumption_profiles_plot_1", height = "400px", width = "auto"),
                    
                    ### Plot
                    plotOutput("nutrient_consumption_profiles_plot_2", height = "400px", width = "auto")
                    

             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "nutrient_consumption_profiles"]),
                    
                    selectizeInput("w_national_nutrition_data_nutrient",
                                   label = tagList(
                                     tags$b(text$item_label[text$item_id == "w_national_nutrition_data_nutrient"]),
                                     # Info button
                                     tags$button(id = "info_nutrient_types",
                                                 class = "btn action-button info-button",
                                                 icon("info"))),
                                   choices = nutrient_choices,
                                   selected = nutrient_choices[1]),
                    
                    includeHTML("./text/03-national-nutrition-data/nutrient_consumption_profiles.html")
                    
             )
             
      )
      
  )
  
}