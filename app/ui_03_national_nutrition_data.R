### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

NationalNutritionData <- function(){
  
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
                                   choices = c("A", "B", "D"),
                                   selected = "A",
                                   width = "100%")
             )
      
      ),
      
      # Story text 1
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_1.html")
             
      ),
      
      # Content box 1: Protein Intake
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "protein_intake"]),
                    
                    includeHTML("./text/03-national-nutrition-data/protein_intake_1.html") ### Make reactive
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "protein-intake-tabs", width = 12,
                           
                           tabPanel(value = "protein-intake-tabs-1",
                                    text$item_label[text$item_id == "protein-intake-tabs-1"]
                                    
                                    ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "protein-intake-tabs-2",
                                    text$item_label[text$item_id == "protein-intake-tabs-2"]
                                    
                                    ### PLOT PLACEHOLDER
                           )
                    )
                    
             )
             
      ),
      
      # Story text 2
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_2.html")
             
      ),
      
      # Content box 2: Seafood Consumption & Nutrient Contributions
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "seafood-consumption-tabs", width = 12,
                           
                           tabPanel(value = "seafood-consumption-tabs-1",
                                   text$item_label[text$item_id == "seafood-consumption-tabs-1"]
                                   
                                   ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "seafood-consumption-tabs-2",
                                   text$item_label[text$item_id == "seafood-consumption-tabs-2"]
                                   
                                   ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "seafood-consumption-tabs-3",
                                    text$item_label[text$item_id == "seafood-consumption-tabs-3"]
                                    
                                    ### PLOT PLACEHOLDER
                           )
                    )
                    
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "seafood_consumption"]),
                    
                    includeHTML("./text/03-national-nutrition-data/seafood_consumption_1.html") #### MAKE REACTIVE
                    
             )
             
      ),
      
      # Story text 3
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_3.html")
             
      ),
      
      # Content box 3: Nutritional Health
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "nutritional_health"]),
                    
                    includeHTML("./text/03-national-nutrition-data/nutritional_health.html")
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    ### Plot placeholder
                    
             )
             
      ),
      
      # Story text 4
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/03-national-nutrition-data/story_text_4.html")
             
      ),
      
      # Content box 4: Nutrient Consumption Profiles
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                   ### Plot placeholder
                   
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "nutrient_consumption_profiles"]),
                    
                    selectizeInput("w_national_nutrition_data_nutrient",
                                   label = text$item_label[text$item_id == "w_national_nutrition_data_nutrient"],
                                   choices = c("A", "B", "C"),
                                   selected = "A"),
                    
                    includeHTML("./text/03-national-nutrition-data/nutrient_consumption_profiles.html")
                    
             )
             
      )
      
  )
  
}