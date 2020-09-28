### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

GlobalNationalOutlook <- function(){
  
  fluidPage(
    
      # Page style
      style = "padding-bottom: 40px;",
      
      # Header
      column(12, id = "header-div",
             
             h3(text$item_label[text$item_id == "global-national-outlook"])
             
      ),
      
      # Introductory text
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/instructions.html")
      
      ),
      
      # Widgets
      column(12, id = "spaced-div-underline",
             
             fluidRow(
               
               column(3, offset = 2, align = "center", id = "lr-spaced-div",
                      
                      radioButtons("w_global_national_outlook_resolution",
                                   label = text$item_label[text$item_id == "w_global_national_outlook_resolution"],
                                   choices = c("Global", "National"),
                                   selected = "Global",
                                   inline = TRUE)
               ),
               
               conditionalPanel("input.w_global_national_outlook_resolution == 'National'",
                                
                                column(5, align = "center", id = "lr-spaced-div",
                                       
                                       selectizeInput("w_global_national_outlook_country",
                                                      label = text$item_label[text$item_id == "w_global_national_outlook_country"],
                                                      choices = c("A", "B", "D"),
                                                      selected = "A",
                                                      width = "100%")
                                )
               )
             )
      ),
      
      # Story text 1
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_1.html")
             
      ),
      
      # Content box 1: Population Growth
      column(12, id = "shaded-div",
             
             column(3,
                    
                    tags$h4(text$item_label[text$item_id == "population_growth"]),
                    
                    includeHTML("./text/02-global-national-outlook/population_growth.html")
                    
             ),
             
             column(9,
                    
                    ### PLOT PLACEHOLDER
                    
             )
             
      ),
      
      # Story text 2
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_2.html")
             
      ),
      
      # Content box 2: Nutrient Demand
      column(12, id = "shaded-div",
             
             column(9,
                    
                    ### PLOT PLACEHOLDER
                    
             ),
             
             column(3,
                    
                    tags$h4(text$item_label[text$item_id == "nutrient_demand"]),
                    
                    includeHTML("./text/02-global-national-outlook/nutrient_demand_1.html") #### MAKE REACTIVE
                    
             )
             
      ),
      
      # Story text 3
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_3.html")
             
      ),
      
      # Content box 3: Marine Seafood as a Source of Nutrients
      column(12, id = "shaded-div",
             
             column(3,
                    
                    tags$h4(text$item_label[text$item_id == "marine_seafood_as_source_nutrients"]),
                    
                    includeHTML("./text/02-global-national-outlook/marine_seafood_as_source_1.html") #### MAKE REACTIVE
                    
             ),
             
             column(9,
                    
                    ### PLOT PLACEHOLDER
                    
             )
             
      ),
      
      # Story text 4
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_4.html")
             
      ),
      
      # Content box 4: Future Seafood Supply & Nutrient Contributions
      column(12, id = "shaded-div",
             
             column(9,
                    
                    ### PLOT PLACEHOLDER
                    
             ),
             
             column(3,
                    
                    tags$h4(text$item_label[text$item_id == "future_seafood_supply"]),
                    
                    includeHTML("./text/02-global-national-outlook/future_seafood_supply_1.html") #### MAKE REACTIVE
                    
             )
             
      )
      
  )
  
}