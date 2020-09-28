### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

SeafoodReforms <- function(){
  
  fluidPage(
    
      # Page style
      style = "padding-bottom: 40px;",
      
      # Header
      column(12, id = "header-div",
             
             h3(text$item_label[text$item_id == "seafood-reforms"])
             
      ),
      
      # Introductory text
      column(12, id = "tb-spaced-div-underline",
             
             column(8, id = "lr-spaced-div",
                    
                    # Page text
                    includeHTML("./text/04-seafood-reforms/instructions.html")
                    
             ),
             
             column(4, id = "lr-spaced-div",
                    
                    selectizeInput("w_seafood_reforms_country",
                                   label = text$item_label[text$item_id == "w_seafood_reforms_country"],
                                   choices = c("A", "B", "D"),
                                   selected = "A",
                                   width = "100%")
             )
      
      ),
      
      # Story text 1
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_1.html")
             
      ),
      
      # Content box 1: Population Growth
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "population_growth"]),
                    
                    includeHTML("./text/02-global-national-outlook/population_growth.html")
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
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
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "nutrient-demand-tabs", width = 12,
                           
                           tabPanel(value = "nutrient-demand-tabs-1",
                                   text$item_label[text$item_id == "nutrient-demand-tabs-1"]
                                   
                                   ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "nutrient-demand-tabs-2",
                                   text$item_label[text$item_id == "nutrient-demand-tabs-2"]
                                   
                                   ### PLOT PLACEHOLDER
                           )
                    )
                    
             ),
             
             column(3, id = "lr-spaced-div",
                    
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
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "marine_seafood_as_source_nutrients"]),
                    
                    includeHTML("./text/02-global-national-outlook/marine_seafood_as_source_1.html") #### MAKE REACTIVE
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "marine-seafood-as-source-tabs", width = 12,
                           
                           tabPanel(value = "marine-seafood-as-source-tabs-1",
                                   text$item_label[text$item_id == "marine-seafood-as-source-tabs-1"]
                                   
                                   ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "marine-seafood-as-source-tabs-2",
                                   text$item_label[text$item_id == "marine-seafood-as-source-tabs-2"]
                                   
                                   ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "marine-seafood-as-source-tabs-3",
                                   text$item_label[text$item_id == "marine-seafood-as-source-tabs-3"]
                                   
                                   ### PLOT PLACEHOLDER
                           )
                           
                    )
                    
             )
             
      ),
      
      # Story text 4
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_4.html")
             
      ),
      
      # Content box 4: Future Seafood Supply & Nutrient Contributions
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "future-seafood-supply-tabs", width = 12,
                           
                           tabPanel(value = "future-seafood-supply-tabs-1",
                                   text$item_label[text$item_id == "future-seafood-supply-tabs-1"]
                                     
                                   
                                   ### PLOT PLACEHOLDER
                           ),
                           
                           tabPanel(value = "future-seafood-supply-tabs-2",
                                   text$item_label[text$item_id == "future-seafood-supply-tabs-2"]
                                   
                                   ### PLOT PLACEHOLDER
                           )
                    )
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "future_seafood_supply"]),
                    
                    includeHTML("./text/02-global-national-outlook/future_seafood_supply_1.html") #### MAKE REACTIVE
                    
             )
             
      )
      
  )
  
}