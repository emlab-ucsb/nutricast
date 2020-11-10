### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

GlobalNationalOutlook <- function(country_choices, nutrient_choices){
  
  fluidPage(
    
      # Page style
      style = "padding-bottom: 40px;",
      
      # Header
      column(12, id = "header-div",
             
             h3(text$item_label[text$item_id == "global-national-outlook"])
             
      ),
      
      # Introductory text
      column(12, id = "tb-spaced-div-underline",
             
             column(8, id = "lr-spaced-div",
                    
                    # Page text
                    includeHTML("./text/02-global-national-outlook/instructions.html")
                    
             ),
             
             column(4, id = "lr-spaced-div",
                    
                    radioButtons("w_global_national_outlook_resolution",
                                 label = text$item_label[text$item_id == "w_global_national_outlook_resolution"],
                                 choices = c("Global", "National"),
                                 selected = "Global",
                                 inline = TRUE),
                    
                    conditionalPanel("input.w_global_national_outlook_resolution == 'National'",
                    
                                     
                                     selectizeInput("w_global_national_outlook_country",
                                                    label = text$item_label[text$item_id == "w_global_national_outlook_country"],
                                                    choices = country_choices,
                                                    selected = "USA",
                                                    width = "100%")
                    )
             )
             
      ),
      
      # Story text 1
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_1.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "population_growth",
             ""
      ),
      
      # Content box 1: Population Growth
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    # Section title
                    tags$h4(text$item_label[text$item_id == "population_growth"]),
                    
                    # Descriptive text
                    includeHTML("./text/02-global-national-outlook/population_growth.html")
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    ### PLOT
                    plotOutput("population_growth_plot", height = "500px", width = "auto")

             )
             
      ),
      
      # Story text 2
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_2.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "nutrient_demand",
             ""
      ),
      
      # Content box 2: Nutrient Demand
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "nutrient_demand_tabs", width = 12,
                           
                           tabPanel(value = "nutrient-demand-tabs-1",
                                   text$item_label[text$item_id == "nutrient-demand-tabs-1"],
                                   
                                   ### PLOT
                                   plotOutput("nutrient_demand_plot_1", height = "800px", width = "auto"),
                                   
                                   # Widget
                                   column(6, offset = 6, id = "absolute-widget-panel-top-right",
                                          
                                          radioGroupButtons("w_nutrient_demand_plot_1",
                                                            label = NULL,
                                                            choices = c("number of people",
                                                                        "% of population"),
                                                            selected = "number of people")
                                   )
                           ),
                           
                           tabPanel(value = "nutrient-demand-tabs-2",
                                   text$item_label[text$item_id == "nutrient-demand-tabs-2"],
                                   
                                   ### PLOT
                                   plotOutput("nutrient_demand_plot_2", height = "550px", width = "auto"),
                                   
                                   # Widgets
                                   column(12, id = "absolute-widget-panel-top-center",
                                          
                                            column(9, offset = 2,
                                                   
                                                   selectizeInput("w_nutrient_demand_plot_2",
                                                                  label = text$item_label[text$item_id == "w_nutrient_demand_plot_2"],
                                                                  choices = nutrient_choices,
                                                                  width = "80%")
                                                   
                                            )
                                   )
                                   
                           )
                    )
                    
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    # Section title
                    tags$h4(text$item_label[text$item_id == "nutrient_demand"]),
                    
                    # Reactive description text
                    uiOutput("nutrient_demand_text")

             )
             
      ),
      
      # Story text 3
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_3.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "marine_seafood_as_source_nutrients",
             ""
      ),
      
      # Content box 3: Marine Seafood as a Source of Nutrients
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    # Section title
                    tags$h4(text$item_label[text$item_id == "marine_seafood_as_source_nutrients"]),
                    
                    # Reactive description text
                    uiOutput("marine_seafood_as_source_text")
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "marine_seafood_as_source_tabs", width = 12,
                           
                           tabPanel(value = "marine-seafood-as-source-tabs-1",
                                   text$item_label[text$item_id == "marine-seafood-as-source-tabs-1"],
                                   
                                   ### PLOT
                                   plotOutput("marine_seafood_as_source_plot_1", height = "600px", width = "auto"),
                                   
                                   # Widget
                                   column(6, offset = 6, id = "absolute-widget-panel-top-right",

                                          radioGroupButtons("w_marine_seafood_as_source_plot_1",
                                                            label = NULL,
                                                            choices = c("live weight",
                                                                        "edible meat",
                                                                        "edible meat per capita"),
                                                            selected = "live weight")
                                   )
                           ),
                           
                           tabPanel(value = "marine-seafood-as-source-tabs-2",
                                   text$item_label[text$item_id == "marine-seafood-as-source-tabs-2"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("marine_seafood_as_source_plot_2", height = "500px", width = "auto")
                           ),
                           
                           tabPanel(value = "marine-seafood-as-source-tabs-3",
                                   text$item_label[text$item_id == "marine-seafood-as-source-tabs-3"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("marine_seafood_as_source_plot_3", height = "600px", width = "auto")
                           )
                           
                    )
                    
             )
             
      ),
      
      # Story text 4
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/02-global-national-outlook/story_text_4.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "future_seafood_supply",
             ""
      ),
      
      # Content box 4: Future Seafood Supply & Nutrient Contributions
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "future_seafood_supply_tabs", width = 12,
                           
                           tabPanel(value = "future-seafood-supply-tabs-1",
                                   text$item_label[text$item_id == "future-seafood-supply-tabs-1"],
                                     
                                   ### PLOT PLACEHOLDER
                                   plotOutput("future_seafood_supply_plot_1", height = "500px", width = "auto"),
                                   
                                   # Widget
                                   column(6, offset = 6, id = "absolute-widget-panel-top-right",
                                          
                                          radioGroupButtons("w_future_seafood_supply_plot_1",
                                                            label = NULL,
                                                            choices = c("live weight",
                                                                        "edible meat",
                                                                        "edible meat per capita"),
                                                            selected = "live weight")
                                   )
                           ),
                           
                           tabPanel(value = "future-seafood-supply-tabs-2",
                                   text$item_label[text$item_id == "future-seafood-supply-tabs-2"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("future_seafood_supply_plot_2", height = "500px", width = "auto")
                           )
                    )
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "future_seafood_supply"]),
                    
                    uiOutput("future_seafood_supply_text")
                    
             )
             
      )
      
  )
  
}