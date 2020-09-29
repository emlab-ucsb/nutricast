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
             includeHTML("./text/04-seafood-reforms/story_text_1.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "seafood_nutrition_content",
             ""
      ),
      
      # Content box 1: Seafood Nutrition Content
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "seafood_nutrition_content"]),
                    
                    includeHTML("./text/02-global-national-outlook/population_growth.html")
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    ### PLOT PLACEHOLDER
                    plotOutput("seafood_nutrition_content_plot", height = "500px", width = "auto")
                    
             )
             
      ),
      
      # Story text 2
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/04-seafood-reforms/story_text_2.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "fisheries_reforms",
             ""
      ),
      
      # Content box 2: Fisheries Reforms
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "fisheries-reforms-tabs", width = 12,
                           
                           tabPanel(value = "fisheries-reforms-tabs-1",
                                   text$item_label[text$item_id == "fisheries-reforms-tabs-1"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("fisheries_reforms_plot_1", height = "500px", width = "auto")
                           ),
                           
                           tabPanel(value = "fisheries-reforms-tabs-2",
                                   text$item_label[text$item_id == "fisheries-reforms-tabs-2"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("fisheries_reforms_plot_2", height = "500px", width = "auto")
                           ),
                           
                           tabPanel(value = "fisheries-reforms-tabs-3",
                                    text$item_label[text$item_id == "fisheries-reforms-tabs-3"],
                                    
                                    ### PLOT PLACEHOLDER
                                    plotOutput("fisheries_reforms_plot_3", height = "500px", width = "auto")
                           )
                    )
                    
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "fisheries_reforms"]),
                    
                    includeHTML("./text/02-global-national-outlook/nutrient_demand_1.html") #### MAKE REACTIVE
                    
             )
             
      ),
      
      # Story text 3
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/04-seafood-reforms/story_text_3.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "aquaculture_reforms",
             ""
      ),
      
      # Content box 3: Marine Seafood as a Source of Nutrients
      column(12, id = "shaded-div",
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "aquaculture_reforms"]),
                    
                    includeHTML("./text/02-global-national-outlook/marine_seafood_as_source_1.html") #### MAKE REACTIVE
                    
             ),
             
             column(9, id = "lr-spaced-div",
                    
                    tabBox(id = "aquaculture-reforms-tabs", width = 12,
                           
                           tabPanel(value = "aquaculture-reforms-tabs-1",
                                   text$item_label[text$item_id == "aquaculture-reforms-tabs-1"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("aquaculture_reforms_plot_1", height = "500px", width = "auto")
                                   
                           ),
                           
                           tabPanel(value = "aquaculture-reforms-tabs-2",
                                   text$item_label[text$item_id == "aquaculture-reforms-tabs-2"],
                                   
                                   ### PLOT PLACEHOLDER
                                   plotOutput("aquaculture_reforms_plot_2", height = "500px", width = "auto"),

                                   # Widgets
                                   column(12, id = "absolute-widget-panel-top-center",

                                          fluidRow(

                                            column(9, offset = 2,

                                                   selectizeInput("w_seafood_reforms_radar_species",
                                                                  label = text$item_label[text$item_id == "w_seafood_reforms_radar_species"],
                                                                  choices = c("A", "B", "C"),
                                                                  selected = "A",
                                                                  width = "80%")

                                            )
                                          )
                                   )
                           )

                    )
                    
             )
             
      ),
      
      # Story text 4
      column(12, id = "spaced-div",
             
             # Page text
             includeHTML("./text/04-seafood-reforms/story_text_4.html")
             
      ),
      
      # Placeholder to scroll to 
      column(12, id = "mariculture_site_explorer",
             ""
      ),
      
      # Content box 4: Future Seafood Supply & Nutrient Contributions
      column(12, id = "shaded-div",
             
             column(9, id = "lr-spaced-div",
                    
                    # Plot
                    plotOutput("mariculture_site_explorer_plot", height = "500px", width = "auto")
                    
                    # # Widgets
                    # column(12, id = "absolute-widget-panel-top-center",
                    #        
                    #        fluidRow(
                    #          
                    #          column(5, offset = 1, 
                    #                 
                    #                 selectizeInput("w_seafood_reforms_site_explorer_species",
                    #                                label = text$item_label[text$item_id == "w_seafood_reforms_site_explorer_species"],
                    #                                choices = c("A", "B", "C"),
                    #                                selected = "A",
                    #                                width = "80%")
                    #                 
                    #          ),
                    #          column(5, 
                    #                 
                    #                 selectizeInput("w_seafood_reforms_climate_scenario",
                    #                                label = text$item_label[text$item_id == "w_seafood_reforms_climate_scenario"],
                    #                                choices = c("A", "B", "C"),
                    #                                selected = "A",
                    #                                width = "80%")
                    #                 
                    #          )
                    #        )
                    # )
             ),
             
             column(3, id = "lr-spaced-div",
                    
                    tags$h4(text$item_label[text$item_id == "mariculture_site_explorer"]),
                    
                    selectizeInput("w_seafood_reforms_site_explorer_species",
                                   label = text$item_label[text$item_id == "w_seafood_reforms_site_explorer_species"],
                                   choices = c("A", "B", "C"),
                                   selected = "A",
                                   width = "100%"),
                    
                    selectizeInput("w_seafood_reforms_climate_scenario",
                                   label = text$item_label[text$item_id == "w_seafood_reforms_climate_scenario"],
                                   choices = c("A", "B", "C"),
                                   selected = "A",
                                   width = "100%"),
                    
                    includeHTML("./text/02-global-national-outlook/future_seafood_supply_1.html") #### MAKE REACTIVE
                    
             )
             
      )
      
  )
  
}