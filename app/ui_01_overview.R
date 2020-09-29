### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

Overview <- function(){
  
  fluidPage(
    
      # page style
      style = "padding-bottom: 40px;",
      
      # open links in new window
      tags$head(tags$base(target = "_blank")),
      
      ### Image with text -------------------
      
      # Parent container: landing page header (photo + overlay)
      tags$div(id = "overview_div",
               
               # Child element 1: background image
               tags$div(id = "overview_picture_div",
                        
                        tags$img(src = "overview_photo.jpg")
               ),
               
               # Child element 2: overlay
               tags$div(id = "overview_picture_overlay",
                        
                        # Main Text
                        tags$div(id = "overview_picture_overlay_text",
                                 
                                 tags$h2(text$item_label[text$item_id == "app-title"], style = "color: white;"),
                                 tags$br(),
                                 tags$h4(text$item_label[text$item_id == "overview_header"])
                        )
                     
               ) # /div
               
      ), # /div 
    
    # Introduction
    column(12, id = "spaced-div",

           # Header
           h3(text$item_label[text$item_id == "introduction"]),

           # Page text
           includeHTML("./text/01-overview/introduction.html")
    ),
    
    # Section navigation buttons
    column(12, id = "spaced-div",
           
           # table
           tags$table(id = "overview-table",
             tags$tr(id = "overview-table-row",
               tags$td(id = "overview-table-cell-1",
                 
                 actionButton("ab_overview_to_global_national_outlook",
                              tags$b(text$item_label[text$item_id == "ab_overview_to_global_national_outlook"], icon("caret-right")))
                 
               ),
               tags$td(id = "overview-table-cell-2",
                 
                 actionButton("ab_overview_to_national_nutrition_data",
                              tags$b(text$item_label[text$item_id == "ab_overview_to_national_nutrition_data"], icon("caret-right")))
                 
               ),
               tags$td(id = "overview-table-cell-3",
                 
                 actionButton("ab_overview_to_seafood_reforms",
                              tags$b(text$item_label[text$item_id == "ab_overview_to_seafood_reforms"], icon("caret-right")))
                 
               )
             )
           )
    ),
    
    # Placeholder to scroll to for references and contact
    column(12, id = "marine_seafood_production_scenarios",
           ""
    ),
    
    # Marine Seafood Production Scenarios
    column(12, id = "spaced-div",
           
                    # Header
                    h3(text$item_label[text$item_id == "marine_seafood_production_scenarios"]),
                    
                    # Page text
                    includeHTML("./text/01-overview/marine_seafood_production_scenarios.html")
    ),
    
    # Placeholder to scroll to for references and contact
    column(12, id = "references_contact",
           ""
    ),
    
    # Marine Seafood Production Scenarios
    column(12, id = "spaced-div",
           
                    # Header
                    h3(text$item_label[text$item_id == "references_contact"]),
                    
                    # Page text
                    includeHTML("./text/01-overview/references_contact.html")
    )
    
  )
  
}