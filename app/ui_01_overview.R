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
                                 
                                 tags$h1(text$item_label[text$item_id == "app-title"]),
                                 tags$h3(text$item_label[text$item_id == "overview_header"])
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
    
    # Marine Seafood Production Scenarios
    column(12, id = "spaced-div",
           
           # Header
           h3(text$item_label[text$item_id == "marine_seafood_production_scenarios"])
           
           # Page text
           #includeHTML("./text/overview_text.html")
    ),
    
    # Marine Seafood Production Scenarios
    column(12, id = "spaced-div",
           
           # Header
           h3(text$item_label[text$item_id == "references_contact"])
           
           # Page text
           #includeHTML("./text/overview_text.html")
    )
    
  )
  
}