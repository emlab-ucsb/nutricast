### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

Overview <- function(){
  
  fluidPage(
    
      # page style
      style = "color: #ffffff; padding-bottom: 40px;",
      
      # open links in new window
      tags$head(tags$base(target = "_blank")),
      
      ### Image with text -------------------
      
      # Parent container: landing page header (photo + overlay)
      tags$div(class = "landing-wrapper",
               
               # Child element 1: background image
               tags$div(class = "picture-wrapper",
                        
                        tags$img(src = "intro-background.jpg")
               ),
               
               # Child element 2: overlay
               tags$div(class = "picture-overlay-black",
                        
                        # Main Text
                        tags$div(class = "picture-overlay-main-text",
                                 
                                 includeHTML("./text/01-overview/background.html")
                                 
                        ),
                        
                        # Buttons
                        
                        tags$div(class = "picture-overlay-button-row",
                                 
                                 tags$div(class = "picture-overlay-button-wrapper-l",
                                          
                                          actionButton("ab_overview_to_global",
                                                       tags$h4(text$item_label[text$item_id == "ab_introduction_to_selected_results"]))
                                          
                                 ),
                                 
                                 tags$div(class = "picture-overlay-button-wrapper-c",
                                          
                                          actionButton("ab_overview_to_national",
                                                       tags$h4(text$item_label[text$item_id == "ab_introduction_to_methods_process"]))
                                          
                                 ),
                                 
                                 tags$div(class = "picture-overlay-button-wrapper-c",
                                          
                                          actionButton("ab_overview_to_reforms",
                                                       tags$h4(text$item_label[text$item_id == "ab_introduction_to_methods_process"]))
                                          
                                 ),
                                 
                                 tags$div(class = "picture-overlay-button-wrapper-r",
                                          
                                          actionButton("ab_overview_to_nutrition",
                                                       tags$h4(text$item_label[text$item_id == "ab_introduction_to_global_subsidies"]))
                                          
                                 )
                                 
                        ),
                        
                        # Footer
                        tags$div(class = "picture-overlay-footer-row",
                                 
                                 tags$div(class = "picture-overlay-footer-wrapper",
                                          
                                          actionLink("al_introduction_to_need_help",
                                                     tags$h4(text$item_label[text$item_id == "al_introduction_to_need_help"]))
                                          
                                 )
                                 
                        ) # /div picture-overlay-row
                        
               ) # /div picture-overlay-black
               
      ), # /div landing-wrapper
    
    # Introduction
    column(12,
           
           # Header
           h3(text$item_label[text$item_id == "overview"]),
           
           # Page text
           includeHTML("./text/overview_text.html")
    )
    
  )
  
}