### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the overview tab
### ----------------------------------------------
### ----------------------------------------------

Overview <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    # Introduction
    column(12,
           
           # Header
           h3(text$item_label[text$item_id == "overview"]),
           
           # Page text
           includeHTML("./text/overview_text.html")
    )
    
  )
  
}