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
      column(12, id = "page-header",
             
             h3(text$item_label[text$item_id == "global-national-outlook"])
             
      )
      
  )
  
}