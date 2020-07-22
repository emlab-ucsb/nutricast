### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the aqua-explorer tab
### ----------------------------------------------
### ----------------------------------------------

AquaExplorer <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    h3(text$item_label[text$item_id == "aqua-explorer"])
    
  )
  
}