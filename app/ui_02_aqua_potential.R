### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for the aqua-potential tab
### ----------------------------------------------
### ----------------------------------------------

AquaPotential <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    h3(text$item_label[text$item_id == "aqua-potential"])
    
  )
  
}