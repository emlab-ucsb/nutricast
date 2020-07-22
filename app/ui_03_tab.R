### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for tab 3
### ----------------------------------------------
### ----------------------------------------------

Tab3 <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    h2(text$item_label[text$item_id == "tab-3"])
    
  )
  
}