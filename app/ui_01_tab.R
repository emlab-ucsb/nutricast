### ----------------------------------------------
### ----------------------------------------------
### This script contains the UI for tab 1
### ----------------------------------------------
### ----------------------------------------------

Tab1 <- function(){
  
  fluidPage(
    
    # page style
    style = "padding-bottom: 40px;",
    
    # Row of value boxes 
    fluidRow(
      
      # Value Box 1
      valueBoxOutput("value_box_1"),
      
      # Value Box 2
      valueBoxOutput("value_box_2"),
      
      # Box with widget
      box(width = 4,
          height = 100,
          solidHeader = T,
          selectizeInput("w_dropdown_fancy",
                         label = text$item_label[text$item_id == "w_dropdown_fancy"],
                         choices = rownames(dat), 
                         width = "100%")
      )

    ),
    
    # Plot in a dashboard box
    fluidRow(
      
      box(width = 12,
          solidHeader = T,
          title = "title",
          plotOutput("plot_2"))
      
    ),
    
   # Plot in a plain "material" box
   fluidRow(
     
     # plot
     box(width = 8,
         height = 400,
         status = "warning",
         plotOutput("plot_1", height = 370)
         
     ),
     
     # widgets
     box(width = 4,
         height = 400,
         background = "black",
         "box content")
     
   )
   
  ) # /fluidPage
  
}