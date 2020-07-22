### ---------------------------------
### Misc. helper functions
### ---------------------------------

# Little function to smoosh together our data from each spawn (by stage) so it's easier to work with
CombineElements <- function(stage, dat_list){
  
  out <- lapply(dat_list, `[[`, stage) %>%
    bind_rows()
  
}