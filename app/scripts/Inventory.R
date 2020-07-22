### ---------------------------------
### Create inventory for given month
### ---------------------------------

Inventory <- function(dat,
                      results_path = here::here("results/"),
                      summary_date_start,
                      summary_date_end){
  
  
  browser()
  ### Sheet 1 - Intermediate ----------

  intermediate_inventory <- dat$intermediate %>%
    dplyr::filter((harvest_date > summary_date_start & harvest_date <= summary_date_end) | still_in) %>%
    group_by(spawn) %>%
    summarize(num_abs = sum(number_of_abalone_for_rack, na.rm = T)) %>%
    mutate(stage = "Int")
  
  ### Sheet 2 - Cycle 1 ----------
  
  cycle_1_inventory <- dat$cycle_1 %>%
    dplyr::filter((harvest_date > summary_date_start & harvest_date <= summary_date_end) | still_in) %>%
    group_by(spawn) %>%
    summarize(num_abs = sum(number_of_abalone_for_rack, na.rm = T)) %>%
    mutate(stage = "Cycle 1")
  
  ### Sheet 3 - Cycle 2 ----------
  
  cycle_2_inventory <- dat$cycle_2 %>%
    dplyr::filter((harvested_date > summary_date_start & harvested_date <= summary_date_end) | still_in) %>%
    group_by(spawn) %>%
    summarize(num_abs = sum(number_of_abalone_for_rack, na.rm = T)) %>%
    mutate(stage = "Cycle 2")
      
  
}