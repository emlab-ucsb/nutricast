### ------------------------------------------------
### Summarize across all spawns for harvest month
### ------------------------------------------------

SummarizeBigAbs <- function(dat,
                            results_path = here::here("results/"),
                            month,
                            year){

  ### Housekeeping 
  
  month_num <- match(month, month.name)
  
  browser()
  
  ### Sheet 1 - <59 ----------
  
  x59_summary <- dat$x59 %>%
    group_by(harvested_year, harvested_month) %>%
    summarize(avg_cycle_length = mean(cycle_length, na.rm = T),
              avg_specific_growth_rate = mean(specific_growth_rate, na.rm = T),
              avg_output_40_59 = mean(harvested_weight_59, na.rm = T),
              avg_output_60_84 = mean(harvested_weight_60_84, na.rm = T),
              avg_output_85_104 = mean(harvested_weight_85_104, na.rm = T),
              avg_output_105_130 = mean(harvested_weight_105_130, na.rm = T),
              avg_output_131 = mean(harvested_weight_131, na.rm = T),
              avg_sales_size = mean(avg_100_125_piece_wt_g, na.rm = T),
              num_tanks = n(),
              num_spawns = n_distinct(spawn)) %>%
    mutate(size = "<59")
  
  ### Sheet 2 - 60-84 ----------
  
  x60_84_summary <- dat$x60_84 %>%
    group_by(harvested_year, harvested_month) %>%
    summarize(avg_cycle_length = mean(cycle_length, na.rm = T),
              avg_specific_growth_rate = mean(specific_growth_rate, na.rm = T),
              avg_output_40_59 = mean(harvested_weight_59, na.rm = T),
              avg_output_60_84 = mean(harvested_weight_60_84, na.rm = T),
              avg_output_85_104 = mean(harvested_weight_85_104, na.rm = T),
              avg_output_105_130 = mean(harvested_weight_105_130, na.rm = T),
              avg_output_131 = mean(harvested_weight_131, na.rm = T),
              avg_sales_size = mean(avg_100_125_piece_wt_g, na.rm = T),
              num_tanks = n(),
              num_spawns = n_distinct(spawn)) %>%
    mutate(size = "60-84")
  
  ### Sheet 3 - 85-99 ----------
  
  x85_99_summary <- dat$x85_99 %>%
    group_by(harvested_year, harvested_month) %>%
    summarize(avg_cycle_length = mean(cycle_length, na.rm = T),
              avg_specific_growth_rate = mean(specific_growth_rate, na.rm = T),
              avg_output_40_59 = mean(harvested_weight_59, na.rm = T),
              avg_output_60_84 = mean(harvested_weight_60_84, na.rm = T),
              avg_output_85_104 = mean(harvested_weight_85_104, na.rm = T),
              avg_output_105_130 = mean(harvested_weight_105_130, na.rm = T),
              avg_output_131 = mean(harvested_weight_131, na.rm = T),
              avg_sales_size = mean(avg_100_125_piece_wt_g, na.rm = T),
              num_tanks = n(),
              num_spawns = n_distinct(spawn)) %>%
    mutate(size = "85-99")
  
  # Combine
  summary <- x59_summary %>%
    bind_rows(x60_84_summary) %>%
    bind_rows(x85_99_summary) %>%
    mutate(year_month = paste0(summary_year, "_", summary_month)) %>%
    arrange(size, summary_year, summary_month)
  
  # Extract current month/year and two preeceeding months and save
  current_month <- summary %>%  
    dplyr::filter(summary_month == month_num & summary_year == year)
    
  write_csv(current_month, path = paste0(results_path, "/", year, "_", month, "_data_summary.csv"))
  
  return(summary)
  
}