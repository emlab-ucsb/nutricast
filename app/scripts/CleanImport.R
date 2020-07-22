### ------------------------------------------------
### Load and wrangle the terrible .xlsx input file
### ------------------------------------------------

CleanImport <- function(file_path, file_name){
  
dat_path <- file_path

### Section 1 - Intermediate ----------

### Get column names from the intermediate sheet
intermediate_dat_names <- suppressMessages(
  (
  read_excel(dat_path, sheet = "Intermediate", n_max = 3)[2,] %>%
  gather(column, name_top) %>%
  tidyr::fill(name_top) %>%
  dplyr::select(name_top)
  
  ) %>%
  bind_cols(
    read_excel(dat_path, sheet = "Intermediate", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )
  )%>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean)
  
### Load intermediate dat and apply
intermediate_dat <- suppressMessages(read_excel(dat_path, sheet = "Intermediate", skip = 3)[,1:length(intermediate_dat_names$name)]) %>%
  setNames(intermediate_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvest_year = year(harvest_date),
         harvest_month = month(harvest_date),
         still_in = case_when(is.na(harvest_date) ~ T,
                              TRUE ~ F))

### Section 2 - Cycle 1 ----------

### Get column names from the cycle 1 sheet
cycle_1_dat_names <- suppressMessages(
  (
  read_excel(dat_path, sheet = "Cycle 1", n_max = 3)[2,] %>%
    gather(column, name_top) %>%
    tidyr::fill(name_top) %>%
    dplyr::select(name_top)
  
) %>%
  bind_cols(
    read_excel(dat_path, sheet = "Cycle 1", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )) %>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean)

### Load intermediate dat and apply
cycle_1_dat <- suppressMessages(read_excel(dat_path, sheet = "Cycle 1", skip = 3)[,1:length(cycle_1_dat_names$name)]) %>%
  setNames(cycle_1_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvest_year = year(harvest_date),
         harvest_month = month(harvest_date),
         still_in = case_when(is.na(harvest_date) ~ T,
                              TRUE ~ F))

### Section 3 - Cycle 2 ----------

### Get column names from the cycle 1 sheet
cycle_2_dat_names <- suppressMessages(
  (
  read_excel(dat_path, sheet = "Cycle 2", n_max = 3)[2,] %>%
    gather(column, name_top) %>%
    tidyr::fill(name_top) %>%
    dplyr::select(name_top)
  
) %>%
  bind_cols(
    read_excel(dat_path, sheet = "Cycle 2", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )) %>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean)

### Load intermediate dat and apply
cycle_2_dat <- suppressMessages(read_excel(dat_path, sheet = "Cycle 2", skip = 3)[,1:length(cycle_2_dat_names$name)]) %>%
  setNames(cycle_2_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvested_year = year(harvested_date),
         harvested_month = month(harvested_date),
         still_in = case_when(is.na(harvested_date) ~ T,
                              TRUE ~ F))

### Section 4 - <40 ----------
w_40_dat_names <- suppressMessages(
(
  read_excel(dat_path, sheet = "<40", n_max = 3)[2,] %>%
    gather(column, name_top) %>%
    tidyr::fill(name_top) %>%
    dplyr::select(name_top)
  
) %>%
  bind_cols(
    read_excel(dat_path, sheet = "<40", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )) %>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean)

### Load intermediate dat and apply
w_40_dat <- suppressMessages(read_excel(dat_path, sheet = "<40", skip = 3)[,1:length(w_40_dat_names$name)]) %>%
  setNames(w_40_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvest_year = year(harvested_date),
         harvest_month = month(harvested_date),
         still_in = case_when(is.na(harvested_date) ~ T,
                              TRUE ~ F))


### Section 5 - <59 ----------

w_59_dat_names <- suppressMessages(
(
  read_excel(dat_path, sheet = "<59", n_max = 3)[2,] %>%
    gather(column, name_top) %>%
    tidyr::fill(name_top) %>%
    dplyr::select(name_top)
  
) %>%
  bind_cols(
    read_excel(dat_path, sheet = "<59", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )) %>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean) 

### Load intermediate dat and apply
w_59_dat <- suppressMessages(read_excel(dat_path, sheet = "<59", skip = 3)[,1:length(w_59_dat_names$name)]) %>%
  setNames(w_59_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvest_year = year(harvested_date),
         harvest_month = month(harvested_date),
         still_in = case_when(is.na(harvested_date) ~ T,
                              TRUE ~ F))

### Section 6 - 60-84 ----------

w_60_84_dat_names <- suppressMessages(
(
  read_excel(dat_path, sheet = "60-84", n_max = 3)[2,] %>%
    gather(column, name_top) %>%
    tidyr::fill(name_top) %>%
    dplyr::select(name_top)
  
) %>%
  bind_cols(
    read_excel(dat_path, sheet = "60-84", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )) %>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean)

### Load intermediate dat and apply
w_60_84_dat <- suppressMessages(read_excel(dat_path, sheet = "60-84", skip = 3)[,1:length(w_60_84_dat_names$name)]) %>%
  setNames(w_60_84_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvest_year = year(harvested_date),
         harvest_month = month(harvested_date),
         still_in = case_when(is.na(harvested_date) ~ T,
                              TRUE ~ F))

### Section 7 - 85-99 ----------

w_85_99_dat_names <- suppressMessages(
  (
  read_excel(dat_path, sheet = "85-99", n_max = 3)[2,] %>%
    gather(column, name_top) %>%
    tidyr::fill(name_top) %>%
    dplyr::select(name_top)
  
) %>%
  bind_cols(
    read_excel(dat_path, sheet = "85-99", n_max = 3)[3,] %>%
      gather(column, name_bottom) %>%
      dplyr::select(name_bottom) %>%
      mutate(name_bottom = case_when(is.na(name_bottom) ~ "",
                                     TRUE ~ name_bottom))
  )) %>%
  mutate(name = paste(name_top, name_bottom)) %>%
  mutate(name_clean = make_clean_names(name)) %>%
  dplyr::select(name, name_clean)

### Load intermediate dat and apply
w_85_99_dat <- suppressMessages(read_excel(dat_path, sheet = "85-99", skip = 3)[,1:length(w_85_99_dat_names$name)]) %>%
  setNames(w_85_99_dat_names$name_clean) %>%
  dplyr::filter(!is.na(stock_date)) %>%
  mutate(spawn = file_name,
         harvest_year = year(harvested_date),
         harvest_month = month(harvested_date),
         still_in = case_when(is.na(harvested_date) ~ T,
                              TRUE ~ F))

print(paste0("Data loaded for the following spawn: ", file_name))

return(out = list(intermediate = intermediate_dat,
                  cycle_1 = cycle_1_dat,
                  cycle_2 = cycle_2_dat,
                  x40 = w_40_dat,
                  x59 = w_59_dat,
                  x60_84 = w_60_84_dat,
                  x85_99 = w_85_99_dat))

}

