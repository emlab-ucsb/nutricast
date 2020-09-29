### --------------------------------------
### --------------------------------------
### This script defines the server
### --------------------------------------
### --------------------------------------

shinyServer(function(input, output, session) {
  
  ### ----------------------------
  ### overview Tab ---------------
  ### ----------------------------
  
  # Navigation links within overview tab
  observeEvent(input$al_introduction, {
    updateTabItems(session, "menu-items", "overview")
    ### Bring to top of page
  })
  
  observeEvent(input$al_marine_seafood_production_scenarios, {
    updateTabItems(session, "menu-items", "overview")
    ### Bring to top of page
  })
  
  observeEvent(input$al_references_contact, {
    updateTabItems(session, "menu-items", "overview")
    ### Bring to top of page
  })
  
  # Navigation buttons to other sections
  observeEvent(input$ab_overview_to_global_national_outlook, {
    updateTabItems(session, "menu-items", "global-national-outlook")
  })
  
  observeEvent(input$ab_overview_to_national_nutrition_data, {
    updateTabItems(session, "menu-items", "national-nutrition-data")
  })
  
  observeEvent(input$ab_overview_to_seafood_reforms, {
    updateTabItems(session, "menu-items", "seafood-reforms")
  })
  
  ### -------------------------------------------
  ### global-national-outlook Tab ---------------
  ### -------------------------------------------
  
  # Navigation links to global-national-nutrition tab
  observeEvent(input$al_population_growth, {
    updateTabItems(session, "menu-items", "global-national-outlook")
    ### Bring to top of page
  })
  
  observeEvent(input$al_nutrient_demand, {
    updateTabItems(session, "menu-items", "global-national-outlook")
    ### Bring to top of page
  })
  
  observeEvent(input$al_marine_seafood_as_source_nutrients, {
    updateTabItems(session, "menu-items", "global-national-outlook")
    ### Bring to top of page
  })
  
  observeEvent(input$al_future_seafood_supply, {
    updateTabItems(session, "menu-items", "global-national-outlook")
    ### Bring to top of page
  })
  
  ### -------------------------------------------
  ### national-nutrition-data Tab ---------------
  ### -------------------------------------------
  
  # Navigation links to national-nutrition-data tab
  observeEvent(input$al_protein_intake, {
    updateTabItems(session, "menu-items", "national-nutrition-data")
    ### Bring to top of page
  })
  
  observeEvent(input$al_seafood_consumption, {
    updateTabItems(session, "menu-items", "national-nutrition-data")
    ### Bring to top of page
  })
  
  observeEvent(input$al_nutritional_health, {
    updateTabItems(session, "menu-items", "national-nutrition-data")
    ### Bring to top of page
  })
  
  observeEvent(input$al_nutrient_consumption_profiles, {
    updateTabItems(session, "menu-items", "national-nutrition-data")
    ### Bring to top of page
  })
  
  ### -----------------------------------
  ### seafood-reforms Tab ---------------
  ### -----------------------------------
  
  # Navigation links to global-national-nutrition tab
  observeEvent(input$al_seafood_nutrition_content, {
    updateTabItems(session, "menu-items", "seafood-reforms")
    ### Bring to top of page
  })
  
  observeEvent(input$al_fisheries_reforms, {
    updateTabItems(session, "menu-items", "seafood-reforms")
    ### Bring to top of page
  })
  
  observeEvent(input$al_aquaculture_reforms, {
    updateTabItems(session, "menu-items", "seafood-reforms")
    ### Bring to top of page
  })
  
  observeEvent(input$al_mariculture_site_explorer, {
    updateTabItems(session, "menu-items", "seafood-reforms")
    ### Bring to top of page
  })
  
  ### Seafood Nutrition Content -----------------
  
  # Plot output 
  output$seafood_nutrition_content_plot <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    
    ### NEED
    
  })
  
  ### Fisheries Reforms ---------------
  
  # Plot output: Tab 1
  output$fisheries_reforms_plot_1 <- renderPlot({
    
    req(input$w_seafood_reforms_country)

    ### NEED
    
  })
  
  # Plot output: Tab 2
  output$fisheries_reforms_plot_2 <- renderPlot({
    
    req(input$w_seafood_reforms_country)

    ### NEED
    
  })
  
  # Plot output: Tab 3
  output$fisheries_reforms_plot_3 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    
    ### NEED
    
  })
  
  
  ### Aquaculture Reforms ---------------
  
  # Plot output
  output$aquaculture_reforms_plot_1 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    req(input$w_seafood_reforms_radar_species)
    
    ### NEED
    
  })
  
  # Plot output
  output$aquaculture_reforms_plot_2 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    req(input$w_seafood_reforms_radar_species)
    
    ### NEED
    
  })
  
  
  ### Mariculture Site Explorer ---------------
  
  # Plot output
  output$mariculture_site_explorer_plot <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    req(input$w_seafood_reforms_site_explorer_species)
    req(input$w_seafood_reforms_climate_scenario)
    
    ### NEED
    
  })
  
    ### ----------------------------------
    ### aqua-potential Tab ---------------
    ### ----------------------------------
  
#     ### Historical production plot ---------
#     output$historical_production_plot <- renderPlot({
#       
#       req(input$aqua_potential_select_country)
#       
#       # filter data 
#       plot_dat <- production_current_group_dat %>%
#         dplyr::filter(sov1_name == input$aqua_potential_select_country)
#       
#       # plot
#       plot <- ggplot(plot_dat, aes(x = year, y = quantity_mt/1e6, fill = isscaap))+
#         geom_area()+
#         theme_bw()+
#         labs(x = "Year", y = "Production (million mt)", fill = "Commercial group")
#       
#       plot
#       
#     })
#     
#     ### Future production plot ---------
#     output$future_production_plot <- renderPlot({
#       
#       req(input$aqua_potential_select_country)
#       
#       # filter data 
#       plot_dat <- production_all_sovereign_dat %>%
#         dplyr::filter(sov1_name == input$aqua_potential_select_country)
#       
#       # plot
#       plot <- ggplot(plot_dat, aes(x = year, y = quantity_mt/1e6, color = interaction(feed_scen,dev_pattern, sep=" - ")))+
#         geom_line()+
#         theme_bw()+
#         labs(x = "Year", y = "Production (million mt)", color = "Scenario (Feed - Development)")+
#         facet_grid(group ~ rcp, scales = "free")+
#         theme(legend.position = "right")
#       
#       plot
#       
#     })
#     
#     ### ----------------------------------
#     ### aqua-species Tab ---------------
#     ### ----------------------------------
#     
#     ### Reactive data set based on selected country and climate scenario
#     eez_species_dat <- eventReactive(c(input$aqua_species_select_country,
#                                        input$aqua_species_select_scenario), {
#                                          
#                                          plot_dat <- rcp_projections %>%
#                                            dplyr::filter(sov1_name == input$aqua_species_select_country & scenario == input$aqua_species_select_scenario) %>%
#                                            dplyr::filter(year == 2100) %>%
#                                            mutate(rank = dense_rank(desc(prod_mt_yr))) %>%
#                                            mutate(species = fct_reorder(species, desc(rank)))                               
#                                        })
#     
#     ### Future production plot ---------
#     output$future_species_production_plot <- renderPlot({
#       
#       req(nrow(eez_species_dat()) > 0)
#       
#       # plot
#       plot <- ggplot(eez_species_dat(), aes(x = species, y = prod_mt_yr/1e6, fill = group))+
#         geom_bar(stat = "identity")+
#         theme_bw()+
#         labs(x = "", y = "Production (million mt)", fill = "Species Type")+
#         coord_flip()+
#         facet_wrap(~ group, scales = "free", ncol = 2)+
#         theme(legend.position = "none")
# 
#       plot
#       
#     })
#     
#     ### Update species select input based on country and RCP selected above ---------
#     observe({
#       
#       # Viable Species ordered 
#       viable_species_ordered <- eez_species_dat() %>%
#         dplyr::select(species, rank)
#       
#       # Only allow species that are viable in 2100 to be selected
#       viable_species <- nutrient_dat_pmax %>%
#         inner_join(viable_species_ordered, by = "species") %>%
#         mutate(species = fct_reorder(species, rank))
#       
#       # Update input
#       updateSelectizeInput(session, 
#                            "aqua_species_select_species",
#                            choices = levels(viable_species$species))
#     })
#     
#     ### Nutrition radar plot for selected species (no more than 10) ---------
#     output$future_species_nutrition <- renderPlot({
#       
#       req(nrow(eez_species_dat()) > 0,
#           length(input$aqua_species_select_species) > 0)
#       
#       # Subset data
#       plot_data <- nutrient_dat_pmax %>% 
#         filter(species %in% input$aqua_species_select_species)
#       
#       req(nrow(plot_data) > 0)
#       
#       # Plot data
#       g <- ggradar(plot_data) + 
#         theme(legend.position = "right")
#       g
#       
#     })
#     
#     ### ----------------------------------
#     ### aqua-explorer Tab ---------------
#     ### ----------------------------------
#     
#     ### Reactive data set based on selected country and climate scenario ---
#     aqua_explorer_dat <- eventReactive(c(input$aqua_explorer_select_species,
#                                        input$aqua_explorer_select_scenario), {
# 
#                                          
#           # plot_dat <- rcp_projections %>%
#           #             dplyr::filter(species == input$aqua_explorer_select_species & 
#           #                             scenario == input$aqua_explorer_select_scenario)
#          # Load data
#          scenario <- str_replace(str_replace(input$aqua_explorer_select_scenario, " ", ""), "[.]", "")
#          category <- unique(rcp_projections$group[rcp_projections$species == input$aqua_explorer_select_species])
#          species <- str_replace(input$aqua_explorer_select_species, " ", "_")  
#          dat_file_name <- paste0(scenario, "_", category, "_", species, ".Rds")
#          
#          data <- readRDS(paste0("./data/raw/", dat_file_name))
#          
#          })
#     
#     ### Update scenario select input based species selected  ---------
#     observe({
# 
#       # Viable Species ordered
#       possible_scenarios_years <- rcp_projections %>%
#         dplyr::filter(species == input$aqua_explorer_select_species) %>%
#         distinct(species, scenario, year)
# 
#       scenarios <- unique(possible_scenarios_years$scenario)
#       
#       # Update input
#       updateSelectizeInput(session,
#                            "aqua_explorer_select_scenario",
#                            choices = scenarios)
#       
#     })
#     
#     ### Update species select input based on scenario selected  ---------
#     # observe({
#     #   
#     #   # Viable Species ordered
#     #   possible_species_years <- rcp_projections %>%
#     #     dplyr::filter(scenario == input$aqua_explorer_select_scenario) %>%
#     #     distinct(species, scenario, year)
#     #   
#     #   scenarios <- unique(possible_scenarios_years$scenario)
#     #   
#     #   # Update input
#     #   updateSelectizeInput(session,
#     #                        "aqua_species_select_scenario",
#     #                        choices = scenarios)
#     #   
#     # })
#     
#     ### Production map for selected species ---------
#     output$future_species_production_map <- renderPlot({
# 
#       req(nrow(aqua_explorer_dat()) > 0,
#           input$aqua_explorer_select_year)
# 
#       # Filter for year, and plot 
#       plot_dat <- aqua_explorer_dat() %>%
#         dplyr::filter(year == input$aqua_explorer_select_year)
#       
#       # data_raster <- raster::rasterFromXYZ(data,
#       #                                      crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
#       #    
#       
#       # Plot data
#       g <- ggplot() +
#         geom_sf(data = worldmap) + 
#         coord_sf(crs = st_crs('+proj=moll'),
#                  expand = F) +
#         geom_raster(data = plot_dat, aes(x = x, y = y), fill = "red")+
#         labs(x = "", y = "")+
#         theme_bw()+
#         # scale_x_continuous(limits = c(-18086282, 18083718))+
#         # scale_y_continuous(limits = c(-9069952, 9070048))+
#         theme(axis.text = element_blank())
#       
#       g
# 
#     })
#     

})



