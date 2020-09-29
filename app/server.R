### --------------------------------------
### --------------------------------------
### This script defines the server
### --------------------------------------
### --------------------------------------

shinyServer(function(input, output, session) {
  
  # Treat menuItems with children as a link for navigation purposes
  observeEvent(input$sidebarItemExpanded, {
    
    if(!is.null(input$sidebarItemExpanded)){
      updateTabItems(session, "menu-items", input$sidebarItemExpanded)
    }
  })
  
  ### ----------------------------
  ### overview Tab ---------------
  ### ----------------------------

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
  
  ### Population growth ---------------------
  
  # Plot output 
  output$population_growth_plot <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  ### Nutrient Demand ---------------------
  
  # Reactive text: Based upon selected tab
  output$nutrient_demand_text <- renderUI({
    
    selected_tab <- switch(input$nutrient_demand_tabs,
                           "nutrient-demand-tabs-1" = list("nutrient_demand_1"),
                           "nutrient-demand-tabs-2" = list("nutrient_demand_2"))
    
    includeHTML(paste0("./text/02-global-national-outlook/", selected_tab[[1]], ".html"))
    
  })
  
  # Plot output: Tab 1
  output$nutrient_demand_plot_1 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 2 
  output$nutrient_demand_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  ### Marine Seafood as a Source of Nutrients ---------------------
  
  # Reactive text: Based upon selected tab
  output$marine_seafood_as_source_text <- renderUI({
    
    selected_tab <- switch(input$marine_seafood_as_source_tabs,
                           "marine-seafood-as-source-tabs-1" = list("marine_seafood_as_source_1"),
                           "marine-seafood-as-source-tabs-2" = list("marine_seafood_as_source_2"),
                           "marine-seafood-as-source-tabs-3" = list("marine_seafood_as_source_3"))
    
    includeHTML(paste0("./text/02-global-national-outlook/", selected_tab[[1]], ".html"))
    
  })
  
  # Plot output: Tab 1
  output$marine_seafood_as_source_plot_1 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 2 
  output$marine_seafood_as_source_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 3
  output$marine_seafood_as_source_plot_3 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  ### Future Seafood Supply & Nutrient Contributions ---------------------
  
  # Reactive text: Based upon selected tab
  output$future_seafood_supply_text <- renderUI({
    
    selected_tab <- switch(input$future_seafood_supply_tabs,
                           "future-seafood-supply-tabs-1" = list("future_seafood_supply_1"),
                           "future-seafood-supply-tabs-2" = list("future_seafood_supply_2"))
    
    includeHTML(paste0("./text/02-global-national-outlook/", selected_tab[[1]], ".html"))
    
  })
  
  # Plot output: Tab 1
  output$future_seafood_supply_plot_1 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 2 
  output$future_seafood_supply_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED
    
  })
  
  ### -------------------------------------------
  ### national-nutrition-data Tab ---------------
  ### -------------------------------------------
  
  ### Protein Intake ---------------------
  
  # Reactive text: Based upon selected tab
  output$protein_intake_text <- renderUI({
    
    selected_tab <- switch(input$protein_intake_tabs,
                           "protein-intake-tabs-1" = list("protein_intake_1"),
                           "protein-intake-tabs-2" = list("protein_intake_2"))
    
    includeHTML(paste0("./text/03-national-nutrition-data/", selected_tab[[1]], ".html"))
    
  })
  
  # Plot output: Tab 1
  output$protein_intake_plot_1 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 2 
  output$protein_intake_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    ### NEED
    
  })
  
  ### Seafood Consumption & Nutrient Contributions ---------------------
  
  # Reactive text: Based upon selected tab
  output$seafood_consumption_text <- renderUI({
    
    selected_tab <- switch(input$seafood_consumption_tabs,
                           "seafood-consumption-tabs-1" = list("seafood_consumption_1"),
                           "seafood-consumption-tabs-2" = list("seafood_consumption_2"),
                           "seafood-consumption-tabs-3" = list("seafood_consumption_3"))
    
    includeHTML(paste0("./text/03-national-nutrition-data/", selected_tab[[1]], ".html"))
    
  })
  
  # Plot output: Tab 1
  output$seafood_consumption_plot_1 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 2 
  output$seafood_consumption_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    ### NEED
    
  })
  
  # Plot output: Tab 3
  output$seafood_consumption_plot_3 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    ### NEED
    
  })
  
  ### Nutritional Health ---------------------
  
  # Plot output
  output$nutritional_health_plot <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    ### NEED
    
  })
  
  ### Nutrient Consumption Profiles ---------------------
  
  # Plot output
  output$nutrient_consumption_profiles_plot <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    req(input$w_national_nutrition_data_nutrient)
    
    ### NEED
    
  })
  
  ### -----------------------------------
  ### seafood-reforms Tab ---------------
  ### -----------------------------------
  
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



