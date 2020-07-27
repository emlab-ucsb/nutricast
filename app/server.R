### --------------------------------------
### --------------------------------------
### This script defines the server
### --------------------------------------
### --------------------------------------

shinyServer(function(input, output, session) {
  
    ### ----------------------------------
    ### aqua-potential Tab ---------------
    ### ----------------------------------
  
    ### Historical production plot ---------
    output$historical_production_plot <- renderPlot({
      
      req(input$aqua_potential_select_country)
      
      # filter data 
      plot_dat <- production_current_group_dat %>%
        dplyr::filter(sov1_name == input$aqua_potential_select_country)
      
      # plot
      plot <- ggplot(plot_dat, aes(x = year, y = quantity_mt/1e6, fill = isscaap))+
        geom_area()+
        theme_bw()+
        labs(x = "Year", y = "Production (million mt)", fill = "Commercial group")
      
      plot
      
    })
    
    ### Future production plot ---------
    output$future_production_plot <- renderPlot({
      
      req(input$aqua_potential_select_country)
      
      # filter data 
      plot_dat <- production_all_sovereign_dat %>%
        dplyr::filter(sov1_name == input$aqua_potential_select_country)
      
      # plot
      plot <- ggplot(plot_dat, aes(x = year, y = quantity_mt/1e6, color = interaction(feed_scen,dev_pattern, sep=" - ")))+
        geom_line()+
        theme_bw()+
        labs(x = "Year", y = "Production (million mt)", color = "Scenario (Feed - Development)")+
        facet_grid(group ~ rcp, scales = "free")+
        theme(legend.position = "right")
      
      plot
      
    })
    
    ### ----------------------------------
    ### aqua-species Tab ---------------
    ### ----------------------------------
    
    ### Reactive data set based on selected country and climate scenario
    eez_species_dat <- eventReactive(c(input$aqua_species_select_country,
                                       input$aqua_species_select_scenario), {
                                         
                                         plot_dat <- rcp_projections %>%
                                           dplyr::filter(sov1_name == input$aqua_species_select_country & scenario == input$aqua_species_select_scenario) %>%
                                           dplyr::filter(year == 2100) %>%
                                           mutate(rank = dense_rank(desc(prod_mt_yr))) %>%
                                           mutate(species = fct_reorder(species, desc(rank)))                               
                                       })
    
    ### Future production plot ---------
    output$future_species_production_plot <- renderPlot({
      
      req(nrow(eez_species_dat()) > 0)
      
      # plot
      plot <- ggplot(eez_species_dat(), aes(x = species, y = prod_mt_yr/1e6, fill = group))+
        geom_bar(stat = "identity")+
        theme_bw()+
        labs(x = "", y = "Production (million mt)", fill = "Species Type")+
        coord_flip()+
        facet_wrap(~ group, scales = "free", ncol = 2)+
        theme(legend.position = "none")

      plot
      
    })
    
    ### Update species select input based on country and RCP selected above ---------
    observe({
      
      # Viable Species ordered 
      viable_species_ordered <- eez_species_dat() %>%
        dplyr::select(species, rank)
      
      # Only allow species that are viable in 2100 to be selected
      viable_species <- nutrient_dat_pmax %>%
        inner_join(viable_species_ordered, by = "species") %>%
        mutate(species = fct_reorder(species, rank))
      
      # Update input
      updateSelectizeInput(session, 
                           "aqua_species_select_species",
                           choices = levels(viable_species$species))
    })
    
    ### Nutrition radar plot for selected species (no more than 10) ---------
    output$future_species_nutrition <- renderPlot({
      
      req(nrow(eez_species_dat()) > 0,
          length(input$aqua_species_select_species) > 0)
      
      # Subset data
      plot_data <- nutrient_dat_pmax %>% 
        filter(species %in% input$aqua_species_select_species)
      
      req(nrow(plot_data) > 0)
      
      # Plot data
      g <- ggradar(plot_data) + 
        theme(legend.position = "right")
      g
      
    })
    
    ### ----------------------------------
    ### aqua-explorer Tab ---------------
    ### ----------------------------------
    
    ### Reactive data set based on selected country and climate scenario ---
    aqua_explorer_dat <- eventReactive(c(input$aqua_explorer_select_species,
                                       input$aqua_explorer_select_scenario,
                                       input$aqua_explorer_select_year), {
                                         
                                         plot_dat <- rcp_projections %>%
                                           dplyr::filter(species == input$aqua_species_select_species & scenario == input$aqua_species_select_scenario & year == input$aqua_explorer_select_year)                  
                                       })
    
    ### Update species select input based on year and RCP selected  ---------
    # observe({
    #   
    #   # Viable Species ordered 
    #   possible_species <- eez_species_dat() %>%
    #     dplyr::select(species, rank)
    #   
    #   # Only allow species that are viable in 2100 to be selected
    #   viable_species <- nutrient_dat_pmax %>%
    #     inner_join(viable_species_ordered, by = "species") %>%
    #     mutate(species = fct_reorder(species, rank))
    #   
    #   # Update input
    #   updateSelectizeInput(session, 
    #                        "aqua_species_select_species",
    #                        choices = levels(viable_species$species))
    # })
    
    ### Production map for selected species ---------
    output$future_species_production_map <- renderPlot({
      
      req(nrow(aqua_explorer_dat()) > 0)
      
      browser()
      # Subset data
      
      # Plot data
      # g <- ggradar(plot_data) + 
      #   theme(legend.position = "right")
      # g
      
    })
    

})



