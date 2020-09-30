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
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      plot_dat_hist <- pop_dat %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & source == "World Bank historical")
      
      plot_dat_proj <- pop_dat  %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & source == "UN WPP projections")
      
    }else{
      
      plot_dat_hist <- pop_dat %>%
        dplyr::filter(iso3 == "Global" & source == "World Bank historical")
      
      plot_dat_proj <- pop_dat %>%
        dplyr::filter(iso3 == "Global" & source == "UN WPP projections")
      
    }
    
    req(nrow(plot_dat_hist) > 0)
    req(nrow(plot_dat_proj) > 0)
      
      # Plot data
      g <- ggplot() +
        geom_line(data=plot_dat_hist, mapping=aes(x=year, y=pop_size_50perc/1e6)) +
        geom_ribbon(data=plot_dat_proj, mapping=aes(x=year, ymin=pop_size_05perc/1e6, ymax=pop_size_95perc/1e6), alpha=0.2, fill="red") +
        geom_line(data=plot_dat_proj, mapping=aes(x=year, y=pop_size_50perc/1e6), color="red") +
        labs(x="", y="Population size\n(millions of people)") +
        scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20)) +
        plot_theme+
        theme(axis.title.x = element_blank())
      
      g
    
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
    req(input$w_nutrient_demand_plot_1)
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      plot_dat <- nutrient_deficiency_dat %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country)
      
    }else {
      
      plot_dat <- nutrient_deficiency_dat %>%
        dplyr::filter(iso3 == "Global")
      
    }
    
    req(nrow(plot_dat) > 0)
    
    # Get plotting variable and plot
    plot_variable <- switch(input$w_nutrient_demand_plot_1,
                            "% of population" = list("ppeople", "% of Population"),
                            "persons" = list("npeople", "People (millions)"))
    
      # Plot
      g <- ggplot(plot_dat) +
        aes(y=get(plot_variable[[1]]), x=reorder(age, desc(age)), fill=sex, alpha=type) +
        # By nutrient
        facet_wrap(~nutrient, ncol=5) +
        # Plot bars
        geom_bar(stat="identity") +
        geom_hline(yintercept = 0) +
        # Flip axis
        coord_flip() +
        # Labels
        labs(y=plot_variable[[2]], x="Age Range") +
        # Legends
        scale_fill_discrete(name="Sex") +
        scale_alpha_manual(name="Nutritional Health", values=c(0.4, 1.0)) +
        # Theme
        plot_theme+
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
        theme(legend.position = "top",
              legend.direction = "vertical",
              legend.box = "horizontal")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
      g
      
  })
  
  # Plot output: Tab 2 
  output$nutrient_demand_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    req(input$w_nutrient_demand_plot_2)
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      plot_dat_hist <- nutrient_demand_dat %>% 
        filter(type=="Historical" & iso3==input$w_global_national_outlook_country & nutrient==input$w_nutrient_demand_plot_2)
      
      plot_dat_proj <- nutrient_demand_dat %>% 
        filter(type=="UN-WPP projections" & iso3==input$w_global_national_outlook_country & nutrient==input$w_nutrient_demand_plot_2)
      
    }else{
      
      plot_dat_hist <- nutrient_demand_dat %>% 
        filter(type=="Historical" & iso3=="Global" & nutrient==input$w_nutrient_demand_plot_2)
      
      plot_dat_proj <- nutrient_demand_dat %>% 
        filter(type=="UN-WPP projections" & iso3=="Global" & nutrient==input$w_nutrient_demand_plot_2)
      
    }
    
    req(nrow(plot_dat_hist) > 0 | nrow(plot_dat_proj) > 0)

    # Plot data
    g <- ggplot() +
      geom_line(data=plot_dat_hist, mapping=aes(x=year, y=supply_req_mt_yr_50perc)) +
      geom_ribbon(data=plot_dat_proj, mapping=aes(x=year, ymin=supply_req_mt_yr_05perc, ymax=supply_req_mt_yr_95perc), alpha=0.2, fill="red") +
      geom_line(data=plot_dat_proj, mapping=aes(x=year, y=supply_req_mt_yr_50perc), color="red") +
      labs(x="", y="Annual nutrient supply (Mg)\nrequired to eliminate deficiencies") +
      scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20)) +
      plot_theme+
      theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))+
      theme(axis.title.x = element_blank())
    
    g
    
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
    ### [EVENTUALLY ADD LANDINGS/EDIBLE MEAT TOGGLE]
    
    if(input$w_global_national_outlook_resolution == 'National'){
      
      plot_dat <- production_plot_dat %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & prod_type == "Landings")
      
    }else{
      
      plot_dat <- production_plot_dat %>%
        dplyr::filter(iso3 == "Global" & prod_type == "Landings")
      
    }
    
    req(nrow(plot_dat) > 0)
    
    g <- ggplot(plot_dat)+
      aes(x = year, y = prod_mt/1e6, fill = plot_group, alpha = source)+
      geom_area()+
      plot_theme +
      labs(y = "Production (million mt)")+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      scale_fill_discrete(name="Species Type") +
      scale_alpha_manual(name="Production Type", values=c(0.4, 1.0)) +
      theme(axis.title.x = element_blank())+
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
      theme(legend.position = "top",
            legend.direction = "vertical",
            legend.box = "horizontal")
    
    g
    
  })
  
  # Plot output: Tab 2 
  output$marine_seafood_as_source_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    # Add protein data to map
    world_data <- world %>% 
      left_join(protein_dat, by=c("adm0_a3"="iso3"))
    
    req(nrow(world_data) > 0)
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      selected_country_point <- world_points %>%
        dplyr::filter(adm0_a3 == input$w_global_national_outlook_country) %>%
        left_join(protein_dat, by=c("adm0_a3"="iso3"))
      
      req(nrow(selected_country_point) > 0)
      
      selected_country_prop <- protein_dat %>%
        filter(iso3==input$w_global_national_outlook_country) %>%
        pull(prop_seafood)
      
      req(!is.nan(selected_country_prop))
      
      # Plot map
      g1 <- ggplot(world_data) +
        geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
        geom_sf(data=selected_country_point, color="red", size=3) +
        scale_fill_gradientn(name="% of Protein From Marine Seafood", 
                             colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                             na.value = "grey70",
                             limits=c(0,10),
                             breaks=seq(0,10,5), 
                             labels=c("0%", "5%", ">10%")) +
        guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
        map_theme+
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.box = "horizontal")+
        guides(fill = guide_colorbar(title.position = "top", barwidth = 25, ticks.colour = "black", frame.colour = "black", title.hjust = 0.5))
      
      # Plot p(protein) histogram
      g2 <- ggplot(protein_dat, aes(x=prop_seafood)) +
        geom_histogram(binwidth = 0.01) +
        geom_vline(xintercept = selected_country_prop, col="red") +
        labs(x="Percent of Protein From Marine Seafood", y="Number of Countries") +
        scale_x_continuous(labels = scales::percent_format(accuracy=1), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0))+
        plot_theme
      
      # Merge
      g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths = c(1.75,1))
      g
      
    }else{
      
      # Plot map
      g1 <- ggplot(world_data) +
        geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
        scale_fill_gradientn(name="% of Protein From Marine Seafood", 
                             colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                             na.value = "grey70",
                             limits=c(0,10),
                             breaks=seq(0,10,5), 
                             labels=c("0%", "5%", ">10%")) +
        guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
        map_theme+
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.box = "horizontal")+
        guides(fill = guide_colorbar(title.position = "top", barwidth = 15))
      
      
      # Plot p(protein) histogram
      g2 <- ggplot(protein_dat, aes(x=prop_seafood)) +
        geom_histogram(binwidth = 0.01) +
        #geom_vline(xintercept = cntry_prop, col="red") +
        #annotate(geom="text", y=25, x=cntry_prop+0.01, hjust=0, label=cntry, color="red") +
        labs(x="Percent of Protein From Marine Seafood", y="Number of Countries") +
        scale_x_continuous(labels = scales::percent_format(accuracy=1), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0))+
        plot_theme
      
      # Merge
      g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths = c(1.75,1))

      g
      
    }

  })
  
  # Plot output: Tab 3
  output$marine_seafood_as_source_plot_3 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    # Data
    diet_data <- national_diet_from_seafood_dat %>% 
      filter(iso3==input$w_global_national_outlook_country) %>% 
      mutate(seafood_g_person_day = ifelse(total_g_person_day==0, NA, seafood_g_person_day))
    
    req(nrow(diet_data) > 0)
    
    # Plot % of diet from seafood over time
    g1 <- ggplot(diet_data, aes(x=year, y=seafood_g_person_day)) +
      geom_line() +
      labs(y="Daily per Capita Seafood Consumption\n(grams / person / day)") +
      # Theme
      plot_theme+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    # Plot % of diet from seafood over time
    g2 <- ggplot(diet_data, aes(x=year, y=prop_seafood)) +
      geom_line() +
      # Labels
      scale_y_continuous(labels = scales::percent) +
      labs(y="Percent of Daily Diet\nFrom Marine Seafood") +
      # Theme
      plot_theme+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    # Data
    nutrient_data <- national_nutrient_from_seafood_dat %>% 
      filter(iso3==input$w_global_national_outlook_country)
    
    req(nrow(nutrient_data) > 0)
    
    # Plot % of nutrient consumption from seafood in 2011
    g3 <- ggplot(nutrient_data, aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
      geom_bar(stat="identity") +
      coord_flip() +
      # Labels
      labs(y="Percent of Daily Nutrient Consumption\nFrom Marine Seafood") +
      scale_y_continuous(labels = scales::percent) +
      # Theme
      plot_theme+
      theme(axis.title.y=element_blank())
    
    # Merge
    g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(data=c(1,2, 3,3), byrow=F, ncol=2), widths=c(0.45, 0.55))
    g
    
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
    
    ### NEED THESE PLOTS
    
  })
  
  # Plot output: Tab 2 
  output$future_seafood_supply_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    ### NEED THESE PLOTS
    
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
    
    # Add country level protein data to map
    world_data <- world %>% 
      left_join(protein_dat, by=c("adm0_a3"="iso3"))
    
    req(nrow(world_data) > 0)
    
    # Get point for selected country
    selected_country_point <- world_points %>%
      dplyr::filter(adm0_a3 == input$w_national_nutrition_data_country) %>%
      left_join(protein_dat, by=c("adm0_a3"="iso3"))
    
    #req(nrow(selected_country_point) > 0)
    
    # Plot map
    g <- ggplot(world_data) +
      geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
      geom_sf(data=selected_country_point, color="red", size=3) +
      scale_fill_gradientn(name="% of Protein From Marine Seafood", 
                           colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                           na.value = "grey70",
                           limits=c(0,10),
                           breaks=seq(0,10,5), 
                           labels=c("0%", "5%", ">10%")) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      map_theme+
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.box = "horizontal")+
      guides(fill = guide_colorbar(title.position = "top", barwidth = 25, ticks.colour = "black", frame.colour = "black", title.hjust = 0.5))
    
    g
    
  })
  
  # Plot output: Tab 2 
  output$protein_intake_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    selected_country_prop <- protein_dat %>%
      filter(iso3==input$w_national_nutrition_data_country) %>%
      pull(prop_seafood)
    
    # Plot p(protein) histogram
    g <- ggplot(protein_dat, aes(x=prop_seafood)) +
      geom_histogram(binwidth = 0.01) +
      geom_vline(xintercept = selected_country_prop, col="red") +
      labs(x="Percent of Protein From Marine Seafood", y="Number of Countries") +
      scale_x_continuous(labels = scales::percent_format(accuracy=1), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))+
      plot_theme+
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
    
    g

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
    
    # Data
    diet_data <- national_diet_from_seafood_dat %>% 
      filter(iso3==input$w_national_nutrition_data_country) %>% 
      mutate(seafood_g_person_day = ifelse(total_g_person_day==0, NA, seafood_g_person_day)) %>%
      dplyr::filter(!is.na(seafood_g_person_day))
    
    req(nrow(diet_data) > 0)
    
    # Plot % of diet from seafood over time
    g <- ggplot(diet_data, aes(x=year, y=seafood_g_person_day)) +
      geom_line() +
      labs(y="Daily per Capita Seafood Consumption\n(grams / person / day)") +
      # Theme
      plot_theme+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    g
    
  })
  
  # Plot output: Tab 2 
  output$seafood_consumption_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    # Data
    diet_data <- national_diet_from_seafood_dat %>% 
      filter(iso3==input$w_national_nutrition_data_country) %>% 
      mutate(seafood_g_person_day = ifelse(total_g_person_day==0, NA, seafood_g_person_day)) %>%
      dplyr::filter(!is.na(seafood_g_person_day))
    
    req(nrow(diet_data) > 0)
    
    # Plot % of diet from seafood over time
    g <- ggplot(diet_data, aes(x=year, y=prop_seafood)) +
      geom_line() +
      # Labels
      scale_y_continuous(labels = scales::percent) +
      labs(y="Percent of Daily Diet\nFrom Marine Seafood") +
      # Theme
      plot_theme+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    g
    
  })
  
  # Plot output: Tab 3
  output$seafood_consumption_plot_3 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    # Data
    nutrient_data <- national_nutrient_from_seafood_dat %>% 
      dplyr::filter(nutrient != "Sodium") %>%
      filter(iso3==input$w_national_nutrition_data_country)
    
    req(nrow(nutrient_data) > 0)
    
    # Plot % of nutrient consumption from seafood in 2011
    g <- ggplot(nutrient_data, aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
      geom_bar(stat="identity") +
      coord_flip() +
      # Labels
      labs(y="Percent of Daily Nutrient Consumption\nFrom Marine Seafood") +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      # Theme
      plot_theme+
      theme(axis.title.y=element_blank())
    
    g

  })
  
  ### Nutritional Health ---------------------
  
  # Plot output
  output$nutritional_health_plot <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    # Subset and format data
    plot_data <- nutritional_health_plot_dat %>% 
      filter(iso3==input$w_national_nutrition_data_country & !is.na(diet_req))
    
    req(nrow(plot_data) > 0)
    
    # Plot data
    g <- ggplot(plot_data, aes(x=age_range, y=nutrient, fill=value_perc_req_cap)) +
      facet_grid(nutrient_type ~ sex, scale="free", space="free") +
      geom_raster() +
      # Labels
      labs(x="", y="") +
      scale_fill_gradient2(name="Percent Above or Below Daily Recommendation", 
                           midpoint = 0,
                           limits = c(-100,100)) +
      # Theme
      plot_theme+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.title = element_blank())+
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.box = "horizontal")+
      guides(fill = guide_colorbar(title.position = "top", barwidth = 25, ticks.colour = "black", frame.colour = "black", title.hjust = 0.5))
      
    g
    
  })
  
  ### Nutrient Consumption Profiles ---------------------
  
  # Plot output
  output$nutrient_consumption_profiles_plot <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    req(input$w_national_nutrition_data_nutrient)
    
    # Subset data
    plot_dat_1 <- national_nutrient_supplies_dat %>% 
      filter(iso3==input$w_national_nutrition_data_country & nutrient==input$w_national_nutrition_data_nutrient)
    
    plot_dat_2 <- nutritional_health_plot_dat %>% 
      filter(iso3==input$w_national_nutrition_data_country & nutrient==input$w_national_nutrition_data_nutrient)
    
    # DRI data 
    plot_dri_dat <- dri_dat %>%
      filter(nutrient==input$w_national_nutrition_data_nutrient & dri_type=="Estimated Average Requirement (EAR)") %>% 
      arrange(sex, age_range)
    
    # Maximum value
    max1 <- max(plot_dat_1$value_hi, na.rm=T)
    max2 <- max(plot_dat_2$value_hi, na.rm=T)
    max_dri <- max(plot_dri_dat$value)
    max_val <- max(max1, max2, max_dri)
    
    # Extract unit
    nutr_unit <- unique(plot_dat_1$units_short)

    # Time Series Plot
    g1 <- ggplot(plot_dat_1, aes(x=year, y=value_med)) +
      geom_line() +
      geom_ribbon(aes(ymin=value_lo, ymax=value_hi), alpha=0.2) +
      # Y-axis limits
      ylim(0, max_val) +
      # Labels
      labs(x="Year", y=paste0("Daily Per Capita Consumption\n(", nutr_unit, " / person / day)")) +
      plot_theme+
      scale_x_continuous(expand = c(0,0))
    
    # Sex plot
    g2 <- ggplot(plot_dat_2, aes(x=age_range, y=value_med, fill=sex)) +
      facet_grid(~sex, scale="free_x", space="free") +
      # Plot bars
      geom_bar(stat="identity", position="dodge", alpha=0.6, show.legend = F) +
      # Add error bars
      geom_errorbar(mapping=aes(x=age_range, ymin=value_lo, ymax=value_hi, color=sex),
                    width=0, lwd=1, show.legend = F) +
      # Add diet requirement
      geom_line(data=plot_dri_dat, mapping=aes(x=age_range, y=value, group=sex), inherit.aes = F, color="black") +
      geom_point(data=plot_dri_dat, mapping=aes(x=age_range, y=value, group=sex), inherit.aes = F, color="black") +
      # Labels
      labs(x="Age Range", y="") +
      scale_fill_discrete(name="Sex") +
      # Theme
      plot_theme+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())+
      scale_y_continuous(expand = c(0,0), limits = c(0, max_val))
    
    # Combine
    g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.45, 0.55))
    g
      
  })
  
  ### -----------------------------------
  ### seafood-reforms Tab ---------------
  ### -----------------------------------
  
  ### Seafood Nutrition Content -----------------
  
  # Plot output 
  output$seafood_nutrition_content_plot <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    
    # # Subset species data
    # plot_data <- vaitla_nutr_preds_long %>% 
    #   filter(species_label %in% spp)
    
    # Plot
    g <- ggplot(vaitla_nutr_preds_long, aes(x=value_md)) +
      geom_histogram(fill="grey70") +
      facet_wrap(~nutrient_label, ncol=3, scales="free") +
      # Add species lines
      #geom_vline(data=sdata, aes(xintercept=value_md, color=species_label), lwd=1.5) +
      # Labels
      scale_color_discrete(name="") +
      labs(x="Nutrient Concentration", y="Number of Species") +
      # Theme
      plot_theme+
      scale_y_continuous(expand = c(0,0))
    
    g

  })
  
  ### Fisheries Reforms ---------------
  
  # Reactive text: Based upon selected tab
  output$fisheries_reforms_text <- renderUI({
    
    selected_tab <- switch(input$fisheries_reforms_tabs,
                           "fisheries-reforms-tabs-1" = list("fisheries_reforms_1"),
                           "fisheries-reforms-tabs-2" = list("fisheries_reforms_2"),
                           "fisheries-reforms-tabs-3" = list("fisheries_reforms_2"))
    
    includeHTML(paste0("./text/04-seafood-reforms/", selected_tab[[1]], ".html"))
    
  })
  
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
  
  # Reactive text: Based upon selected tab
  output$aquaculture_reforms_text <- renderUI({
    
    selected_tab <- switch(input$aquaculture_reforms_tabs,
                           "aquaculture-reforms-tabs-1" = list("aquaculture_reforms_1"),
                           "aquaculture-reforms-tabs-2" = list("aquaculture_reforms_2"))
    
    includeHTML(paste0("./text/04-seafood-reforms/", selected_tab[[1]], ".html"))
    
  })
  
  # Make reactive data object because it's called in multiple places
  aquaculture_reforms_dat <- eventReactive(c(input$w_seafood_reforms_country,
                                             input$w_seafood_reforms_aquaculture_climate_scenario), {
    
    out <- rcp_projections %>%
      dplyr::filter(sov1_iso == input$w_seafood_reforms_country & scenario == input$w_seafood_reforms_aquaculture_climate_scenario) %>%
      dplyr::filter(year == 2100) %>%
      dplyr::filter(prod_mt_yr > 0) %>%
      mutate(rank = dense_rank(desc(prod_mt_yr))) %>%
      mutate(species = fct_reorder(species, desc(rank)))
    
  })
  
  # Plot output
  output$aquaculture_reforms_plot_1 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    req(input$w_seafood_reforms_aquaculture_climate_scenario)
    
    plot_dat <- aquaculture_reforms_dat()

    req(nrow(plot_dat) > 0)
    
    g <-ggplot(plot_dat)+
      aes(x = species, y = prod_mt_yr/1e6, fill = group) +
      geom_bar(stat = "identity") +
      theme_bw() +
      labs(x = "", y = "Production (million mt)", fill = "Species Type") +
      coord_flip() +
      facet_wrap( ~ group, scales = "free", ncol = 2) +
      scale_y_continuous(expand = c(0,0))+
      plot_theme+
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
      theme(legend.position = "none")
      

    g
    
  })
  
  ### Update species select input based on country and RCP selected above ---------
  observe({

    # Viable Species ordered
    viable_species_ordered <- aquaculture_reforms_dat() %>%
      dplyr::select(species, rank)

    # Only allow species that are viable in 2100 to be selected
    viable_species <- nutrient_dat_pmax %>%
      inner_join(viable_species_ordered, by = "species") %>%
      mutate(species = fct_reorder(species, rank))

    # Update input
    updateSelectizeInput(session,
                         "w_seafood_reforms_radar_species",
                         choices = levels(viable_species$species))
    
    # Update input
    updateSelectizeInput(session,
                         "w_seafood_reforms_site_explorer_species",
                         choices = levels(viable_species$species))
    
  })
  
  # Plot output
  output$aquaculture_reforms_plot_2 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    req(input$w_seafood_reforms_aquaculture_climate_scenario)
    req(input$w_seafood_reforms_radar_species)
    
    req(nrow(aquaculture_reforms_dat()) > 0,
        length(input$w_seafood_reforms_radar_species) > 0)

    # Subset data
    plot_data <- nutrient_dat_pmax %>%
      filter(species %in% input$w_seafood_reforms_radar_species)
    
    req(nrow(plot_data) > 0)
    
    # Plot data
    g <- ggradar(plot_data) +
      theme(legend.position = "right")
    g
    
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



