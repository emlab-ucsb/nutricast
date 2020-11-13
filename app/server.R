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
      
      plot_dat_hist <- population_growth_plot_data %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & source == "World Bank historical")
      
      plot_dat_proj <- population_growth_plot_data  %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & source == "UN WPP projections")
      
      plot_div <- 1e6
      plot_axis_legend <- "Population size\n(millions of people)"
      
    }else{
      
      plot_dat_hist <- population_growth_plot_data %>%
        dplyr::filter(iso3 == "Global" & source == "World Bank historical")
      
      plot_dat_proj <- population_growth_plot_data %>%
        dplyr::filter(iso3 == "Global" & source == "UN WPP projections")
      
      plot_div <- 1e9
      plot_axis_legend <- "Population size\n(billions of people)"
      
    }
    
    req(nrow(plot_dat_hist) > 0)
    req(nrow(plot_dat_proj) > 0)
      
      # Plot data
      g <- ggplot() +
        geom_line(data=plot_dat_hist, mapping=aes(x=year, y=pop_size_50perc/plot_div)) +
        geom_ribbon(data=plot_dat_proj, mapping=aes(x=year, ymin=pop_size_05perc/plot_div, ymax=pop_size_95perc/plot_div), alpha=0.2, fill="red") +
        geom_line(data=plot_dat_proj, mapping=aes(x=year, y=pop_size_50perc/plot_div), color="red") +
        labs(x="", y=plot_axis_legend) +
        scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20), expand = c(0,0)) +
        plot_theme+
        theme(axis.title.x = element_blank())+
        theme(plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"))
      
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
      
      plot_dat <- nutrient_demand_plot_1_data %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country)
      
    }else {
      
      plot_dat <- nutrient_demand_plot_1_data %>%
        dplyr::filter(iso3 == "Global")
      
    }
    
    req(nrow(plot_dat) > 0)
    
    # Get plotting variable and plot
    plot_variable <- switch(input$w_nutrient_demand_plot_1,
                            "% of population" = list("ppeople", "% of group (age-sex)", 1),
                            "number of people" = list("npeople", "People (millions)", 1e6))
    
      # Plot
      g <- ggplot(plot_dat) +
        aes(y=get(plot_variable[[1]])/plot_variable[[3]], x=reorder(age, desc(age)), fill=sex, alpha=type) +
        # By nutrient
        facet_wrap(~nutrient, ncol=5) +
        # Plot bars
        geom_bar(stat="identity") +
        geom_hline(yintercept = 0) +
        # Flip axis
        coord_flip() +
        # Labels
        labs(y=plot_variable[[2]], x="Age range") +
        # Legends
        scale_fill_discrete(name="Sex") +
        scale_alpha_manual(name="Nutritional health", values=c(0.4, 1.0)) +
        scale_y_continuous(breaks = pretty(plot_dat[[plot_variable[[1]]]]), labels = abs(pretty(plot_dat[[plot_variable[[1]]]]))) +
        # Theme
        plot_theme_tab+
        theme(legend.position = "top",
              legend.direction = "vertical",
              legend.box = "horizontal",
              legend.justification = "left")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
      g
      
  })
  
  # Plot output: Tab 2 
  output$nutrient_demand_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    req(input$w_nutrient_demand_plot_2)
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      plot_dat_hist <- nutrient_demand_plot_2_data %>% 
        filter(type=="Historical" & iso3==input$w_global_national_outlook_country & nutrient==input$w_nutrient_demand_plot_2)
      
      plot_dat_proj <- nutrient_demand_plot_2_data %>% 
        filter(type=="UN-WPP projections" & iso3==input$w_global_national_outlook_country & nutrient==input$w_nutrient_demand_plot_2)
      
      plot_fac <- 1
      plot_axis_title <- "Nutrient supply required to eliminate deficiencies\n(Mg per year)"
      
    }else{
      
      plot_dat_hist <- nutrient_demand_plot_2_data %>% 
        filter(type=="Historical" & iso3=="Global" & nutrient==input$w_nutrient_demand_plot_2)
      
      plot_dat_proj <- nutrient_demand_plot_2_data %>% 
        filter(type=="UN-WPP projections" & iso3=="Global" & nutrient==input$w_nutrient_demand_plot_2)
      
      plot_fac <- 1000
      plot_axis_title <- "Nutrient supply required to eliminate deficiencies\n(thousands of Mg per year)"
      
    }
    
    req(nrow(plot_dat_hist) > 0 | nrow(plot_dat_proj) > 0)
    
    # Plot data
    g <- ggplot() +
      geom_line(data=plot_dat_hist, mapping=aes(x=year, y=supply_req_mt_yr_50perc/plot_fac)) +
      geom_ribbon(data=plot_dat_proj, mapping=aes(x=year, ymin=supply_req_mt_yr_05perc/plot_fac, ymax=supply_req_mt_yr_95perc/plot_fac), alpha=0.2, fill="red") +
      geom_line(data=plot_dat_proj, mapping=aes(x=year, y=supply_req_mt_yr_50perc/plot_fac), color="red") +
      labs(x="", y=plot_axis_title) +
      scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20), expand = c(0,0)) +
      plot_theme_tab+
      theme(plot.margin = unit(c(3, 0.8, 0.8, 0.8), "cm"))+
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
    req(input$w_marine_seafood_as_source_plot_1)
    
    if(input$w_global_national_outlook_resolution == 'National'){
      
      plot_dat <- fisheries_mariculture_production_plot_data %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country)
      
    }else{
      
      plot_dat <- fisheries_mariculture_production_plot_data %>%
        dplyr::filter(iso3 == "Global")
      
    }
    
    req(nrow(plot_dat) > 0)
    
    plot_variable <- switch(input$w_marine_seafood_as_source_plot_1,
                            "live weight" = list("live_weight", 1e6, "Production\n(millions of mt)"),
                            "edible meat" = list("edible_meat", 1e6, "Production\n(millions of mt)"),
                            "edible meat per capita" = list("edible_meat_per_capita", 1, "Production\n(mt per person)"))
    
    g <- ggplot(plot_dat)+
      aes(x = year, y = get(plot_variable[[1]])/plot_variable[[2]], fill = plot_group, alpha = source)+
      geom_area()+
      labs(y = plot_variable[[3]])+
      scale_x_continuous(expand = c(0,0), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020))+
      scale_y_continuous(expand = c(0,0))+
      scale_fill_discrete(name="Species type") +
      scale_alpha_manual(name="Production type", values=c(0.4, 1.0)) +
      plot_theme_tab+
      theme(axis.title.x = element_blank())+
      theme(legend.position = "top",
            legend.direction = "vertical",
            legend.box = "horizontal",
            legend.justification = "left")
    
    g
    
  })
  
  # Plot output: Tab 2 
  output$marine_seafood_as_source_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    
    # Add protein data to map
    world_data <- world %>% 
      left_join(protein_from_seafood_plot_data, by=c("adm0_a3"="iso3"))
    
    req(nrow(world_data) > 0)
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      selected_country_point <- world_points %>%
        dplyr::filter(adm0_a3 == input$w_global_national_outlook_country) %>%
        left_join(protein_from_seafood_plot_data, by=c("adm0_a3"="iso3"))
      
      req(nrow(selected_country_point) > 0)
      
      selected_country_prop <- protein_from_seafood_plot_data %>%
        filter(iso3==input$w_global_national_outlook_country) %>%
        pull(prop_seafood)
      
      req(!is.nan(selected_country_prop))
      
      # Plot map
      g1 <- ggplot(world_data) +
        geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
        geom_sf(data=selected_country_point, color="red", size=3) +
        scale_fill_gradientn(name="% of protein from marine seafood", 
                             colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                             na.value = "grey70",
                             limits=c(0,10),
                             breaks=seq(0,10,5), 
                             labels=c("0%", "5%", ">10%")) +
        guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                                     title.position = "top", barwidth = 25, title.hjust = 0.5)) +        
        map_theme_tab+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.box = "horizontal")
      
      # Plot p(protein) histogram
      g2 <- ggplot(protein_from_seafood_plot_data, aes(x=prop_seafood)) +
        geom_histogram(binwidth = 0.01) +
        geom_vline(xintercept = selected_country_prop, col="red") +
        labs(x="% of protein from\nmarine seafood", y="Number of countries") +
        scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
        plot_theme_tab
      
      # Merge
      g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths = c(1.75,1))
      g
      
    }else{
      
      # Plot map
      g1 <- ggplot(world_data) +
        geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
        scale_fill_gradientn(name="% of protein from marine seafood", 
                             colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                             na.value = "grey70",
                             limits=c(0,10),
                             breaks=seq(0,10,5), 
                             labels=c("0%", "5%", ">10%")) +
        map_theme_tab+
        guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                                     title.position = "top", barwidth = 25, title.hjust = 0.5)) + 
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.box = "horizontal")
      
      
      # Plot p(protein) histogram
      g2 <- ggplot(protein_from_seafood_plot_data, aes(x=prop_seafood)) +
        geom_histogram(binwidth = 0.01) +
        labs(x="% of protein from\nmarine seafood", y="Number of countries") +
        scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
        plot_theme_tab
      
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
    diet_data <- seafood_consumption_plot_data_diet %>% 
      filter(iso3==input$w_global_national_outlook_country) %>% 
      mutate(seafood_g_person_day = ifelse(total_g_person_day==0, NA, seafood_g_person_day))
    
    req(nrow(diet_data) > 0)
    
    # Plot % of diet from seafood over time
    g1 <- ggplot(diet_data, aes(x=year, y=seafood_g_person_day)) +
      geom_line() +
      labs(y="Daily per capita seafood consumption\n(grams per person per day)") +
      plot_theme_tab+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))+
      scale_x_continuous(expand = c(0,0))
      
    
    # Plot % of diet from seafood over time
    g2 <- ggplot(diet_data, aes(x=year, y=prop_seafood)) +
      geom_line() +
      # Labels
      scale_y_continuous(labels = scales::percent) +
      labs(y="% of daily diet\nfrom marine seafood") +
      plot_theme_tab+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))+
      scale_x_continuous(expand = c(0,0))
    
    # Data
    nutrient_data <- seafood_consumption_plot_data_nutrients %>% 
      dplyr::filter(nutrient != "Sodium") %>%
      filter(iso3==input$w_global_national_outlook_country)
    
    req(nrow(nutrient_data) > 0)
    
    # Plot % of nutrient consumption from seafood in 2011
    g3 <- ggplot(nutrient_data, aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
      geom_bar(stat="identity") +
      coord_flip() +
      # Labels
      labs(y="% of daily nutrient\nconsumption from\nmarine seafood") +
      scale_y_continuous(labels = scales::percent) +
      plot_theme_tab+
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
    
    if(input$future_seafood_supply_tabs == "future-seafood-supply-tabs-1"){
      
      includeHTML(paste0("./text/02-global-national-outlook/", selected_tab[[1]], ".html"))
      
    }else{
      
      column(12,
             
             selectizeInput("w_future_seafood_supply_nutrient",
                            label = tags$b(text$item_label[text$item_id == "w_national_nutrition_data_nutrient"]),
                            choices = unique(forecasted_demand_filled_plot_dat$nutrient)),
             
             includeHTML(paste0("./text/02-global-national-outlook/", selected_tab[[1]], ".html"))
      )
    }

  })
  
  # Plot output: Tab 1
  output$future_seafood_supply_plot_1 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    req(input$w_future_seafood_supply_plot_1)
    
    if(input$w_global_national_outlook_resolution == "National"){
      
      historical_dat <- forecasted_seafood_supply_plot_data_historical %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country)
      
      projection_dat_bau <- forecasted_seafood_supply_plot_data_projections %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & dev_scenario == "Proportional") %>%
        dplyr::filter(year != 2020 & scenario == "Business-as-usual")
      
      projection_dat_reform <- forecasted_seafood_supply_plot_data_projections %>%
        dplyr::filter(iso3 == input$w_global_national_outlook_country & dev_scenario == "Proportional") %>%
        dplyr::filter(year != 2020 & scenario == "Progressive reforms")
      
    }else {
      
      historical_dat <- forecasted_seafood_supply_plot_data_historical %>%
        dplyr::filter(iso3 == "Global")
      
      projection_dat_bau <- forecasted_seafood_supply_plot_data_projections %>%
        dplyr::filter(iso3 == "Global" & dev_scenario == "Proportional") %>%
        dplyr::filter(year != 2020 & scenario == "Business-as-usual")
      
      projection_dat_reform <- forecasted_seafood_supply_plot_data_projections %>%
        dplyr::filter(iso3 == "Global" & dev_scenario == "Proportional") %>%
        dplyr::filter(year != 2020 & scenario == "Progressive reforms")

    }
    
    req(nrow(historical_dat) > 0 & (nrow(projection_dat_bau) > 0 | nrow(projection_dat_reform) > 0))
    
    plot_var <- switch(input$w_future_seafood_supply_plot_1,
                       "live weight" = list("live_weight", 1e6, "Total production (millions of mt)"),
                       "edible meat" = list("edible_meat", 1e6, "Total production (millions of mt)"),
                       "edible meat per capita" = list("edible_meat_per_capita", 1, "Total production (kg per capita)"))
    
    # Get max values for plotting
    historical_max <- historical_dat %>%
      group_by(year) %>%
      summarize(value = sum(get(plot_var[[1]]))/plot_var[[2]]) %>%
      pull(value) %>% max()
    
    projection_max <- projection_dat_reform %>%
      group_by(rcp, scenario, period) %>%
      summarize(value = sum(get(plot_var[[1]]))/plot_var[[2]]) %>%
      pull(value) %>% max()
    
    max_val <- max(historical_max, projection_max) * 1.02
    fill_pal <- c("Capture fisheries" = "#57606c", "Bivalve mariculture" = "#76B7B2", "Finfish mariculture" = "#B07AA1")    
    # Historical plot panel
    g1 <- ggplot(historical_dat, aes(x=year, y=get(plot_var[[1]])/plot_var[[2]], fill=sector)) +
      geom_area() +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      scale_x_continuous(lim=c(1960, 2020), breaks=seq(1960,2020,10)) +
      # Labels
      labs(x=" ", y= plot_var[[3]], title=" \nHistorical seafood production") +
      scale_fill_manual(name="Sector", values = fill_pal[names(fill_pal) %in% unique(historical_dat$sector)]) +
      plot_theme_tab +
      theme(legend.position = c(0.15, 1.15))+
      theme(plot.margin = unit(c(3, 0.1, 0.5, 0.5), "cm"))
    
    # BAU forecast plot panel
    g2 <- ggplot(projection_dat_bau, aes(x=rcp, y=get(plot_var[[1]])/plot_var[[2]], fill=sector)) +
      geom_bar(stat="identity") +
      facet_wrap(~year) +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a business-as-usual scenario") +
      geom_text(data=projection_dat_bau %>% distinct(scenario, year), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_manual(name="Sector", values = fill_pal[names(fill_pal) %in% unique(historical_dat$sector)]) +
      plot_theme_tab +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())+
      theme(plot.margin = unit(c(3, 0.1, 0.5, 0.1), "cm"))
    
    # Reform forecast plot panel
    g3 <- ggplot(projection_dat_reform, aes(x=rcp, y=get(plot_var[[1]])/plot_var[[2]], fill=sector)) +
      geom_bar(stat="identity") +
      facet_wrap(~year) +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a progressive reform scenario") +
      geom_text(data=projection_dat_reform %>% distinct(scenario, year), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_manual(name="Sector", values = fill_pal[names(fill_pal) %in% unique(historical_dat$sector)]) +
      plot_theme_tab +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())+
      theme(plot.margin = unit(c(3, 0.5, 0.5, 0.1), "cm"))

    # Merge plots
    g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))
    g
    
  })
  
  # Plot output: Tab 2 
  output$future_seafood_supply_plot_2 <- renderPlot({
    
    req(input$w_global_national_outlook_resolution)
    req(input$w_global_national_outlook_country)
    req(input$w_future_seafood_supply_nutrient)
    
    if(input$w_global_national_outlook_resolution == "Global"){
      
      plot_data <- forecasted_demand_filled_plot_dat %>% 
        dplyr::filter(iso3 == "Global" & nutrient == input$w_future_seafood_supply_nutrient)
      
    }else{
      
      plot_data <- forecasted_demand_filled_plot_dat %>% 
        dplyr::filter(input$w_global_national_outlook_country & nutrient == input$w_future_seafood_supply_nutrient)
      
    }
    
    req(nrow(plot_data) > 0)
    
    fill_pal <- c("Capture fisheries" = "#57606c", "Bivalve mariculture" = "#76B7B2", "Finfish mariculture" = "#B07AA1")
    
    # Plot data
    g <- ggplot(plot_data, aes(x=scenario, y=demand_prop, fill=sector)) +
      facet_grid(~year) +
      geom_bar(stat="identity") +
      # Labels
      labs(x="", y="% of demand met by marine seafood") +
      # Legend
      scale_fill_manual(name = "Sector", values = fill_pal[names(fill_pal) %in% unique(plot_data$sector)]) +
      plot_theme_tab +
      theme(legend.position = "top",
            legend.direction = "vertical",
            legend.box = "horizontal")
      
    g
    
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
      left_join(protein_from_seafood_plot_data, by=c("adm0_a3"="iso3"))
    
    req(nrow(world_data) > 0)
    
    # Get point for selected country
    selected_country_point <- world_points %>%
      dplyr::filter(adm0_a3 == input$w_national_nutrition_data_country) %>%
      left_join(protein_from_seafood_plot_data, by=c("adm0_a3"="iso3"))
    
    #req(nrow(selected_country_point) > 0)
    
    # Plot map
    g <- ggplot(world_data) +
      geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
      geom_sf(data=selected_country_point, color="red", size=3) +
      scale_fill_gradientn(name="% of protein from marine seafood", 
                           colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                           na.value = "grey70",
                           limits=c(0,10),
                           breaks=seq(0,10,5), 
                           labels=c("0%", "5%", ">10%")) +
      map_theme_tab+
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.box = "horizontal")+
      guides(fill = guide_colorbar(title.position = "top", barwidth = 25, ticks.colour = "black", frame.colour = "black", title.hjust = 0.5))
    
    g
    
  })
  
  # Plot output: Tab 2 
  output$protein_intake_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    selected_country_prop <- protein_from_seafood_plot_data %>%
      filter(iso3==input$w_national_nutrition_data_country) %>%
      pull(prop_seafood)
    
    # Plot p(protein) histogram
    g <- ggplot(protein_from_seafood_plot_data, aes(x=prop_seafood)) +
      geom_histogram(binwidth = 0.01) +
      geom_vline(xintercept = selected_country_prop, col="red") +
      labs(x="% of protein from marine seafood", y="Number of countries") +
      scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
      plot_theme_tab
    
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
    diet_data <- seafood_consumption_plot_data_diet %>% 
      filter(iso3==input$w_national_nutrition_data_country) %>% 
      mutate(seafood_g_person_day = ifelse(total_g_person_day==0, NA, seafood_g_person_day)) %>%
      dplyr::filter(!is.na(seafood_g_person_day))
    
    req(nrow(diet_data) > 0)
    
    # Plot % of diet from seafood over time
    g <- ggplot(diet_data, aes(x=year, y=seafood_g_person_day)) +
      geom_line() +
      labs(y="Daily per capita seafood consumption\n(grams per person per day)") +
      # Theme
      plot_theme_tab+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    g
    
  })
  
  # Plot output: Tab 2 
  output$seafood_consumption_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    # Data
    diet_data <- seafood_consumption_plot_data_diet %>% 
      filter(iso3==input$w_national_nutrition_data_country) %>% 
      mutate(seafood_g_person_day = ifelse(total_g_person_day==0, NA, seafood_g_person_day)) %>%
      dplyr::filter(!is.na(seafood_g_person_day))
    
    req(nrow(diet_data) > 0)
    
    # Plot % of diet from seafood over time
    g <- ggplot(diet_data, aes(x=year, y=prop_seafood)) +
      geom_line() +
      # Labels
      scale_y_continuous(labels = scales::percent) +
      labs(y="% of daily diet\nfrom marine seafood") +
      # Theme
      plot_theme_tab+
      theme(axis.title.x=element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    g
    
  })
  
  # Plot output: Tab 3
  output$seafood_consumption_plot_3 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    # Data
    nutrient_data <- seafood_consumption_plot_data_nutrients %>% 
      dplyr::filter(nutrient != "Sodium") %>%
      filter(iso3==input$w_national_nutrition_data_country)
    
    req(nrow(nutrient_data) > 0)
    
    # Plot % of nutrient consumption from seafood in 2011
    g <- ggplot(nutrient_data, aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
      geom_bar(stat="identity") +
      coord_flip() +
      # Labels
      labs(y="% of daily nutrient consumption\nfrom marine seafood") +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      # Theme
      plot_theme_tab+
      theme(axis.title.y=element_blank())
    
    g

  })
  
  ### Nutritional Health ---------------------
  
  # Plot output
  output$nutritional_health_plot <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    
    # Subset and format data
    plot_data <- nutritional_health_plot_dat %>%
      filter(iso3==input$w_national_nutrition_data_country)
    
    req(nrow(plot_data) > 0)
    
    # Plot data
    g <- ggplot(plot_data, aes(x=age, y=nutrient, fill=pdeficient*100)) +
      facet_grid(nutrient_type~sex, scales="free", space="free") +
      geom_raster() +
      # Legend
      scale_fill_gradientn(name="% of population nutrient deficient", 
                           colors=RColorBrewer::brewer.pal(9, "YlOrRd"), limits=c(0,100)) +
      # Labels
      labs(x="Age group", y="") +
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
  
  ### Info modal (on button click) ----------------------------
  observeEvent(input$info_nutrient_types, {
    
    showModal(modalDialog(title = NULL,
                          includeHTML(paste0("./text/03-national-nutrition-data/info_nutrient_types.html")),
                          size = "l",
                          easyClose = T))
    
  })
  
  # Plot output (top plot)
  output$nutrient_consumption_profiles_plot_1 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    req(input$w_national_nutrition_data_nutrient)
    
    # Subset data
    plot_dat_1 <- nutrient_consumption_plot_data_country_year %>% 
      filter(iso3==input$w_national_nutrition_data_country & nutrient==input$w_national_nutrition_data_nutrient)
    
    plot_dat_2 <- nutrient_consumption_plot_data_age_sex %>% 
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
      labs(x="Year", y=paste0("Daily per capita consumption\n(", nutr_unit, " per person per day)")) +
      plot_theme_tab+
      scale_x_continuous(expand = c(0,0))
    
    g1
    
  })
  
  output$nutrient_consumption_profiles_plot_2 <- renderPlot({
    
    req(input$w_national_nutrition_data_country)
    req(input$w_national_nutrition_data_nutrient)
    
    # Subset data
    plot_dat_1 <- nutrient_consumption_plot_data_country_year %>% 
      filter(iso3==input$w_national_nutrition_data_country & nutrient==input$w_national_nutrition_data_nutrient)
    
    plot_dat_2 <- nutrient_consumption_plot_data_age_sex %>% 
      filter(iso3==input$w_national_nutrition_data_country & nutrient==input$w_national_nutrition_data_nutrient)
    
    # DRI data 
    plot_dri_dat <- dri_dat %>%
      filter(nutrient==input$w_national_nutrition_data_nutrient & dri_type=="Estimated Average Requirement (EAR)") %>% 
      arrange(sex, age_range)
    
    # Maximum value
    max1 <- max(plot_dat_1$value_hi, na.rm=T)
    max2 <- max(plot_dat_2$value_hi, na.rm=T)
    max_dri <- max(plot_dri_dat$value)
    max_val <- max(max2, max_dri)
    
    # Extract unit
    nutr_unit <- unique(plot_dat_1$units_short)
    
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
      labs(x="Age range", y=paste0("Daily per capita consumption\n(", nutr_unit, " per person per day)")) +
      scale_fill_discrete(name="Sex") +
      # Theme
      plot_theme_tab+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())+
      scale_y_continuous(limits = c(0, max_val))
    
    g2
      
  })
  
  ### -----------------------------------
  ### seafood-reforms Tab ---------------
  ### -----------------------------------
  
  ### Seafood Nutrition Content -----------------
  
  # Plot output 
  output$seafood_nutrition_content_plot <- renderPlot({
    
    g <- ggplot(vaitla_nutr_preds_long, aes(x=value_md)) +
      geom_histogram(fill="grey70") +
      facet_wrap(~nutrient_label, ncol=3, scales="free") +
      scale_color_discrete(name="") +
      labs(x="Nutrient concentration", y="Number of species") +
      plot_theme
    
    g

  })
  
  ### Fisheries Reforms ---------------
  
  # Reactive text: Based upon selected tab
  output$fisheries_reforms_text <- renderUI({
    
    selected_tab <- switch(input$fisheries_reforms_tabs,
                           "fisheries-reforms-tabs-1" = list("fisheries_reforms_1"),
                           "fisheries-reforms-tabs-2" = list("fisheries_reforms_2"),
                           "fisheries-reforms-tabs-3" = list("fisheries_reforms_3"))
    
    includeHTML(paste0("./text/04-seafood-reforms/", selected_tab[[1]], ".html"))
    
  })
  
  # Plot output: Tab 1
  output$fisheries_reforms_plot_1 <- renderPlot({
    
    #req(input$w_seafood_reforms_country)
    req(nrow(fish_nutrition_content_plot_dat) > 0)

    g <- ggradar::ggradar(fish_nutrition_content_plot_dat) + 
      theme(legend.position = "right")
      #theme(plot.margin = unit(c(0.8, 0.5, 0.5, 0.5), "cm"))
    g
    
  })
  
  # Plot output: Tab 2
  output$fisheries_reforms_plot_2 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    
    # Prepare data
    plot_data <- edible_meat_production_data %>% 
      dplyr::filter(iso3 == input$w_seafood_reforms_country) %>% 
      dplyr::filter(year == 2100)
    
    req(nrow(plot_data) > 0)
    
    custom_pal <- c("Bivalves and gastropods" = "#FF5A5F", "Cephalopods" = "#FFB400", "Crustaceans" = "#007A87", "Fish, demersal" = "#8CE071", "Fish, other" =  "#7B0051", "Fish, pelagic" = "#00D1C1")
    
    # Plot data
    g <- ggplot(plot_data, aes(x=scenario, y=meat_mt/1e6, fill=genus)) +
      facet_wrap(~rcp, ncol=4) +
      geom_bar(stat="identity") +
      labs(x="Management scenario", y="Edible meat (millions of mt)") +
      scale_fill_manual(name = "Major group", values = custom_pal[names(custom_pal) %in% unique(plot_data$genus)]) +
      plot_theme_tab
    
    g

  })
  
  # Plot output: Tab 3
  output$fisheries_reforms_plot_3 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    
    # Prepare data
    plot_data <- nutrient_demand_edible_meat_production_data %>% 
      filter(iso3 == input$w_seafood_reforms_country)
    
    req(nrow(plot_data) > 0)
    
    custom_pal <- c("Bivalves and gastropods" = "#FF5A5F", "Cephalopods" = "#FFB400", "Crustaceans" = "#007A87", "Fish, demersal" = "#8CE071", "Fish, other" =  "#7B0051", "Fish, pelagic" = "#00D1C1")
    
    # Plot data
    g <- ggplot(plot_data, aes(x=demand_prop, y=nutrient, fill=genus)) +
      facet_grid(year ~ rcp) +
      geom_bar(stat="identity") +
      # Labels
      labs(x="% of nutrient demand met from marine capture fisheries reforms", y="") +
      scale_fill_manual(name = "Major group", values = custom_pal[names(custom_pal) %in% unique(plot_data$genus)]) +
      plot_theme_tab
    g
    
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
      mutate(viable_area = n_points*100) %>% # number of 100 sq km cells
      dplyr::filter(viable_area > 0) %>%
      mutate(rank = dense_rank(desc(viable_area)))
      
    
  })
  
  # Plot output
  output$aquaculture_reforms_plot_1 <- renderPlot({
    
    req(input$w_seafood_reforms_country)
    req(input$w_seafood_reforms_aquaculture_climate_scenario)
    
    plot_dat <- aquaculture_reforms_dat() %>%
      group_by(group, sov1_iso, scenario) %>%
      slice_min(rank, n = 10, with_ties = F) %>%
      ungroup()

    req(nrow(plot_dat) > 0)
    
    fill_pal <- c("Bivalve" = "#76B7B2", "Finfish" = "#B07AA1") 

    g <-ggplot(plot_dat)+
      aes(x = reorder_within(species, viable_area, group), y = viable_area/1000, fill = group) +
      geom_bar(stat = "identity") +
      theme_bw() +
      scale_x_reordered() +
      labs(x = "", y = "Profitable area (thousands of square km)", fill = "Species type") +
      scale_fill_manual(values = fill_pal[names(fill_pal) %in% unique(plot_dat$group)]) +
      coord_flip() +
      facet_wrap( ~ group, scales = "free_y", ncol = 1) +
      plot_theme_tab+
      theme(legend.position = "none")
      

    g
    
  })
  
  ### Update species select input based on country and RCP selected above ---------
  observe({

    # Viable Species ordered
    viable_species_ordered <- aquaculture_reforms_dat() %>%
      dplyr::select(species, rank)

    # Only allow the top 10 species (ranked in terms of profitable area) to be selected. 
    viable_species <- nutrient_dat_pmax %>%
      inner_join(viable_species_ordered, by = "species") %>%
      mutate(species = fct_reorder(species, rank)) %>%
      arrange(rank)
    
    # Update input
    updateSelectizeInput(session,
                         "w_seafood_reforms_radar_species",
                         choices = levels(viable_species$species),
                         selected = viable_species$species[1:2])
    
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
      theme(plot.margin = unit(c(3.5, 0.5, 0.5, 0.5), "cm"))+
      theme(legend.position = "right")
    g
    
  })
  
  
  ### Mariculture Site Explorer ---------------
  
  ### Reactive data set based on selected country and climate scenario ---
  site_explorer_dat <- eventReactive(input$w_seafood_reforms_site_explorer_species, {
    
    # Load data
    species <- str_replace(input$w_seafood_reforms_site_explorer_species, " ", "_")
    dat_file_name <- paste0(species, ".Rds")
    
    data <- readRDS(paste0("./data/processed/species_rasters/", dat_file_name))
    
  })
  
  ### Update scenario select input based upon species selected and data loaded  ---------
  observe({

    # Viable scenarios
    scenarios <- site_explorer_dat() %>%
      mutate(new_scenario = case_when(scenario == "RCP26" ~ "RCP 2.6",
                                      scenario == "RCP45" ~ "RCP 4.5",
                                      scenario == "RCP60" ~ "RCP 6.0",
                                      scenario == "RCP85" ~ "RCP 8.5")) %>%
      distinct(new_scenario)
    
    
    # Update input
    updateSelectizeInput(session,
                         "w_seafood_reforms_site_explorer_climate_scenario",
                         choices = scenarios$new_scenario)

  })

  # ## Update species select input based on country selected  ---------
  observe({

    # Viable Species ordered
    possible_species <- rcp_projections %>%
      dplyr::filter(sov1_iso == input$w_seafood_reforms_country) %>%
      dplyr::filter(year == 2100) %>%
      distinct(species)

    # Update input
    updateSelectizeInput(session,
                         "w_seafood_reforms_site_explorer_species",
                         choices = possible_species$species)

  })
  
  ### Create ractive values object to keep track of map bounding box ----
  rv_eez_bbox <- reactiveValues(xmin = -180,
                                xmax = 180,
                                ymin = -90,
                                ymax = 90,
                                geometry = NULL)
  
  ## Get bounding box for plot based on country selected --------
  observe({
    
    eez_map <- eez %>%
      dplyr::filter(sov1_iso3 == input$w_seafood_reforms_country) %>%
      group_by(sov1_iso3) %>%
      summarize(geometry = st_union(geometry))
    
    bbox <- st_bbox(eez_map) %>%
      as.vector()
    
    isolate(rv_eez_bbox$xmin <- bbox[1])
    isolate(rv_eez_bbox$ymin <- bbox[2])
    isolate(rv_eez_bbox$xmax <- bbox[3])
    isolate(rv_eez_bbox$ymax <- bbox[4])
    isolate(rv_eez_bbox$geometry <- eez_map)

  })

  # Plot output
  output$mariculture_site_explorer_plot <- renderLeaflet({

    req(nrow(site_explorer_dat()) > 0)
    req(input$w_seafood_reforms_site_explorer_climate_scenario)
    
    scenario_var <- switch(input$w_seafood_reforms_site_explorer_climate_scenario,
                           "RCP 2.6" = list("RCP26"),
                           "RCP 4.5" = list("RCP45"),
                           "RCP 6.0" = list("RCP60"),
                           "RCP 8.5" = list("RCP85"))

    # Filter for year, and plot
    plot_dat <- site_explorer_dat() %>%
      dplyr::filter(scenario == scenario_var[[1]])

    plot_raster <- unlist(plot_dat$suitability)[[1]]
    
    # Define colors
    plot_pal <- colorNumeric(c("green"), values(plot_raster),
                             na.color = "transparent")
    
    # Plot
    leaflet('mariculture_site_explorer_plot', options = leafletOptions(minZoom = 2, maxZoom = 4, zoomControl = TRUE)) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addPolygons(data = eez$geometry,
                  fillColor = "white",
                  fillOpacity = 0,
                  color= "grey", 
                  weight = 0.5) %>%
      addPolygons(data = rv_eez_bbox$geometry$geometry,
                  fillColor = "white",
                  fillOpacity = 0,
                  color= "red", 
                  weight = 1.5) %>%
      addRasterImage(plot_raster, colors = plot_pal, opacity = 0.8) %>%
      fitBounds(rv_eez_bbox$xmin, rv_eez_bbox$ymin, rv_eez_bbox$xmax, rv_eez_bbox$ymax)

  })

})



