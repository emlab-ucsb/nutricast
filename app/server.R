### --------------------------------------
### --------------------------------------
### This script defines the server
### --------------------------------------
### --------------------------------------

shinyServer(function(input, output, session) {
    
    ### ----------------------------------
    ### General/Nav ----------------------
    ### ----------------------------------

    # output$plotly_example <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x  <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # })
    # 
  
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
    
    ### Historical production plot ---------
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
    ### Tab 1 ----------------------------
    ### ----------------------------------
    
    ### Sample value box with date
    output$value_box_1 <- renderValueBox({
        
        valueBox(
            value = today(),
            subtitle = text$item_label[text$item_id == "value_box_1"],
            icon = icon(text$item_icon[text$item_id == "value_box_1"]),
            color = "aqua"
        )
    })
    
    ### Sample value box with value
    output$value_box_2 <- renderValueBox({
        
        valueBox(
            value = round(runif(1, min = 456, max = 1256)),
            subtitle = text$item_label[text$item_id == "value_box_2"],
            icon = icon(text$item_icon[text$item_id == "value_box_2"]),
            color = "maroon"
        )
    })
    
    ### Sample plot 1 
    output$plot_1 <- renderPlot({
        
        ggplot(dat, aes(x = mpg, color = cyl, fill = cyl, group = cyl))+
            geom_density()+
            theme_classic()
        
    })
    
    ### Sample plot 2 
    output$plot_2 <- renderPlot({
        
        ggplot(dat, aes(x = hp, y = mpg, color = cyl, fill = cyl))+
            geom_point()+
            theme_classic()
        
    })
    
    ### ----------------------------------
    ### Tab 2 ----------------------------
    ### ----------------------------------
    
    
    ### ----------------------------------
    ### Tab 3 ----------------------------
    ### ----------------------------------

})



