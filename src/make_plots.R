output$plot_T <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Temperature", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Temperature (\u00B0C)")
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = YearDay, 
                     y = Value, 
                     color = Year == input$year, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND = 6", 
                                    "Regular" = 16)) + 
      scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                    yday(paste(input$year, "-02-01", sep = "")), 
                                    yday(paste(input$year, "-03-01", sep = "")), 
                                    yday(paste(input$year, "-04-01", sep = "")), 
                                    yday(paste(input$year, "-05-01", sep = "")), 
                                    yday(paste(input$year, "-06-01", sep = "")), 
                                    yday(paste(input$year, "-07-01", sep = "")), 
                                    yday(paste(input$year, "-08-01", sep = "")), 
                                    yday(paste(input$year, "-09-01", sep = "")), 
                                    yday(paste(input$year, "-10-01", sep = "")), 
                                    yday(paste(input$year, "-11-01", sep = "")), 
                                    yday(paste(input$year, "-12-01", sep = ""))), 
                         labels = month.abb)
  }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_S <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Salinity", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Salinity (PSU)")
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = YearDay, 
                     y = Value, 
                     color = Year == input$year, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND = 6", 
                                    "Regular" = 16)) + 
      scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                    yday(paste(input$year, "-02-01", sep = "")), 
                                    yday(paste(input$year, "-03-01", sep = "")), 
                                    yday(paste(input$year, "-04-01", sep = "")), 
                                    yday(paste(input$year, "-05-01", sep = "")), 
                                    yday(paste(input$year, "-06-01", sep = "")), 
                                    yday(paste(input$year, "-07-01", sep = "")), 
                                    yday(paste(input$year, "-08-01", sep = "")), 
                                    yday(paste(input$year, "-09-01", sep = "")), 
                                    yday(paste(input$year, "-10-01", sep = "")), 
                                    yday(paste(input$year, "-11-01", sep = "")), 
                                    yday(paste(input$year, "-12-01", sep = ""))), 
                         labels = month.abb)
  }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_entero <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Enterococcus", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Enterococcus (CFU/100 mL)")
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = YearDay, 
                     y = Value, 
                     color = Year == input$year, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND = 6", 
                                    "Regular" = 16)) + 
      scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                    yday(paste(input$year, "-02-01", sep = "")), 
                                    yday(paste(input$year, "-03-01", sep = "")), 
                                    yday(paste(input$year, "-04-01", sep = "")), 
                                    yday(paste(input$year, "-05-01", sep = "")), 
                                    yday(paste(input$year, "-06-01", sep = "")), 
                                    yday(paste(input$year, "-07-01", sep = "")), 
                                    yday(paste(input$year, "-08-01", sep = "")), 
                                    yday(paste(input$year, "-09-01", sep = "")), 
                                    yday(paste(input$year, "-10-01", sep = "")), 
                                    yday(paste(input$year, "-11-01", sep = "")), 
                                    yday(paste(input$year, "-12-01", sep = ""))), 
                         labels = month.abb)
  }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_fecal <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Fecal Coliform", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Fecal coliform (CFU/100 mL)")
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = YearDay, 
                     y = Value, 
                     color = Year == input$year, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND = 6", 
                                    "Regular" = 16)) + 
      scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                    yday(paste(input$year, "-02-01", sep = "")), 
                                    yday(paste(input$year, "-03-01", sep = "")), 
                                    yday(paste(input$year, "-04-01", sep = "")), 
                                    yday(paste(input$year, "-05-01", sep = "")), 
                                    yday(paste(input$year, "-06-01", sep = "")), 
                                    yday(paste(input$year, "-07-01", sep = "")), 
                                    yday(paste(input$year, "-08-01", sep = "")), 
                                    yday(paste(input$year, "-09-01", sep = "")), 
                                    yday(paste(input$year, "-10-01", sep = "")), 
                                    yday(paste(input$year, "-11-01", sep = "")), 
                                    yday(paste(input$year, "-12-01", sep = ""))), 
                         labels = month.abb)
  }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_NH3 <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Ammonia Nitrogen", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Ammonia N (mg/L)") + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} 
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = YearDay, 
                     y = Value, 
                     color = Year == input$year, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND = 6", 
                                    "Regular" = 16)) + 
      scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                    yday(paste(input$year, "-02-01", sep = "")), 
                                    yday(paste(input$year, "-03-01", sep = "")), 
                                    yday(paste(input$year, "-04-01", sep = "")), 
                                    yday(paste(input$year, "-05-01", sep = "")), 
                                    yday(paste(input$year, "-06-01", sep = "")), 
                                    yday(paste(input$year, "-07-01", sep = "")), 
                                    yday(paste(input$year, "-08-01", sep = "")), 
                                    yday(paste(input$year, "-09-01", sep = "")), 
                                    yday(paste(input$year, "-10-01", sep = "")), 
                                    yday(paste(input$year, "-11-01", sep = "")), 
                                    yday(paste(input$year, "-12-01", sep = ""))), 
                         labels = month.abb)
  }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_NNN <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Nitrite + Nitrate Nitrogen", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Nitrate + nitrite N (mg/L)") + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))}
    if (nrow(p$data) > 0) {
      p <- p + 
        geom_point(aes(x = YearDay, 
                       y = Value, 
                       color = Year == input$year, 
                       shape = Shape, 
                       customdata = URL, 
                       text = paste0(Value, "; ", CollectDate))) + 
        scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                      "FALSE" = alpha("black", 0.2))) + 
        scale_shape_manual(values = c("Bad" = 15, 
                                      "ND = 6", 
                                      "Regular" = 16)) + 
        scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                      yday(paste(input$year, "-02-01", sep = "")), 
                                      yday(paste(input$year, "-03-01", sep = "")), 
                                      yday(paste(input$year, "-04-01", sep = "")), 
                                      yday(paste(input$year, "-05-01", sep = "")), 
                                      yday(paste(input$year, "-06-01", sep = "")), 
                                      yday(paste(input$year, "-07-01", sep = "")), 
                                      yday(paste(input$year, "-08-01", sep = "")), 
                                      yday(paste(input$year, "-09-01", sep = "")), 
                                      yday(paste(input$year, "-10-01", sep = "")), 
                                      yday(paste(input$year, "-11-01", sep = "")), 
                                      yday(paste(input$year, "-12-01", sep = ""))), 
                           labels = month.abb)
    }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_totalN <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Total Nitrogen", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Total N (mg/L)") + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} 
    if (nrow(p$data) > 0) {
      p <- p + 
        geom_point(aes(x = YearDay, 
                       y = Value, 
                       color = Year == input$year, 
                       shape = Shape, 
                       customdata = URL, 
                       text = paste0(Value, "; ", CollectDate))) + 
        scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                      "FALSE" = alpha("black", 0.2))) + 
        scale_shape_manual(values = c("Bad" = 15, 
                                      "ND = 6", 
                                      "Regular" = 16)) + 
        scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                      yday(paste(input$year, "-02-01", sep = "")), 
                                      yday(paste(input$year, "-03-01", sep = "")), 
                                      yday(paste(input$year, "-04-01", sep = "")), 
                                      yday(paste(input$year, "-05-01", sep = "")), 
                                      yday(paste(input$year, "-06-01", sep = "")), 
                                      yday(paste(input$year, "-07-01", sep = "")), 
                                      yday(paste(input$year, "-08-01", sep = "")), 
                                      yday(paste(input$year, "-09-01", sep = "")), 
                                      yday(paste(input$year, "-10-01", sep = "")), 
                                      yday(paste(input$year, "-11-01", sep = "")), 
                                      yday(paste(input$year, "-12-01", sep = ""))), 
                           labels = month.abb)
    }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})

output$plot_P <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Orthophosphate Phosphorus", 
                       Locator == input$site) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(x = "", 
         y = "Orthophosphate P (mg/L)") + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} 
    if (nrow(p$data) > 0) {
      p <- p + 
        geom_point(aes(x = YearDay, 
                       y = Value, 
                       color = Year == input$year, 
                       shape = Shape, 
                       customdata = URL, 
                       text = paste0(Value, "; ", CollectDate))) + 
        scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                      "FALSE" = alpha("black", 0.2))) + 
        scale_shape_manual(values = c("Bad" = 15, 
                                      "ND = 6", 
                                      "Regular" = 16)) + 
        scale_x_continuous(breaks = c(yday(paste(input$year, "-01-01", sep = "")), 
                                      yday(paste(input$year, "-02-01", sep = "")), 
                                      yday(paste(input$year, "-03-01", sep = "")), 
                                      yday(paste(input$year, "-04-01", sep = "")), 
                                      yday(paste(input$year, "-05-01", sep = "")), 
                                      yday(paste(input$year, "-06-01", sep = "")), 
                                      yday(paste(input$year, "-07-01", sep = "")), 
                                      yday(paste(input$year, "-08-01", sep = "")), 
                                      yday(paste(input$year, "-09-01", sep = "")), 
                                      yday(paste(input$year, "-10-01", sep = "")), 
                                      yday(paste(input$year, "-11-01", sep = "")), 
                                      yday(paste(input$year, "-12-01", sep = ""))), 
                           labels = month.abb)
    }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})