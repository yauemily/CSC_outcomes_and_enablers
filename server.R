# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  setBookmarkExclude(c(
    "cookies", "link_to_app_content_tab",
    "tabBenchmark_rows_current", "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected", "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected", "tabBenchmark_search",
    "tabBenchmark_rows_selected", "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A", "plotly_hover-A", "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts",
    "bookmark1", "bookmark2"
  ))

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  observe({
    if (input$navlistPanel == "dashboard") {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$selectPhase, ", ",
          input$selectArea
        )
      )
    } else {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
          input$navlistPanel
        )
      )
    }
  })

  # output if cookie is unspecified
  observeEvent(input$cookies, {
    if (!is.null(input$cookies)) {
      if (!("dfe_analytics" %in% names(input$cookies))) {
        shinyalert(
          inputId = "cookie_consent",
          title = "Cookie consent",
          text = "This site uses cookies to record traffic flow using Google Analytics",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Accept",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else {
        msg <- list(
          name = "dfe_analytics",
          value = input$cookies$dfe_analytics
        )
        session$sendCustomMessage("analytics-consent", msg)
        if ("cookies" %in% names(input)) {
          if ("dfe_analytics" %in% names(input$cookies)) {
            if (input$cookies$dfe_analytics == "denied") {
              ga_msg <- list(name = paste0("_ga_", google_analytics_key))
              session$sendCustomMessage("cookie-remove", ga_msg)
            }
          }
        }
      }
    }
  })

  observeEvent(input$cookie_consent, {
    msg <- list(
      name = "dfe_analytics",
      value = ifelse(input$cookie_consent, "granted", "denied")
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "denied") {
          ga_msg <- list(name = paste0("_ga_", google_analytics_key))
          session$sendCustomMessage("cookie-remove", ga_msg)
        }
      }
    }
  })

  observeEvent(input$remove, {
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools, this site uses cookies to identify numbers of unique users as part of Google Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      "Cookies consent has not been confirmed."
    }
  })


  #  output$cookie_status <- renderText(as.character(input$cookies))
  
  
  
  # output$choice_text_test <- renderText({
  #   c(paste0("you have selected",input$select_geography))
  # })
  
  # Time period dropdown also does not need to appear here - does not need to be reactive

  # Simple server stuff goes here ------------------------------------------------------------
  reactiveRevBal <- reactive({
    dfRevBal %>% filter(
      area_name == input$selectArea | area_name == "England",
      school_phase == input$selectPhase
    )
  })

  # Define server logic required to draw a histogram
  output$lineRevBal <- renderPlotly({
    ggplotly(createAvgRevTimeSeries(reactiveRevBal(), input$selectArea)) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  reactiveBenchmark <- reactive({
    dfRevBal %>%
      filter(
        area_name %in% c(input$selectArea, input$selectBenchLAs),
        school_phase == input$selectPhase,
        year == max(year)
      )
  })

  output$colBenchmark <- renderPlotly({
    ggplotly(
      plotAvgRevBenchmark(reactiveBenchmark()) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$tabBenchmark <- renderDataTable({
    datatable(
      reactiveBenchmark() %>%
        select(
          Area = area_name,
          `Average Revenue Balance (£)` = average_revenue_balance,
          `Total Revenue Balance (£m)` = total_revenue_balance_million
        ),
      options = list(
        scrollX = TRUE,
        paging = FALSE
      )
    )
  })
  
  # CSC server logic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enabler 2 Server Logic ----
  # Geographic level does not need to be here as it does not need to change depending on other dropdowns
  
  # Geographic breakdown e2 (list of either LA names or Region names)
  observeEvent(eventExpr={input$select_geography_e2},{
    choices = sort(unique(workforce_data[workforce_data$geographic_level == input$select_geography_e2, "geo_breakdown"]),decreasing = FALSE)
    
    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_e2",
      selected = choices[1],
      choices = choices
      
    )
  }
  )
  # Confirmation sentence -------
  
  region <- reactive({
    location_data %>%
      filter(la_name == input$geographic_breakdown_e2) %>%
      pull(region_name)  %>%
      as.character()  # Convert to character
  })
  
  output$enabler2_choice_text1 <- renderText({
    if (input$select_geography_e2 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_e2 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ".")
    } else if (input$select_geography_e2 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_e2), " level statistics for ", tags$b(input$geographic_breakdown_e2), ", in ", region(), ".")
    }
  })
  
  output$enabler2_choice_text2 <- renderText({
    #Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      #Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })

  
  #social worker rate plot and table -----
  output$s_w_headline_txt <- renderText({
    stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(turnover_rate_fte_perc), nsmall = 1)
    paste0(stat,"%","<br>","<p style='font-size:16px; font-weight:500;'>","(",max(workforce_data$time_period),")", "</p>")
  })
  
  #Social worker plot benchmarking
  
  output$plot_s_w_turnover <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_e2), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_e2),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))
    }
    
    ggplotly(
      plotly_time_series(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2,'turnover_rate_fte_perc', 'Turnover Rate (FTE) %')%>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  
  output$table_s_w_turnover <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
    filtered_data <- workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
      select(time_period, geo_breakdown,turnover_rate_fte_perc)

    #national only
  }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
    filtered_data<-workforce_data %>%
      filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') %>%
      select(time_period, geo_breakdown,turnover_rate_fte_perc)

    #regional only
  }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
    location <- location_data %>%
      filter(la_name %in% input$geographic_breakdown_e2)

    filtered_data<-workforce_data %>%
      filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
      select(time_period, geo_breakdown,turnover_rate_fte_perc)

    #both selected
  }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
    location <- location_data %>%
      filter(la_name %in% input$geographic_breakdown_e2)

    filtered_data<- workforce_data %>%
      filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))%>%
      select(time_period, geo_breakdown,turnover_rate_fte_perc)
  }
    datatable(
      filtered_data,
      colnames = c("Time Period", "Geographical Breakdown", "Turnover Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #turnover rate plot by region
  output$plot_turnover_reg <- plotly::renderPlotly({
    ggplotly(
      plot_turnover_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  #turnover rate table by region
  output$table_turnover_reg <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == 'Regional', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        turnover_rate_fte_perc
      ) %>%
        arrange(desc(turnover_rate_fte_perc)),
      colnames = c("Time Period", "Geographical Breakdown", "Turnover Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  output$plot_turnover_la <- plotly::renderPlotly({
    ggplotly(
      plot_turnover_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_turnover_la <- renderDataTable({
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }
      
      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, turnover_rate_fte_perc)  %>%
        arrange(desc(turnover_rate_fte_perc))
      
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>% filter(geographic_level == 'Local authority', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        turnover_rate_fte_perc
      ) %>%
        arrange(desc(turnover_rate_fte_perc))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "Turnover Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #agency worker rate plot ----
  output$agency_rate_txt <- renderText({
    stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(agency_worker_rate_fte_perc), nsmall = 1)
    paste0(stat,"%","<br>", "<p style='font-size:16px; font-weight:500;'>","(",max(workforce_data$time_period),")", "</p>")
    })
  
  
  output$plot_agency_worker <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_e2), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_e2),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))
    }
    
    ggplotly(
      plotly_time_series(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2,'agency_worker_rate_fte_perc', 'Agency Worker Rate (FTE) %')%>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  # output$table_agency_worker <- renderDataTable({
  #   datatable(
  #     workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown) %>% select(
  #       time_period, geo_breakdown,
  #       agency_worker_rate_fte_perc
  #     ),
  #     colnames = c("Time Period", "Geographical Breakdown", "Agency Worker Rate (FTE) %"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = TRUE
  #     )
  #   )
  # })
  
  output$table_agency_worker <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data <- workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown,agency_worker_rate_fte_perc)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') %>%
        select(time_period, geo_breakdown,agency_worker_rate_fte_perc)
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown,agency_worker_rate_fte_perc)
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))%>%
        select(time_period, geo_breakdown,agency_worker_rate_fte_perc)
    }
    datatable(
      filtered_data,
      colnames = c("Time Period", "Geographical Breakdown", "Agency Worker Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #agency rate plot by region
  output$plot_agency_reg <- plotly::renderPlotly({
    ggplotly(
      plot_agency_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  #agency rate table by region
  output$table_agency_reg <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == 'Regional', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        agency_worker_rate_fte_perc
      ) %>%
        arrange(desc(agency_worker_rate_fte_perc)),
      colnames = c("Time Period", "Geographical Breakdown", "Agency Worker Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  
  output$plot_agency_rate_la <- plotly::renderPlotly({
    ggplotly(
      plot_agency_rate_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  
  output$table_agency_rate_la <- renderDataTable({
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }
      
      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, agency_worker_rate_fte_perc)  %>%
        arrange(desc(agency_worker_rate_fte_perc))
      
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>% filter(geographic_level == 'Local authority', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        agency_worker_rate_fte_perc
      ) %>%
        arrange(desc(agency_worker_rate_fte_perc))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "Agency Worker Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  # Vacancy Rate plot and table -----
  output$vacancy_rate_txt <- renderText({
    paste0(format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(vacancy_rate_fte_perc), nsmall = 1), "%",
           "<br>","<p style='font-size:16px; font-weight:500;'>", "(",max(workforce_data$time_period),")", "</p>")
  })
  
  output$plot_vacancy_rate <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_e2), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_e2),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))
    }
    
    ggplotly(
      plotly_time_series(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2,'vacancy_rate_fte_perc', 'Vacancy Rate (FTE) %')%>%
        config(displayModeBar = F),
      height = 420
    )
  })

  # output$table_vacancy_rate <- renderDataTable({
  #   datatable(
  #     workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown) %>% select(
  #       time_period, geo_breakdown,
  #       vacancy_rate_fte_perc
  #     ),
  #     colnames = c("Time Period", "Geographical Breakdown", "Vacancy Rate (FTE) %"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = TRUE
  #     )
  #   )
  # })
  
  output$table_vacancy_rate <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data <- workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown,vacancy_rate_fte_perc)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') %>%
        select(time_period, geo_breakdown,vacancy_rate_fte_perc)
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown,vacancy_rate_fte_perc)
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))%>%
        select(time_period, geo_breakdown,vacancy_rate_fte_perc)
    }
    datatable(
      filtered_data,
      colnames = c("Time Period", "Geographical Breakdown", "Vacancy Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  output$plot_vacancy_rate_la <- plotly::renderPlotly({
    ggplotly(
      plot_vacancy_rate_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_vacancy_rate_la <- renderDataTable({
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }
      
      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, vacancy_rate_fte_perc)  %>%
        arrange(desc(vacancy_rate_fte_perc))
      
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>% filter(geographic_level == 'Local authority', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        vacancy_rate_fte_perc
      ) %>%
        arrange(desc(vacancy_rate_fte_perc))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "Vacancy Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  
  #agency rate plot by region
  output$plot_vacancy_reg <- plotly::renderPlotly({
    ggplotly(
      plot_vacancy_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  #vacancy rate table by region
  output$table_vacancy_reg <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == 'Regional', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        vacancy_rate_fte_perc
      ) %>%
        arrange(desc(vacancy_rate_fte_perc)),
      colnames = c("Time Period", "Geographical Breakdown", "Vacancy Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  
  
  
  
  #Caseload ----
  output$caseload_txt <- renderText({
    previous_year = workforce_data %>% filter(time_period == (max(workforce_data$time_period)-1) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(caseload_fte)
    current_year = workforce_data %>% filter(time_period == (max(workforce_data$time_period)) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(caseload_fte)
    
    if (current_year < previous_year){
      paste0(format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(caseload_fte), nsmall = 1),"<br>",
             "<p style='font-size:16px; font-weight:500;'>","in ",max(workforce_data$time_period), " down from ", previous_year, " in ", (max(workforce_data$time_period)-1), "</p>")
    }else{
      paste0(format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown_e2) %>% select(caseload_fte), nsmall = 1),"<br>",
             "<p style='font-size:16px; font-weight:500;'>","in ",max(workforce_data$time_period)," up from ", previous_year, " in ", (max(workforce_data$time_period)-1), "</p>")
            
    }
  })
  
  output$caseload_plot <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_e2), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_e2),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter(geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))
    }
    
  # Set the max y-axis scale
  max_rate <- max(workforce_data$caseload_fte, na.rm = TRUE)
  
  # Round the max_rate to the nearest 50
  max_rate <- ceiling(max_rate / 50) * 50
  
  p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e2, input$geographic_breakdown_e2,'caseload_fte', 'Average Caseload (FTE)', max_rate) %>%
    config(displayModeBar = F)
  
  
  ggplotly(p, height = 420) %>%
    layout(yaxis = list(range = c(0, max_rate)))
})
  
  
  
  output$table_caseload <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data <- workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown_e2) %>%
        select(time_period, geo_breakdown,caseload_fte)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_e2) && is.null(input$region_comparison_checkbox_e2)){
      filtered_data<-workforce_data %>%
        filter((geographic_level %in% input$select_geography_e2 & geo_breakdown %in% input$geographic_breakdown_e2)|geographic_level == 'National') %>%
        select(time_period, geo_breakdown,caseload_fte)
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<-workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name))) %>%
        select(time_period, geo_breakdown,caseload_fte)
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_e2) && !is.null(input$region_comparison_checkbox_e2)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_e2)
      
      filtered_data<- workforce_data %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_e2, location$region_name)|geographic_level == 'National'))%>%
        select(time_period, geo_breakdown,caseload_fte)
    }
    datatable(
      filtered_data,
      colnames = c("Time Period", "Geographical Breakdown", "Average Caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  output$plot_caseload_reg <- plotly::renderPlotly({
    ggplotly(
      plot_caseloads_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$plot_caseload_la <- plotly::renderPlotly({
    ggplotly(
      plot_caseload_la(input$geographic_breakdown_e2, input$select_geography_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  # output$table_caseload <- renderDataTable({
  #   datatable(
  #     workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown) %>% select(
  #       time_period, geo_breakdown,
  #       caseload_fte
  #     ),
  #     colnames = c("Time Period", "Geographical Breakdown", "Average Caseload (FTE)"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = TRUE
  #     )
  #   )
  # })

  
  output$table_caseload_reg <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == 'Regional', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        caseload_fte
      ) %>%
        arrange(desc(caseload_fte)),
      colnames = c("Time Period", "Geographical Breakdown", "Average Caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  output$table_caseload_la <- renderDataTable({
    if (input$select_geography_e2 == "Regional") {
      if (input$geographic_breakdown_e2 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_e2) %>%
          pull(la_name)
      }
      
      data <- workforce_data %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, caseload_fte)  %>%
        arrange(desc(caseload_fte))
      
    } else if (input$select_geography_e2 %in% c("Local authority", "National")) {
      data <- workforce_data %>% filter(geographic_level == 'Local authority', time_period == max(workforce_data$time_period)) %>% select(
        time_period, geo_breakdown,
        caseload_fte
      ) %>%
        arrange(desc(caseload_fte))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "Average Caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  

  # Ethnicity and Diversity Domain-----
  output$white_ethnicity_txt <- renderText({
    paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
                                           & geo_breakdown %in% input$geographic_breakdown_e2 
                                           & OrgRole == "All children and family social workers") %>% 
                    select(white_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  })
  
  output$non_white_txt <- renderText({
    white_stat = workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
                                          & geo_breakdown %in% input$geographic_breakdown_e2 
                                          & OrgRole == "All children and family social workers") %>% 
      select(white_perc)
    non_white_stat = 100 - as.numeric(white_stat)
    paste0(format(non_white_stat, nsmall = 1), "%", "<br>","<p style='font-size:16px; font-weight:500;'>", "(", max(workforce_eth$time_period) ,")", "</p>")
  })
  
  
  # output$asian_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown_e2 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(asian_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$black_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown_e2 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(black_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$mixed_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown_e2 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(mixed_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$other_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown_e2 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(other_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$white_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown_e2 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(white_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  
  
  output$plot_ethnicity_rate <- plotly::renderPlotly({
    ggplotly(
      plot_ethnicity_rate(input$geographic_breakdown_e2, input$geographic_level) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$plot_population_ethnicity_rate <- plotly::renderPlotly({
    ggplotly(
      plot_population_ethnicity_rate(input$geographic_breakdown_e2) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_ethnicity_rate <- renderDataTable({
    datatable(
      workforce_eth %>% 
        filter(geo_breakdown %in% input$geographic_breakdown_e2, OrgRole == 'All children and family social workers') %>% 
        select(time_period, geo_breakdown, white_perc, mixed_perc, asian_perc, black_perc, other_perc),
      colnames = c("Time Period", "Geographical Breakdown", "White %", "Mixed %", "Asian %", "Black %", "Other %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  # output$table_population_ethnicity_rate <- renderDataTable({
  #   datatable(
  #     combined_ethnicity_data %>% 
  #       filter(geo_breakdown %in% input$geographic_breakdown, OrgRole == 'All children and family social workers') %>% 
  #       select(geographic_level.x, geo_breakdown,
  #              Population_WhitePercentage, Workforce_WhitePercentage,
  #              Population_BlackPercentage, Workforce_BlackPercentage,
  #              Population_AsianPercentage, Workforce_AsianPercentage,
  #              Population_MixedPercentage, Workforce_MixedPercentage,
  #              Population_OtherPercentage, Workforce_OtherPercentage),
  #     colnames = c("Geographic Level", "Geographical Breakdown", 
  #                  "White % (Population)", "White % (Workforce)",
  #                  "Black % (Population)", "Black % (Workforce)",
  #                  "Asian % (Population)", "Asian % (Workforce)",
  #                  "Mixed % (Population)", "Mixed % (Workforce)",
  #                  "Other % (Population)", "Other % (Workforce)"),
  #     options = list(
  #       scrollx = FALSE,
  #       paging = TRUE
  #     )
  #   )
  # })
  
  
  output$table_population_ethnicity_rate <- renderDataTable({
    datatable(
      combined_ethnicity_data %>% 
      filter(geo_breakdown %in% input$geographic_breakdown_e2, OrgRole == 'All children and family social workers') %>% 
      select(geographic_level.x, geo_breakdown,
             Population_WhitePercentage, Workforce_WhitePercentage,
             Population_BlackPercentage, Workforce_BlackPercentage,
             Population_AsianPercentage, Workforce_AsianPercentage,
             Population_MixedPercentage, Workforce_MixedPercentage,
             Population_OtherPercentage, Workforce_OtherPercentage) %>%
      pivot_longer(
        cols = c(Population_WhitePercentage:Workforce_OtherPercentage), # Specify columns to reshape
        names_to = c(".value", "Ethnicity"), # Assign names to new columns
        names_pattern = "(.*)_(.*)Percentage" # Specify pattern to extract values from column names
      ),
    colnames = c("Geographic Level", "Geographical Breakdown", 
                 "Ethnicity Group", "Population Percentage (%)", "Workforce Percentage (%)"),
    options = list(
      scrollx = FALSE,
      paging = TRUE
    )
    )
  })
  
  output$plot_seniority_eth <- plotly::renderPlotly({
    ggplotly(
      plot_seniority_eth(input$geographic_breakdown_e2, input$geographic_level) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  cols <- c("time_period","geographic_level", "geo_breakdown", "seniority", "known_headcount", "white_perc", "mixed_perc", "asian_perc", "black_perc", "other_perc")
  
  output$table_seniority_eth <- renderDataTable({
    datatable(
      workforce_eth_seniority[, cols] %>% 
        filter(geo_breakdown %in% input$geographic_breakdown_e2, seniority != 'All children and family social workers', time_period == max(workforce_eth_seniority$time_period)) %>% 
        select(time_period,geographic_level, geo_breakdown, seniority, known_headcount, white_perc, mixed_perc, asian_perc, black_perc, other_perc),
      colnames = c("Time Period","Geographic Level", "Geographical Breakdown", "Seniority Level", "Headcount with known ethnicity", "White %", "Mixed %", "Asian %", "Black %", "Other %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE,
        target = 'column'
      )
    )
  })
  
  #Outcome 1 -----
  # Geographic breakdown o1 (list of either LA names or Region names)
  observeEvent(eventExpr={input$select_geography_o1},{
    choices = sort(unique(dropdown_choices[dropdown_choices$geographic_level == input$select_geography_o1, "geo_breakdown"]),decreasing = FALSE)
    
    updateSelectizeInput(
      session = session,
      inputId = "geographic_breakdown_o1",
      selected = choices[1],
      choices = choices,
      
    )
  }
  )
  #outcome 1 confirmation text
  
  region_for_la_o1 <- reactive({
    selected_la <- input$geographic_breakdown_o1
    location_data %>%
      filter(la_name == selected_la) %>%
      pull(region_name)
  })
  
  output$outcome1_choice_text1 <- renderText({
    if (input$select_geography_o1 == "National") {
      paste0("You have selected ", tags$b(input$select_geography_o1), " level statistics on ", tags$b("England"), ".")
    } else if (input$select_geography_o1 == "Regional") {
      paste0("You have selected ", tags$b(input$select_geography_o1), " level statistics for ", tags$b(input$geographic_breakdown_o1), ".")
    } else if (input$select_geography_o1 == "Local authority") {
      paste0("You have selected ", tags$b(input$select_geography_o1), " level statistics for ", tags$b(input$geographic_breakdown_o1), ", in ", region_for_la_o1(), ".")
    }
  })
  
  output$outcome1_choice_text2 <- renderText({
    #Checking to see if they picked national average comparison
    if (!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)) {
      paste0("You have also selected to compare with the ", tags$b("National Average."))
      # If they picked regional comparison
    } else if (is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      paste0("You have also selected to compare with the ", tags$b("Regional average."))
      #Picked both national and regional comparison
    } else if (!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)) {
      paste0("You have also selected to compare with the ", tags$b("National average"), " and the ", tags$b("Regional average."))
    }
  })
  
  
  # CLA rate headline
  output$cla_rate_headline_txt <- renderText({
    stat <- format(cla_rates %>% filter(time_period == max(cla_rates$time_period) & geo_breakdown %in% input$geographic_breakdown_o1 & population_count == "Children starting to be looked after each year") 
                   %>% select(rate_per_10000), nsmall = 0)
    paste0(stat,"<br>","<p style='font-size:16px; font-weight:500;'>","(",max(cla_rates$time_period),")", "</p>")
  })
  
  # CLA rate Plot
  output$plot_cla_rate <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_o1), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_o1),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cla_rates %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<-cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)|geographic_level == 'National'))
    }
    
    filtered_data <- filtered_data %>%
      filter(population_count == "Children starting to be looked after each year")
    
    # Set the max y-axis scale
    max_rate <- max(cla_rates$rate_per_10000[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)
    
    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50
    
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1,'rate_per_10000', 'CLA Rate Per 10,000 Children', max_rate) %>%
      config(displayModeBar = F)
    
    
    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })
  
  # CLA rate TABLE
  output$table_cla_rate <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data <- cla_rates %>% filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown,rate_per_10000,population_count)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cla_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)|geographic_level == 'National') %>%
        select(time_period, geo_breakdown,rate_per_10000,population_count)
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<-cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown,rate_per_10000,population_count)
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<- cla_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)|geographic_level == 'National'))%>%
        select(time_period, geo_breakdown,rate_per_10000,population_count)
    }
    
    datatable(
      filtered_data %>% 
        filter(population_count == "Children starting to be looked after each year") %>% 
        select(time_period, geo_breakdown, rate_per_10000),
      colnames = c("Time Period", "Geographical Breakdown", "CLA Rate Per 10000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  # CLA rate regional plot
  output$plot_cla_rate_reg <- plotly::renderPlotly({
    ggplotly(
      plot_cla_rate_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  # CLA rate regional table
  output$table_cla_rate_reg <- renderDataTable({
    datatable(
      cla_rates %>% filter(geographic_level == 'Regional', time_period == max(cla_rates$time_period), population_count == "Children starting to be looked after each year") %>% select(
        time_period, geo_breakdown,
        rate_per_10000
      ) %>%
        arrange(desc(rate_per_10000)),
      colnames = c("Time Period", "Geographical Breakdown", "CLA Rate Per 10,000 Children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  # CLA rate LA plot
  output$plot_cla_rate_la <- plotly::renderPlotly({
    ggplotly(
      plot_cla_rate_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  # CLA rate La table
  output$table_cla_rate_la <- renderDataTable({
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }
      
      data <- cla_rates %>%
        filter(geo_breakdown %in% location, time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
        select(time_period, geo_breakdown, rate_per_10000)  %>%
        arrange(desc(rate_per_10000))
      
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cla_rates %>% filter(geographic_level == 'Local authority', time_period == max(cla_rates$time_period)) %>% select(
        time_period, geo_breakdown,
        rate_per_10000
      ) %>%
        arrange(desc(rate_per_10000))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "CLA Rate Per 10,000 Children"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
    # CIN rate headline
  output$cin_rate_headline_txt <- renderText({
   stat <- format(cin_rates %>% filter(time_period == max(cin_rates$time_period) & geo_breakdown %in% input$geographic_breakdown_o1) 
                 %>% select(CIN_rate), nsmall = 1)
    paste0(stat,"<br>","<p style='font-size:16px; font-weight:500;'>","(",max(cin_rates$time_period),")", "</p>")
  })
  
  #cin rate plot by region
  output$plot_cin_rate_reg <- plotly::renderPlotly({
    ggplotly(
      plot_cin_rate_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  
  #cin rate table by region
  output$table_cin_rates_reg <- renderDataTable({
    datatable(
      cin_rates %>% filter(geographic_level == 'Regional', time_period == max(cin_rates$time_period)) %>% select(
        time_period, geo_breakdown,
        CIN_rate
      ) %>%
        arrange(desc(CIN_rate)),
      colnames = c("Time Period", "Geographical Breakdown", "CIN Rate Per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #cin rate table by LA
  output$table_cin_rates_la <- renderDataTable({
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }
      
      data <- cin_rates %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown, CIN_rate)  %>%
        arrange(desc(CIN_rate))
      
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_rates %>% filter(geographic_level == 'Local authority', time_period == max(cin_rates$time_period)) %>% select(
        time_period, geo_breakdown,
        CIN_rate
      ) %>%
        arrange(desc(CIN_rate))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "CIN rates per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #cin rate chart by LA
  output$plot_cin_rates_la <- plotly::renderPlotly({
    ggplotly(
      plot_cin_rates_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  # CIN referral headline
  output$cin_referral_headline_txt <- renderText({
    stat <- format(cin_referrals %>% filter(time_period == max(cin_referrals$time_period) & geo_breakdown %in% input$geographic_breakdown_o1) 
                   %>% select(Re_referrals_percent), nsmall = 1)
    paste0(stat,"%","<br>","<p style='font-size:16px; font-weight:500;'>","(",max(cin_referrals$time_period),")", "</p>")
  })
  
    # CIN rate plot
  output$plot_cin_rate <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_o1), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_o1),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cin_rates %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<-cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)|geographic_level == 'National'))
    }
    
 
    # Set the max y-axis scale
    max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)
    
    # Round the max_rate to the nearest 50
    max_rate <- ceiling(max_rate / 50) * 50
    
    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1,'CIN_rate', 'CIN Rate Per 10,000 Children', max_rate) %>%
      config(displayModeBar = F)
    
    
    ggplotly(p, height = 420) %>%
      layout(yaxis = list(range = c(0, max_rate)))
  })
  
  #CIN rate table
  output$table_cin_rate <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data <- cin_rates %>% filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown,CIN_number,CIN_rate)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cin_rates %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)|geographic_level == 'National') %>%
        select(time_period, geo_breakdown,CIN_number,CIN_rate)
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<-cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown,CIN_number,CIN_rate)
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<- cin_rates %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)|geographic_level == 'National'))%>%
        select(time_period, geo_breakdown,CIN_number,CIN_rate)
    }
    
    datatable(
      filtered_data %>% 
              select(time_period, geo_breakdown, CIN_number,CIN_rate),
      colnames = c("Time Period", "Geographical Breakdown", "CIN at 31 March", "CIN Rate Per 10,000"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
    ##CIN referral plot
  output$plot_cin_referral <- plotly::renderPlotly({
    validate(need(!is.null(input$select_geography_o1), 'Select a geography level.'),
             need(!is.null(input$geographic_breakdown_o1),'Select a breakdown.'))
    #not both
    if(is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cin_referrals %>%
        filter(geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cin_referrals %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)|geographic_level == 'National') 
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<-cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) 
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)|geographic_level == 'National'))
    }
    
    ggplotly(
      plotly_time_series(filtered_data, input$select_geography_o1, input$geographic_breakdown_o1,'Re_referrals_percent', 'Re-referrals (%)')%>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  #CIN referral table
  output$table_cin_referral <- renderDataTable({
    #neither checkboxes
    if(is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data <- cin_referrals %>% filter(geo_breakdown %in% input$geographic_breakdown_o1) %>%
        select(time_period, geo_breakdown,Referrals, Re_referrals, Re_referrals_percent)
      
      #national only
    }else if(!is.null(input$national_comparison_checkbox_o1) && is.null(input$region_comparison_checkbox_o1)){
      filtered_data<-cin_referrals %>%
        filter((geographic_level %in% input$select_geography_o1 & geo_breakdown %in% input$geographic_breakdown_o1)|geographic_level == 'National') %>%
        select(time_period, geo_breakdown,Referrals, Re_referrals, Re_referrals_percent)
      
      #regional only
    }else if(is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<-cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name))) %>%
        select(time_period, geo_breakdown,Referrals, Re_referrals, Re_referrals_percent)
      
      #both selected
    }else if(!is.null(input$national_comparison_checkbox_o1) && !is.null(input$region_comparison_checkbox_o1)){
      location <- location_data %>%
        filter(la_name %in% input$geographic_breakdown_o1)
      
      filtered_data<- cin_referrals %>%
        filter((geo_breakdown %in% c(input$geographic_breakdown_o1, location$region_name)|geographic_level == 'National'))%>%
        select(time_period, geo_breakdown,Referrals, Re_referrals, Re_referrals_percent)
    }
    
    datatable(
      filtered_data %>% 
        select(time_period, geo_breakdown, Referrals, Re_referrals, Re_referrals_percent),
      colnames = c("Time Period", "Geographical Breakdown", "Referrals in the year", "Re-referrals within 12 months of a previous referral", "Re-referrals (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #cin referral table by region
  output$table_cin_referral_reg <- renderDataTable({
    datatable(
      cin_referrals %>% filter(geographic_level == 'Regional', time_period == max(cin_referrals$time_period)) %>% select(
        time_period, geo_breakdown,
        Referrals, Re_referrals, Re_referrals_percent
      ) %>%
        arrange(desc(Re_referrals_percent)),
      colnames = c("Time Period", "Geographical Breakdown", "Referrals in the year",
                   "Re-referrals within 12 months of a previous referral", "Re-referrals within 12 months (%)" ),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #cin referral table by LA
  output$table_cin_referral_la <- renderDataTable({
    if (input$select_geography_o1 == "Regional") {
      if (input$geographic_breakdown_o1 == "London") {
        # Include both Inner London and Outer London
        location <- location_data %>%
          filter(region_name %in% c("Inner London", "Outer London")) %>%
          pull(la_name)
      } else {
        # Get the la_name values within the selected region_name
        location <- location_data %>%
          filter(region_name == input$geographic_breakdown_o1) %>%
          pull(la_name)
      }
      
      data <- cin_referrals %>%
        filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
        select(time_period, geo_breakdown,
               Referrals, Re_referrals, Re_referrals_percent)  %>%
        arrange(desc(Re_referrals_percent))
      
    } else if (input$select_geography_o1 %in% c("Local authority", "National")) {
      data <- cin_referrals  %>% filter(geographic_level == 'Local authority', time_period == max(cin_referrals$time_period)) %>% select(
        time_period, geo_breakdown,
        Referrals, Re_referrals, Re_referrals_percent) %>%
        arrange(desc(Re_referrals_percent))
    }
    
    datatable(
      data,
      colnames = c("Time Period", "Geographical Breakdown", "Referrals in the year",
                   "Re-referrals within 12 months of a previous referral", "Re-referrals within 12 months (%)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  
  #cin referral plot by region
  output$plot_cin_referral_reg <- plotly::renderPlotly({
    ggplotly(
      plot_cin_referral_reg() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  #cin referral chart by LA
  output$plot_cin_referral_la <- plotly::renderPlotly({
    ggplotly(
      plot_cin_referral_la(input$geographic_breakdown_o1, input$select_geography_o1) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  
  
  
  
  # Don't touch the code below -----------------------

  observeEvent(input$go, {
    toggle(id = "div_a", anim = T)
  })


  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  })

  # Download the underlying data button
  output$download_data <- downloadHandler(
    filename = "shiny_template_underlying_data.csv",
    content = function(file) {
      write.csv(dfRevBal, file)
    }
  )

  # Add input IDs here that are within the relevant drop down boxes to create dynamic text
  output$dropdown_label <- renderText({
    paste0("Current selections: ", input$selectPhase, ", ", input$selectArea)
  })


  # # Reactive value for last selected tab that isn't user guide
  # backTo <- reactive({
  #   if (input$navlistPanel != "user_guide") {
  #     return(input$navlistPanel)
  #   }
  # })
  #
  # # Observe return button click
  # observeEvent(input$go_back, {
  #   updateTabsetPanel(session, "navlistPanel", selected = "backTo")
  # })

  observeEvent(input$tutorial, {
    updateTabsetPanel(session, "navlistPanel", selected = "user_guide")
  })

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}

