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
  
  # ---- User input logic --------------------------------------------------------------------
  # Geographic level does not need to be here as it does not need to change depending on other dropdowns
  
  # Geographic breakdown (list of either LA names or Region names)
  observeEvent(eventExpr={input$select_geography},{
      updateSelectizeInput(
        session = session,
        inputId = "geographic_breakdown",
        selected = sort(unique(dropdown_choices[dropdown_choices$geographic_level == input$select_geography, "geo_breakdown"]),decreasing = FALSE)[1],
        choices = sort(unique(dropdown_choices[dropdown_choices$geographic_level == input$select_geography, "geo_breakdown"]),decreasing = FALSE),
        server = TRUE

      )
    }
  )
  
  output$choice_text_test <- renderText({
    c(paste0("you have selected",input$select_geography))
  })
  
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
  headline_plotter <- function(d,x,y){
    info <- getCurrentOutputInfo()
    large <- isTRUE(info$height() > 200)
    
    plot_ly(d, x = x, y = y) %>%
      add_lines(
        color = "white",
        span = I(1),
        #hoverinfo = if (!large) "none",
        fill = 'white',
        alpha = 0.2
      ) %>%
      layout(
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = info$fg()),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  stat_test_plot <- renderPlotly({
    headline_plotter(workforce_data, x = ~time_period, y = ~input$geographic_breakdown)
  })
  
  # plotly_time_series function test: it does work, will need to implement this later
  
  output$plotly_test <- renderPlotly({
    ggplotly(
      plotly_time_series(workforce_data,input$select_geography, input$geographic_breakdown,"turnover_rate_fte_perc")+ylab("Social worker Turnover rate (FTE) (%)") %>%
        config(displayModeBar = F),
      height = 420
    )#+ylab("Social worker Turnover rate (FTE) (%)")
  })
  
  # Enabler 1 Server Logic ----
  # National Checkbox
  
  # observeEvent(eventExpr = {
  #   input$national_comparison_checkbox
  #   input$region_comparison_checkbox
  #   },
  #   {
  #     if(is.null(national_comparison_checkbox) && is.null(region_comparison_checkbox)){
  #       output$testing_checkboxes <- renderText({
  #         paste0("No checkboxes")
  #       })
  #     } else if(!is.null(national_comparison_checkbox) && is.null(region_comparison_checkbox)){
  #       output$testing_checkboxes <- renderText({
  #         paste0("National comparison")
  #       })
  #     } else if (is.null(national_comparison_checkbox) && !is.null(region_comparison_checkbox)){
  #       output$testing_checkboxes <- renderText({
  #         paste0("Regional Comparison")
  #       })
  #     } else{
  #       output$testing_checkboxes <- renderText({
  #         paste0("Both checkboxes")
  #       })
  #     }
  #   }
  #   )
  
  
  output$choices_confirmation_text <- renderText({
    paste0("You have selected a geographic level of ", tags$b(input$select_geography), ", with a specific breakdown of ", tags$b(input$geographic_breakdown), ".")
  })
  
  
  #social worker rate plot and table
  output$s_w_headline_txt <- renderText({
    stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown) %>% select(turnover_rate_fte_perc), nsmall = 1)
    paste0(stat,"%","<br>",#input$geographic_breakdown,"<br>",
           "(",max(workforce_data$time_period),")")
  })
  
  output$plot_s_w_turnover <- plotly::renderPlotly({
    ggplotly(
      plot_social_worker_turnover(input$select_geography, input$geographic_breakdown) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_s_w_turnover <- renderDataTable({
    datatable(
      workforce_data %>% filter(geo_breakdown %in% input$geographic_breakdown) %>% select(
        time_period, geo_breakdown,
        turnover_rate_fte_perc
      ),
      colnames = c("Time Period", "Geographical Breakdown", "Turnover Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  #agency worker rate plot
  output$agency_rate_txt <- renderText({
    stat <- format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown) %>% select(agency_worker_rate_fte_perc), nsmall = 1)
    paste0(stat,"%","<br>", "(",max(workforce_data$time_period),")")
    })
  
  output$plot_agency_worker <- plotly::renderPlotly({
    ggplotly(
      plt_agency_rates(input$select_geography, input$geographic_breakdown) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_agency_worker <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == "Regional") %>% select(
        time_period, geo_breakdown,
        agency_worker_rate_fte_perc
      ),
      colnames = c("Time Period", "Geographical Breakdown", "Agency Worker Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  # Vacancy Rate plot and table
  output$vacancy_rate_txt <- renderText({
    paste0(format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown) %>% select(vacancy_rate_fte_perc), nsmall = 1), "%","<br>", "(",max(workforce_data$time_period),")")
  })
  
  output$plot_vacancy_rate <- plotly::renderPlotly({
    ggplotly(
      plot_vacancy_rate(input$select_geography, input$geographic_breakdown) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$table_vacancy_rate <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == "Regional") %>% select(
        time_period, geo_breakdown,
        vacancy_rate_fte_perc
      ),
      colnames = c("Time Period", "Geographical Breakdown", "Vacancy Rate (FTE) %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })
  
  
  #Caseload
  output$caseload_txt <- renderText({
    paste0(format(workforce_data %>% filter(time_period == max(workforce_data$time_period) & geo_breakdown %in% input$geographic_breakdown) %>% select(caseload_fte), nsmall = 1),"<br>", "(",max(workforce_data$time_period),")")
  })
  
  # output$plotly_caseload <- renderPlotly({
  #   ggplotly(
  #     plotly_time_series_discrete(workforce_data,input$select_geography, input$geographic_breakdown, workforce_data$caseload_fte)+ylab("Social worker Caseload (FTE)") %>%
  #       config(displayModeBar = F),
  #     height = 420
  #   )#+ylab("Social worker Turnover rate (FTE) (%)")
  # })
  # 
  # output$plot_caseload <- plotly::renderPlotly({
  #     ggplotly(
  #       plot_caseloads() %>%
  #       config(displayModeBar = F),
  #     height = 420
  #   )
  #     
  #   }
  # )
  # 
  output$caseload_plot <- plotly::renderPlotly({
    ggplotly(
      plot_caseload_rate(input$select_geography, input$geographic_breakdown)%>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$plot_caseload_test1 <- plotly::renderPlotly({
    ggplotly(
      plot_caseloads_test1() %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_caseload <- renderDataTable({
    datatable(
      workforce_data %>% filter(geographic_level == "Regional") %>% select(
        time_period, geo_breakdown,
        caseload_fte
      ),
      colnames = c("Time Period", "Geographical Breakdown", "Caseload (FTE)"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  # Ethnicity and Diversity Domain
  output$white_ethnicity_txt <- renderText({
    paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
                                           & geo_breakdown %in% input$geographic_breakdown 
                                           & OrgRole == "All children and family social workers") %>% 
                    select(white_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  })
  
  output$non_white_txt <- renderText({
    white_stat = workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
                                          & geo_breakdown %in% input$geographic_breakdown 
                                          & OrgRole == "All children and family social workers") %>% 
      select(white_perc)
    non_white_stat = 100 - as.numeric(white_stat)
    paste0(non_white_stat, "%", "<br>", "(", max(workforce_eth$time_period) ,")")
  })
  
  
  # output$asian_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(asian_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$black_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(black_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$mixed_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(mixed_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$other_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(other_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  # 
  # output$white_ethnicity_txt <- renderText({
  #   paste0(format(workforce_eth %>% filter(time_period == max(workforce_eth$time_period) 
  #                                          & geo_breakdown %in% input$geographic_breakdown 
  #                                          & OrgRole == "All children and family social workers") %>% 
  #                   select(white_perc), nsmall = 1), "%","<br>", "(",max(workforce_eth$time_period),")")
  # })
  
  
  output$plot_ethnicity_rate <- plotly::renderPlotly({
    ggplotly(
      plot_ethnicity_rate(input$geographic_breakdown, input$geographic_level) %>%
        config(displayModeBar = F),
      height = 420
    )
  })
  
  output$table_ethnicity_rate <- renderDataTable({
    datatable(
      workforce_eth %>% 
        filter(geo_breakdown %in% input$geographic_breakdown, OrgRole == 'All children and family social workers') %>% 
        select(time_period, geo_breakdown, white_perc, mixed_perc, asian_perc, black_perc, other_perc),
      colnames = c("Time Period", "Geographical Breakdown", "White %", "Mixed %", "Asian %", "Black %", "Other %"),
      options = list(
        scrollx = FALSE,
        paging = TRUE
      )
    )
  })

  
  # output$enabler1_d1_tab <- renderDataTable({
  #   datatable(
  #     definitions %>% filter(Domain == "Workforce Stability") %>% select("Indicator", "Rationale/Description"), rownames = FALSE,
  #     #shinyGovstyle::govTable("enabler1_d1_tab", definitions, "testtable", "s")
  #   )
  # })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Define server logic to create a box

  # output$boxavgRevBal <- renderValueBox({
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(
  #       (reactiveRevBal() %>% filter(
  #         year == max(year),
  #         area_name == input$selectArea,
  #         school_phase == input$selectPhase
  #       ))$average_revenue_balance,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the latest value for the selected inputs"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$boxpcRevBal <- renderValueBox({
  #   latest <- (reactiveRevBal() %>% filter(
  #     year == max(year),
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #   penult <- (reactiveRevBal() %>% filter(
  #     year == max(year) - 1,
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  # 
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(latest - penult,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the change on previous year"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$boxavgRevBal_small <- renderValueBox({
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(
  #       (reactiveRevBal() %>% filter(
  #         year == max(year),
  #         area_name == input$selectArea,
  #         school_phase == input$selectPhase
  #       ))$average_revenue_balance,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the latest value for the selected inputs"),
  #     color = "orange",
  #     fontsize = "small"
  #   )
  # })
  # 
  # output$boxpcRevBal_small <- renderValueBox({
  #   latest <- (reactiveRevBal() %>% filter(
  #     year == max(year),
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #   penult <- (reactiveRevBal() %>% filter(
  #     year == max(year) - 1,
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  # 
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(latest - penult,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's showing
  #     paste0("This is the change on previous year"),
  #     color = "orange",
  #     fontsize = "small"
  #   )
  # })
  # 
  # output$boxavgRevBal_large <- renderValueBox({
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(
  #       (reactiveRevBal() %>% filter(
  #         year == max(year),
  #         area_name == input$selectArea,
  #         school_phase == input$selectPhase
  #       ))$average_revenue_balance,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's hsowing
  #     paste0("This is the latest value for the selected inputs"),
  #     color = "green",
  #     fontsize = "large"
  #   )
  # })
  # 
  # output$boxpcRevBal_large <- renderValueBox({
  #   latest <- (reactiveRevBal() %>% filter(
  #     year == max(year),
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  #   penult <- (reactiveRevBal() %>% filter(
  #     year == max(year) - 1,
  #     area_name == input$selectArea,
  #     school_phase == input$selectPhase
  #   ))$average_revenue_balance
  # 
  #   # Put value into box to plug into app
  #   valueBox(
  #     # take input number
  #     paste0("£", format(latest - penult,
  #       big.mark = ","
  #     )),
  #     # add subtitle to explain what it's showing
  #     paste0("This is the change on previous year"),
  #     color = "green",
  #     fontsize = "large"
  #   )
  # })

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
