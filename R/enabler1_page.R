enabler1_tab <- function() {
  tabPanel(
    value = "enabler1_page",
    "Enabler 1",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 1: The workforce is equipped and effective."),
        )
      ),
      gov_row(
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          gov_row(
            column(
              width = 6,
              selectizeInput(
                inputId = "select_geography",
                label = "Select a geographical level:",
                choices = distinct(dropdown_choices['geographic_level']),
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            column(
              width = 6,
              selectizeInput(
                inputId = "geographic_breakdown",
                label = "Select a breakdown: ",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            #textOutput("choice_text_test")
          )
        )
      ),
      br(),
      # gov_row(
      #   value_box(
      #     title = "national stats test",
      #     value = workforce_data
      #   )
      # ),
      gov_row(
        br(),
        div(
          tabsetPanel(
            id = "enabler1_panels",
            type = "tabs",
            tabPanel(
              "Workforce Stability",
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "Turnover rate (FTE) in 2022",
                    value = textOutput("s_w_headline_txt")
                  ),
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Agency worker rate (FTE) in 2022",
                    value = textOutput("agency_rate_txt")
                    #value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(agency_worker_rate_fte_perc),"%")
                  ),
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Vacancy rate (FTE) in 2022",
                    value = textOutput("vacancy_rate_txt")
                    #value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(vacancy_rate_fte_perc),"%")
                  )
                ),
                br(),
              ),
              fluidRow(
                column(
                  width = 12,
                  # Social Worker Turnover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Social Worker Turnover"),
                    
                    p("Prioritising a stable workforce allows children, young people and families to maintain consistent relationships with practitioners.", 
                      style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),
                    
                    insert_text(inputId = "social_work_turnover_definition", text = paste(
                      "<b>","Turnover rate", "</b><br>",
                      "The turnover rate is calculated as the number of FTE (full-time equivalent) 
                                  children and family social worker leavers in the year divided by the number of FTE children and 
                                  family social workers in post at the 30 September."
                    )),
                    # p("plots go here"),
                    plotlyOutput("plot_s_w_turnover"),
                    br(),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_s_w_turnover",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_s_w_turnover")
                      )
                    ),
                    
                    #expandable for the additional info links
                    details(
                      inputId = "turnover_info",
                      label = "Additional information:",
                      help_text = (
                        p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                          "<br>",
                          "For more information on the methodology, please refer to the",a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                      )
                    ),
                  ),
                  
                  
                  # Agency Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Agency Rates"),
                    p("Prioritising a stable and permanent workforce allows children, young people and families to maintain consistent relationships with practitioners.
                           Agency workers should only be used as per the national agency rules from Autumn 2024."),
                    
                    insert_text(inputId = "agency_rate_definition", text = paste(
                      "<b>Agency Workers</b> are child and family social workers not directly paid by the local authority. These may be social workers who are paid by an agency rather than the local authority or who are self-employed.",
                      "<br>",
                      "The <b>FTE agency worker rate</b> is calculated as the number of FTE agency staff working as (children and family) social workers at 30 September divided by the sum of the number of FTE agency staff working as social workers at 30 September and the number of FTE social workers."
                    )),
                    
                    p("The FTE agency worker rate is calculated as the number of FTE agency staff working as (children and family) social workers at 30 September divided by the sum of the number of FTE agency staff working as social workers at 30 September and the number of FTE social workers."),
                    br(),
                    plotlyOutput("plot_agency_worker"),
                    br(),
                    #p("plots go here"),
                    br(),
                    details(
                      inputId = "table_agency_worker",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_agency_worker")
                      )
                    ),
                  ), 
                  
                  
                  # Vacancy Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Vacancy Rates"),
                    insert_text(inputId = "vacancy_rates_rationale", text = paste(
                      "A workforce strategy should develop and maintain an effective workforce.
                           With a well-supported workforce vacancy rates should remain low."
                    )),
                    p("The vacancy rate is calculated as the number of FTE vacancies at 30 September divided by the sum of the number of FTE vacancies at 30 September and the number of FTE social workers at 30 September."),
                    br(),
                    plotlyOutput("plot_vacancy_rate"),
                    #p("plots go here"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_vacancy_rate",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_vacancy_rate")
                      )
                    )
                  ) 
                ),
              ),
            ),
            tabPanel(
              "Quality of support for children and families",
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "Social worker caseloads (FTE) in 2022",
                    value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(caseload_fte))
                  ),
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Social worker caseloads"),
                  br(),
                  fluidRow(
                    card(
                      style = "border: 2px solid #1d70b8; border-radius: 4px;",
                      card_header(
                        class = "bg-dark-blue",
                        "Rationale/Description"
                      ),
                      card_body(
                        style = "font-family: GDS Transport, arial, sans-serif; font-size :17px; padding-left: 4px;",
                        p("Ensuring that practitioners have an appropriate caseload supports recruitment and 
                        retention and allows practitioners to deliver impactful services.") 
                      )),
                    br(),
                    insert_text(inputId = "Social_worker_caseload_def", text = paste(
                      "<b>","Cases","</b>",
                      "<br>",
                      "A case is defined as any person allocated to a named social worker, 
                      where the work involves child and family social work. Cases may be held by social workers 
                      regardless of their role in the organisation and not just those specifically in a ‘case holder’ role.",
                      "<br>","<br>",
                      "<b>","Average caseload calculation","</b>",
                      "The average caseload is calculated as the total number of cases held by FTE social workers 
                      (including agency workers) in post at 30 September divided by the number of FTE social workers 
                      (including agency workers) in post at 30 September that hold one or more cases."
                    )),
                    
                  ),
                  fluidRow(
                    plotlyOutput("plot_caseload"),
                    br(),
                    plotlyOutput("plot_caseload_test1")
                  ),
                  fluidRow(
                    details(
                      inputId = "tbl_caseload",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_caseload")
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      box(
                        p("box here"),
                        #   width = 12,
                        #   plotlyOutput("colBenchmark")
                      )
                    ),
                    column(
                      width = 6,
                      p("input enabler indicator info here")
                    )
                  )
                )
              )
            ),
            
            # Third tab panel
            tabPanel(
              "Societal and cultural awareness and diversity",
              fluidRow(
                column(
                  width = 12,
                  h2("indicator 3"),
                  p("paragraph"),
                )
              )
            )
          )
        )
      )
    )
  )
}
