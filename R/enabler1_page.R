enabler1_tab <- function() {
  tabPanel(
    value = "enabler1_page",
    "Enabler 1",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 1: The workforce is equipped and effective.")
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
            )
          )
        )
      ),
      br(),
      gov_row(
        br(),
        p(htmlOutput("choices_confirmation_text")),
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
                    title = "Turnover Rate (FTE)",
                    value = htmlOutput("s_w_headline_txt"),
                    #showcase = plotlyOutput("stat_test_plot"),
                    #showcase_layout = "bottom"
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Agency Worker Rate (FTE)",
                    value = htmlOutput("agency_rate_txt")
                    #value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(agency_worker_rate_fte_perc),"%")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Vacancy Rate (FTE)",
                    value = htmlOutput("vacancy_rate_txt")
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
                    
                    p("Prioritising a stable workforce allows children, young people and families to maintain consistent relationships with practitioners."), 
                     # style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),
                    
                    insert_text(inputId = "social_work_turnover_definition", text = paste(
                      "<b>","Turnover rate", "</b><br>",
                      "The turnover rate is calculated as the number of FTE (full-time equivalent) 
                                  children and family social worker leavers in the year divided by the number of FTE children and 
                                  family social workers in post at the 30 September."
                    )),
                    # p("plots go here"),
                    plotlyOutput("plot_s_w_turnover"),
                    br(),
                    #plotlyOutput("plotly_test"),
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
                          tags$br(),
                          "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                      )
                    ),
                  ),
                  
                  
                  # Agency Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Agency Rates"),
                    p("Prioritising a stable and permanent workforce allows children, young people and families to maintain consistent relationships with practitioners.
                           Agency workers should only be used as per the national agency rules from Autumn 2024."),
                    
                    # insert_text(inputId = "agency_rate_definition", text = paste(
                    #   "<b>Agency Workers</b> are child and family social workers not directly paid by the local authority. These may be social workers who are paid by an agency rather than the local authority or who are self-employed.",
                    #   "<br>","<br>",
                    #   "The <b>FTE agency worker rate</b> is calculated as the number of FTE agency staff working as (children and family) social workers at 30 September divided by the sum of the number of FTE agency staff working as social workers at 30 September and the number of FTE social workers."
                    # )),
                    
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
                    
                    details(
                      inputId = "agency_worker_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(tags$b("Agency workers"), " are child and family social workers not directly paid by the local authority. These may be social workers who are paid by an agency rather than the local authority or who are self-employed."),
                          tags$li("The ", tags$b("FTE agency worker rate"), " is calculated as the number of FTE agency staff working as (children and family) social workers at 30 September divided by the sum of the number of FTE agency staff working as social workers at 30 September and the number of FTE social workers."),
                          tags$br(),
                          p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                        )
                      )
                    ), 
                  ),
                  
                  # Vacancy Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Vacancy Rates"),
                    p("A workforce strategy should develop and maintain an effective workforce. With a well-supported workforce vacancy rates should remain low."),
                    # insert_text(inputId = "vacancy_rates_rationale", text = paste(
                    #   "A workforce strategy should develop and maintain an effective workforce.
                    #        With a well-supported workforce vacancy rates should remain low."
                    # )),
                    #p("The vacancy rate is calculated as the number of FTE vacancies at 30 September divided by the sum of the number of FTE vacancies at 30 September and the number of FTE social workers at 30 September."),
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
                    ),
                    details(
                      inputId = "vacancy_rate_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(tags$b("Vacancies"), " are defined as any FTE (child and family social worker) vacancy within a local authority’s organisational structure, including vacancies that are not being actively recruited for, and those covered by agency workers."),
                          tags$li("The ", tags$b("vacancy rate"), " is calculated as the number of FTE vacancies at 30 September divided by the sum of the number of  FTE vacancies at 30 September and the number of FTE social workers at 30 September."),
                          tags$br(),
                          p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                        )
                      )
                    )
                  ) 
                ),
              ),
            ),
            # Second Domain - "quality of support for children and families"
            tabPanel(
              "Quality of support for children and families",
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "Social Worker Caseloads (FTE)",
                    value = htmlOutput("caseload_txt")
                      #paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(caseload_fte))
                  ),
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  gov_row(
                    h2("Social worker caseloads"),
                    #br(),
                    p("Ensuring that practitioners have an appropriate caseload supports recruitment and 
                        retention and allows practitioners to deliver impactful services."),
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
                    ),
                    details(
                      inputId = "caseload_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("A", tags$b("case"), " is defined as any person allocated to a named social worker, where the work involves child and family social work. Cases may be held by social workers regardless of their role in the organisation and not just those specifically in a ‘case holder’ role."),
                          tags$li("The ", tags$b("average caseload"), " is calculated as the total number of cases held by FTE social workers (including agency workers) in post at 30 September divided by the number of FTE social workers (including agency workers) in post at 30 September that hold one or more cases."),
                          tags$br(),
                          p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                        )
                      )
                    )
                    
                  ),
                )
              )
            ),
            
            # Third tab panel
            tabPanel(
              "Societal and cultural awareness and diversity",
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "Asian Ethnicity Social Workers",
                    value = htmlOutput("asian_ethnicity_txt")
                    #paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(caseload_fte))
                  ),
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Black Ethnicity Social Workers",
                    value = htmlOutput("black_ethnicity_txt")
                    #value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(agency_worker_rate_fte_perc),"%")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Mixed Ethnicity Social Workers",
                    value = htmlOutput("mixed_ethnicity_txt")
                    #value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(agency_worker_rate_fte_perc),"%")
                  )
                ),
              ),
              
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "Other Ethnicity Social Workers",
                    value = htmlOutput("other_ethnicity_txt")
                    #paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(caseload_fte))
                  ),
                ),
                column(
                  width = 4,
                  value_box(
                    title = "White Ethnicity Social Workers",
                    value = htmlOutput("white_ethnicity_txt")
                    #value = paste0(workforce_data %>% filter(time_period == "2022" & geographic_level == "National") %>% select(agency_worker_rate_fte_perc),"%")
                  )
                ),
              ),
              
              fluidRow(
                column(
                  width = 12,
                  gov_row(
                    h2("Ethnic diversity of workforce, and levels of seniority"),
                    p("A diverse workforce, across all levels, should enable practice which reflects the cultural, linguistic, and religious needs of the communities’ practitioners serve."),
                  )
                )
              ),
              gov_row(
                br(),
                p("plot goes here")
              )
            )
          )
        )
      )
    )
  )
}
