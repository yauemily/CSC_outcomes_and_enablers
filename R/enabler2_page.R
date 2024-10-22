enabler2_tab <- function() {
  tabPanel(
    value = "enabler2_page",
    "Enabler 2",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 2: The workforce is equipped and effective.")
        )
      ),
      gov_row(
        div(
          class = "input_box",
          style = "min-height:100%; height = 100%; overflow-y: visible",
          layout_columns(
            selectizeInput(
              inputId = "select_geography_e2",
              label = "Select a geographical level:",
              choices = unique(workforce_data %>% pull('geographic_level')),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(condition = "input.select_geography_e2 != 'National'",selectizeInput(
              inputId = "geographic_breakdown_e2",
              label = "Select a breakdown: ",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              options = NULL
            )),
            col_widths = c(4,8)
          ),
          layout_columns(
            conditionalPanel(condition = "input.select_geography_e2 != 'National'",
                             column(
                               width = 5,
                               checkbox_Input(
                                 inputId = "national_comparison_checkbox_e2",
                                 cb_labels = "Compare with National",
                                 checkboxIds = "Yes_national",
                                 label = "",
                                 hint_label = NULL,
                                 small = TRUE
                               )
                             )),
            conditionalPanel(
              condition = "(input.select_geography_e2 == 'Local authority')",
              column(
                width = 5,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_e2",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              ),
            ),
            col_widths = c(4,8)
          ),
        )
      ),
      br(),
      gov_row(
        br(),
      p(htmlOutput("enabler2_choice_text1"),htmlOutput("enabler2_choice_text2")),
              conditionalPanel(
          condition = "(input.geographic_breakdown_e2 == 'Kingston upon Thames / Richmond upon Thames')",
                   p("Kingston upon Thames and Richmond upon Thames submit a joint workforce return each year, and their data is reported together.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e2 == 'North Northamptonshire / West Northamptonshire')",
          p("North Northamptonshire and West Northamptonshire submitted a joint workforce return in 2021 and onwards, and their data is reported together") ),
      conditionalPanel(
        condition = "(input.geographic_breakdown_e2 == 'Cumbria')",
        p("To view 2023 and onwards data select ", strong("Cumberland"), "or", strong("Westmorland and Furness"), ". Cumbria local authority was replaced with two new unitary authorities, Cumberland and Westmorland and Furness, in April 2023.") ),
              conditionalPanel(
          condition = "(input.geographic_breakdown_e2 == 'Northamptonshire')",
          p("To view 2021 and onwards data select ", strong("North Northamptonshire / West Northamptonshire"), ". Northamptonshire local authority was replaced with two new unitary authorities, North Northamptonshire and West Northamptonshire, in April 2021.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e2 == 'Poole')",
          p("To view 2019 and onwards data select ", strong("Bournemouth, Christchurch and Poole"),". Bournemouth, Christchurch and Poole local authority was formed in April 2019.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_e2 == 'Bournemouth')",
          p("To view 2019 and onwards data select ", strong("Bournemouth, Christchurch and Poole"),". Bournemouth, Christchurch and Poole local authority was formed in April 2019.") ),
        #p(htmlOutput("enabler2_choice_text2")),
        br(),
        div(
          tabsetPanel(
            id = "enabler2_panels",
            type = "tabs",
            tabPanel(
              "Workforce Stability",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Turnover rate (FTE)",
                    value = htmlOutput("s_w_headline_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Agency worker rate (FTE)",
                    value = htmlOutput("agency_rate_txt")
                  )
                ),
                column(
                  width = 4,
                  value_box(
                    title = "Vacancy rate (FTE)",
                    value = htmlOutput("vacancy_rate_txt")
                  )
                ),
                br(),
              ),
              # fluidRow(
              #   column(
              #    width = 12,
              accordion(
                accordion_panel(
                  "Social Worker Turnover",
                  # Social Worker Turnover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Social worker turnover"),
                    
                    p("Prioritising a stable workforce allows children, young people and families to maintain consistent relationships with practitioners."), 
                    # style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),
                    
                    insert_text(inputId = "social_work_turnover_definition", text = paste(
                      "<b>","Turnover rate", "</b><br>",
                      "The turnover rate is calculated as the number of FTE (full-time equivalent) 
                                  children and family social worker leavers in the year divided by the number of FTE children and 
                                  family social workers in post at the 30 September."
                    )),
                    plotlyOutput("plot_s_w_turnover"),
                    br(),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_s_w_turnover",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_s_w_turnover")
                      )
                    ),
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
                  gov_row(
                    h2("Turnover rates by region"),
                    p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_turnover_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_turnover_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_turnover_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Turnover rates by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(workforce_data$time_period))),
                    br(),
                    plotlyOutput("plot_turnover_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_turnover_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_turnover_la")
                      )
                    ),
                  ),
                ),
                accordion_panel(
                  "Agency Rates",
                  # Agency Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Agency rates"),
                    p("Prioritising a stable and permanent workforce allows children, young people and families to maintain consistent relationships with practitioners.
                           Agency workers should only be used as per the national agency rules from Autumn 2024."),
                    br(),
                    plotlyOutput("plot_agency_worker"),
                    br(),
                    br(),
                    details(
                      inputId = "table_agency_worker",
                      label = "View chart as a table",
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
                    )
                  ),
                  gov_row(
                    h2("Agency rates by region"),
                    p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_agency_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_agency_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_agency_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Agency rates by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(workforce_data$time_period))),
                    br(),
                    plotlyOutput("plot_agency_rate_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_agency_rate_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_agency_rate_la")
                      )
                    ),
                  ),
                ),
                accordion_panel(
                  "Vacancy Rates",
                  # Vacancy Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Vacancy rates"),
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
                      label = "View chart as a table",
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
                    ),
                  ),
                  gov_row(
                    h2("Vacancy rates by region"),
                    p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_vacancy_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_vacancy_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_vacancy_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Vacancy rates by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(workforce_data$time_period))),
                    br(),
                    plotlyOutput("plot_vacancy_rate_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_vacancy_rate_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_vacancy_rate_la")
                      )
                    ),
                  ),
                )
                , open = FALSE
              )
            ),
            # Second Domain - "quality of support for children and families" -------------
            tabPanel(
              "Quality of support for children and families",
              fluidRow(
                br(),
              ),
              fluidRow(
                column(
                  width = 4,
                  value_box(
                    title = "Average caseload (FTE)",
                    value = htmlOutput("caseload_txt")
                  ),
                )
              ),
              # fluidRow(
              #   column(
              #     width = 12,
              accordion(
                accordion_panel(
                  "Social worker caseloads",
                  gov_row(
                    h2("Social worker caseloads"),
                    p("Ensuring that practitioners have an appropriate caseload supports recruitment and 
                         retention and allows practitioners to deliver impactful services."),
                    br(),
                    plotlyOutput("caseload_plot"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_caseload",
                      label = "View chart as a table",
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
                  gov_row(
                    h2("Social worker caseloads by region"),
                    p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top."),
                    br(),
                    plotlyOutput("plot_caseload_reg"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_caseload_reg",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_caseload_reg")
                      )
                    ),
                  ),
                  gov_row(
                    h2("Social worker caseloads by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                    p(sprintf("The graph represents data from %s.", max(workforce_data$time_period))),
                    br(),
                    plotlyOutput("plot_caseload_la"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_caseload_la",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_caseload_la")
                      )
                    )
                  )
                ), open = FALSE
              ),
            ),
            
            # Third tab panel -----
            tabPanel(
              "Societal and cultural awareness and diversity",
              fluidRow(
                br(),
              ),
              fluidRow(
                # column(
                # width = 6,
                # value_box(
                #  title = "Social Worker White Ethnic Group",
                #  value = htmlOutput("white_ethnicity_txt")
                #)
                #  ),
                column(
                  width = 6,
                  value_box(
                    title = "Social workers from ethnic minority backgrounds",
                    value = htmlOutput("non_white_txt")
                  )
                )
              ),
              accordion(
                accordion_panel(
                  "Ethnic diversity of workforce",
                  gov_row(
                    h2("Ethnic diversity of workforce"),
                    p("A diverse workforce, across all levels, should enable practice which reflects the cultural, linguistic, and religious needs of the communities’ practitioners serve."),
                    br(),
                   insert_text(inputId = "Ethnicity_definition", text = paste(
                      "<b>","Ethnicity (headcount)", "</b><br>",
                      "Percentage of headcount children and family social workers in post at 30 September by ethnicity group."
                    )),
                    plotlyOutput("plot_ethnicity_rate"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_ethnicity",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_ethnicity_rate")
                      )
                    ),
                    details(
                      inputId = "ethnicity_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(tags$b("Ethnicity groups"), " are based on ethnic origin and are provided on a headcount basis."),
                          tags$li(tags$b("Ethnicity"), sprintf(" was known for 81%% of child and family social workers nationally in %s.", max(workforce_eth$time_period))),
                           tags$li(tags$b("Headcount"), "is a count of all individual children and family social workers, regardless of their working pattern."), 
                          tags$li(tags$b("Ethnic minority backgrounds"), " exclude white British, white Irish, or any other white background."),
                          tags$br(),
                          p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                        )
                      )
                    )
                  )
                  
                ),
                accordion_panel(
                  "Ethnic diversity of workforce vs. general population",
                  gov_row(
                    h2("Ethnic diversity of workforce vs. general population"),
                    br(),
                    plotlyOutput("plot_population_ethnicity_rate"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_population_ethnicity",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_population_ethnicity_rate")
                      )
                    ),
                    details(
                      inputId = "population_ethnicity_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li("Population data is taken from the latest available", a(href = "https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/3","ONS Census data (2021).")), 
                          tags$li(sprintf("The Workforce data comparison uses the latest available collection year in the Workforce diversity dataset (%s).", max(workforce_eth$time_period))),
                          tags$li(tags$b("Ethnicity"), sprintf(" was known for 81%% of child and family social workers nationally in %s.", max(workforce_eth$time_period))),
                          tags$br(),
                          p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                        )
                      )
                    )
                  )
                ),
                accordion_panel(
                  "Ethnic diversity of workforce by seniority level",
                  gov_row(
                    h2("Ethnic diversity of workforce by seniority level"),
                    br(),
                    plotlyOutput("plot_seniority_eth"),
                    br(),
                    br(),
                    details(
                      inputId = "tbl_seniority_eth",
                      label = "View Chart as a table",
                      help_text = (
                        dataTableOutput("table_seniority_eth")
                      )
                    ),
                    details(
                      inputId = "seniority_ethnicity_info",
                      label = "Additional information:",
                      help_text = (
                        tags$ul(
                          tags$li(sprintf("The data used is from the latest available collection year in the Workforce diversity dataset (%s).", max(workforce_eth$time_period))),
                          tags$li(tags$b("Ethnicity"), sprintf(" was known for 81%% of child and family social workers nationally in %s.", max(workforce_eth$time_period))),
                          tags$li("Seniority level relates to social worker role. Manager roles have been grouped and include first line managers, middle managers and senior managers."),
                          tags$li("A Senior Practitioner works in a local authority in a children’s services department as a team leader, supervising social worker or senior social worker."),
                          tags$li("A case holder is a children and family social worker that manages cases, but is not in a manager or senior practitioner role (however, cases can be hold by those not in case holder roles)."),
                          tags$li("Qualified without cases includes all other qualified and registered social workers, including those without cases (for example Independent Reviewing Officer (IRO), Chairs of Child Protection Conferences, Youth Custody worker, Family Support) and those not currently practicing (for example, those in learning and development or quality assurance roles)."),
                          tags$br(),
                          p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-s-social-work-workforce/data-guidance", "Children's social work workforce data guidance."),
                            tags$br(),
                            "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-s-social-work-workforce-methodology", "Children's social work workforce methodology."))
                        )
                      )
                    )
                    
                  )
                ), open = FALSE
              ),
            )
          )
        )
      )
    )
  )
}
