outcome1_tab <- function(){
  tabPanel(
    value = "outcome1_page",
    "Outcome 1",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 1: Children, young people and families stay together and get the help they need")
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
              conditionalPanel(condition = "input.select_geography != 'National'",selectizeInput(
                inputId = "geographic_breakdown",
                label = "Select a breakdown: ",
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = NULL
                #multiple = TRUE,
                #options = list(maxItems = 3)
              )),
            )
          ),
        )
      ),
      br(),
      gov_row(
        p(htmlOutput("outcome1_choice_text1"),htmlOutput("outcome1_choice_text2") ),
      ),
      gov_row(
        br(),
        h2("Confirmation Sentence"),
        br(),
        div(
          tabsetPanel(
            id = "outcome1_panels",
            type = "tabs",
            tabPanel(
              "Family Stability",
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "CLA Rate Per 10,000",
                    value = htmlOutput("cla_rate_headline_txt")
                  )
                ),
                br(),
              ),
              fluidRow(
                column(
                  width = 12,
                  # CLA Rates per 10000 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  gov_row(
                    h2("Rate of new entrants to care, with a breakdown by whether new entrants to care are Unaccompanied AsylumSeeking Children (UASC)"),
                    
                    p("This measures the flow of those children moving into care. Where UASC are placed within an authority, 
                      this will represent an unavoidable increase in numbers of children entering the system. This breakdown is provided for context."), 
                    # style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),
                    
                    insert_text(inputId = "social_work_turnover_definition", text = paste(
                      "<b>","Children looked after (CLA) rate", "</b><br>",
                      "The CLA rate is calculated as the number of children that are looked after per 10000 people in the general population."
                    )),
                    # p("plots go here"),
                    plotlyOutput("plot_cla_rate"),
                    br(),
                    # Expandable for the table alternative
                    details(
                      inputId = "table_cla_rate",
                      label = "View chart as a table",
                      help_text = (
                        dataTableOutput("table_cla_rate")
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
                  )
                )
              ),

            ),
            tabPanel(
              "Access to support and getting help",
              fluidRow(
                br(),
                column(
                  width = 4,
                  value_box(
                    title = "Children In Need Rate Per 10,000 Children",
                    value = htmlOutput("cin_rate_headline_txt")
                  )
                ),
               br(),
              ),
              column(
                width = 12,
                # CIN Rates per 10000 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                gov_row(
                  h2("Rate of Child in Need (CIN)"),
                  
                  p("Helping children to stay together with their families means ensuring the right support is in place at earlier stages of intervention. 
                    Looking at the flow of children who become a CIN will show children being supported by the wider system. Combined with family stability indicators, this will reflect a broad view of flow into and through the children’s social care system."), 
                  # style ="font-family: GDS Transport, arial, sans-serif; font-size :19px; padding-left: 4px;"),
                  
                  insert_text(inputId = "CIN_definition", text = paste(
                    "<b>","Children in Need (CIN) rate", "</b><br>",
                    "Rate of children in need at 31 March, per 10,000 children in the general population."
                  )),
                  # p("plots go here"),
                  plotlyOutput("plot_cin_rate"),
                  br(),
                  # Expandable for the table alternative
                  details(
                    inputId = "table_cin_rate",
                    label = "View chart as a table",
                    help_text = (
                      dataTableOutput("table_cin_rate")
                    )
                  ),
                  #expandable for the additional info links
                  details(
                    inputId = "CIN_info",
                    label = "Additional information:",
                    help_text = (
                      tags$ul(
                        tags$li("Rate of children as at 31 March 2023 assessed as needing help and protection as a result of risks to their devlopment or health."),
                        tags$li("Rates per 10,000 children are calculated based on ONS", a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2021","mid-year population estimates."),  "for children aged 0 to 17 years. The rates for 2022 and 2023 are based on 2021 population estimates which in turn are based on 2021 Census data."),
                        tags$li("The rates for 2023 have been calculated based on 2021 population estimates as 2022 estimates were not available at the time of publication. Therefore, some caution is needed when interpreting the 2023 rates, either in isolation or in comparison with other years. The 2023 rates will be revised as part of the next 2024 publication."),
                        tags$li("Revised population estimates for 2012 to 2020 based on 2021 Census data, to calculate revised 2013 to 2021 rates, were not available at the time of publication. Therefore, some caution is needed when interpreting these rates, either in isolation or in comparison with other years. The 2013 to 2021 rates will be revised as part of the next 2024 publication."),
                        tags$br(),
                      p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/characteristics-of-children-in-need/data-guidance", "Children in need data guidance."),
                        tags$br(),
                        "For more information on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/characteristics-of-children-in-need-methodology", "Children in need methodology."))
                    )
                  )
                ),
                fluidRow(
                  h2("Children In Need Rates by Region"),
                  p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top."),
                  br(),
                  plotlyOutput("plot_cin_rate_reg"),
                ),
                fluidRow(
                  details(
                    inputId = "tbl_cin_rates_reg",
                    label = "View chart as a table",
                    help_text = (
                      dataTableOutput("table_cin_rates_reg")
                    )
                  )
                ),
                h2("CIN Rates by Local Authority"),
                p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
                p(sprintf("The graph represents data from %s.", max(cin_rates$time_period))),
                br(),
                plotlyOutput("plot_cin_rates_la"),
                br(),
                br(),
                details(
                  inputId = "tbl_cin_rates_la",
                  label = "View chart as a table",
                  help_text = (
                    dataTableOutput("table_cin_rates_la")
                   )
                  ),
                )
                ),
              ),
            
                     tabPanel(
              "Child wellbeing and development",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Educational attainment",
              fluidRow(
                p("testing")
              )
            )
          )
        )
      )
    )
  )
}