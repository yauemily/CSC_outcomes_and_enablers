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
                inputId = "select_geography_o1",
                label = "Select a geographical level:",
                choices = distinct(dropdown_choices['geographic_level']),
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            column(
              width = 6,
              conditionalPanel(condition = "input.select_geography_o1 != 'National'",selectizeInput(
                inputId = "geographic_breakdown_o1",
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
          gov_row(
            conditionalPanel(condition = "input.select_geography_o1 != 'National'",
                             column(
                               width = 3,
                               checkbox_Input(
                                 inputId = "national_comparison_checkbox_o1",
                                 cb_labels = "Compare with National",
                                 checkboxIds = "Yes_national",
                                 label = "",
                                 hint_label = NULL,
                                 small = TRUE
                               )
                             )),
            conditionalPanel(
              condition = "(input.select_geography_o1 == 'Local authority')",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o1",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              ),
            )
          )
        )
      ),
      br(),
      gov_row(
        br(),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 != 'Richmond upon Thames' && input.geographic_breakdown_o1 != 'West Northamptonshire')",
          p(htmlOutput("outcome1_choice_text1"),htmlOutput("outcome1_choice_text2") )),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 == 'Richmond upon Thames')",
          p("Please select ", strong("Kingston upon Thames"), " to view jointly reported statistics for Kingston upon Thames and Richmond upon Thames.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 == 'Kingston upon Thames')",
          p("Kingston upon Thames and Richmond upon Thames submit a joint workforce return each year and their data is reported together against Kingston upon Thames.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 == 'North Northamptonshire')",
          p("North Northamptonshire and West Northamptonshire submitted a joint workforce return in 2021 and onwards, and their data is reported together against North Northamptonshire. ") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 == 'West Northamptonshire')",
          p("Please select ", strong("North Northamptonshire"), ", or Northamptonshire for pre-2021 data, to view jointly reported statistics for North Northamptonshire and West Northamptonshire. ") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o1 == 'Northamptonshire')",
          p("To view 2021 and onwards data select ", strong("North Northamptonshire"), ". Northamptonshire local authority was replaced with two new unitary authorities, North Northamptonshire and West Northamptonshire, in April 2021.") ),
      ),
      gov_row(
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
                      "The CLA rate is calculated as the number of children that are looked after per 10,000 children in the general population."
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
              fluidRow(
                h2("CLA Rates by Region"),
                p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top."),
                br(),
                plotlyOutput("plot_cla_rate_reg"),
              ),
              fluidRow(
                details(
                  inputId = "tbl_cla_rate_reg",
                  label = "View chart as a table",
                  help_text = (
                    dataTableOutput("table_cla_rate_reg")
                  )
                )
              ),
              h2("CLA Rates by Local Authority"),
              p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region."),
              p(sprintf("The graph represents data from %s.", max(cla_rates$time_period))),
              br(),
              plotlyOutput("plot_cla_rate_la"),
              br(),
              br(),
              details(
                inputId = "tbl_cla_rate_la",
                label = "View chart as a table",
                help_text = (
                  dataTableOutput("table_cla_rate_la")
                )
              ),
              fluidRow(
                h2("CLA Rates by UASC Status"),
                br(),
                plotlyOutput("plot_uasc"),
              ),
            ),
            tabPanel(
              "Access to support and getting help",
              fluidRow(
                p("testing")
              )
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