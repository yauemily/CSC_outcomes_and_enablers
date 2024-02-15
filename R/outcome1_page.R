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