enabler1_tab <- function(){
  tabPanel(
    "Enabler 1",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 1: The workforce is equipped and effective."),
          br(),
          br(),
          column(
            width = 12,
            tabsetPanel(
              id = "enabler1_panels",
              tabPanel(
                "Workforce Stability",
                #fluidRow(
                #  dataTableOutput("enabler1_d1_tab")
                  #shinyGovstyle::govTable("test_tab", definitions, "testtable", "s")
                #),
                fluidRow(
                  column(width = 12,
                         #Social Worker Turnover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                         h2("Social Worker Turnover"),
                         insert_text(inputId = "social_work_turnover_rationale", text = paste(
                           "Prioritising a stable workforce allows children, young people and families to maintain consistent relationships with practitioners.")),
                        p("The turnover rate is calculated as the number of FTE (full-time equivalent) children and family social worker leavers in the year divided by the number of FTE children and family social workers in post at the 30 September."),
                         
                        
                        
                        #Agency Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                         h2("Agency Rates"),
                           insert_text(inputId = "agency_rates_rationale", text = paste(
                             "Prioritising a stable and permanent workforce allows children, young people and families to maintain consistent relationships with practitioners. 
                           Agency workers should only be used as per the national agency rules from Autumn 2024.")),
                        p("The FTE agency worker rate is calculated as the number of FTE agency staff working as (children and family) social workers at 30 September divided by the sum of the number of FTE agency staff working as social workers at 30 September and the number of FTE social workers."),
                         
                         
                        #Vacancy Rates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                         h2("Vacancy Rates"),
                           insert_text(inputId = "vacancy_rates_rationale", text = paste(
                             "A workforce strategy should develop and maintain an effective workforce.
                           With a well-supported workforce vacancy rates should remain low.")),
                         p("The vacancy rate is calculated as the number of FTE vacancies at 30 September divided by the sum of the number of FTE vacancies at 30 September and the number of FTE social workers at 30 September."),
                         
                         
                           ),
                         ),
                  ),
                tabPanel(
                  "Quality of support for children and families",
                  fluidRow(
                    column(
                      width = 12,
                      h2("indicator 2 (h2)"),
                      p("This is the standard paragraph style for adding guiding info around data content."),
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
          ),
        )))
      }
