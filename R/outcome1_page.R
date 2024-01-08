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
              h2("Inputs go here")
            ),
            column(
              width = 6,
              h2("Inputs go here")
            )
          )
        )
      ),
      br(),
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
                p("testing")
              )
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