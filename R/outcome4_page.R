outcome4_tab <- function(){
  tabPanel(
    value = "outcome4_page",
    "Outcome 4",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 4: Children in care and care leavers have stable, loving homes")
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
            id = "outcome4_panels",
            type = "tabs",
            tabPanel(
              "Stability and quality of where a child lives",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Wellbeing of child",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Quality of life for care experienced people",
              fluidRow(
                p("testing")
              )
            ),
          )
        )
      )
    )
  )
}