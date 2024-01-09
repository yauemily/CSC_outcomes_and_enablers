outcome3_tab <- function(){
  tabPanel(
    value = "outcome3_page",
    "Outcome 3",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 3: Children and young people are safe in and outside of their home")
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
            id = "outcome3_panels",
            type = "tabs",
            tabPanel(
              "Child safety – general",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Child abuse / neglect",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Harms outside the home",
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