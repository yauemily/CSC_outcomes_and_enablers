outcome2_tab <- function(){
  tabPanel(
    value = "outcome2_page",
    "Outcome 2",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Outcome 2: Children and young people are supported by their family network")
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
            id = "outcome2_panels",
            type = "tabs",
            tabPanel(
              "Families engaging and receiving support from their family network",
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