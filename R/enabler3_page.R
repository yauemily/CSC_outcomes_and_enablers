enabler3_tab <- function(){
  tabPanel(
    value = "enabler3_page",
    "Enabler 3",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 3: Leaders drive conditions for effective practice")
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
              "Spending",
              fluidRow(
                p("testing")
              )
            ),
            tabPanel(
              "Culture focused on outcomes from children and families and continually improving services",
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