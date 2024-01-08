enabler1_tab <- function(){
  tabPanel(
    value = "enabler1_page",
    "Enabler 1",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Enabler 1: Multi-agency working is prioritised and effective")
        )
      ),
      # gov_row(
      #   div(
      #     class = "input_box",
      #     style = "min-height:100%; height = 100%; overflow-y: visible",
      #     gov_row(
      #       column(
      #         width = 6,
      #         h2("Inputs go here")
      #       ),
      #       column(
      #         width = 6,
      #         h2("Inputs go here")
      #       )
      #     )
      #   )
      # ),
      br(),
      gov_row(
        br(),
        h2("We will work with the sector and other experts to develop indicators this National Framework enabler."),
        br(),
        # div(
        #   tabsetPanel(
        #     id = "outcome3_panels",
        #     type = "tabs",
        #     tabPanel(
        #       "Child safety – general",
        #       fluidRow(
        #         p("testing")
        #       )
        #     ),
        #     tabPanel(
        #       "Child abuse / neglect",
        #       fluidRow(
        #         p("testing")
        #       )
        #     ),
        #     tabPanel(
        #       "Harms outside the home",
        #       fluidRow(
        #         p("testing")
        #       )
        #     )
        #   )
        # )
      )
    )
  )
}