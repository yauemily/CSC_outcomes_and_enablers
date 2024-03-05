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
              selectizeInput(
                inputId = "select_geography_o2",
                label = "Select a geographical level:",
                choices = unique(workforce_data %>% pull('geographic_level')),
                selected = NULL,
                multiple = FALSE,
                options = NULL
              )
            ),
            column(
              width = 6,
              conditionalPanel(condition = "input.select_geography_o2 != 'National'",selectizeInput(
                inputId = "geographic_breakdown_o2",
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
            conditionalPanel(condition = "input.select_geography_o2 != 'National'",
                             column(
                               width = 3,
                               checkbox_Input(
                                 inputId = "national_comparison_checkbox_o2",
                                 cb_labels = "Compare with National",
                                 checkboxIds = "Yes_national",
                                 label = "",
                                 hint_label = NULL,
                                 small = TRUE
                               )
                             )),
            conditionalPanel(
              condition = "(input.select_geography_o2 == 'Local authority')",
              column(
                width = 3,
                checkbox_Input(
                  inputId = "region_comparison_checkbox_o2",
                  cb_labels = "Compare with Region",
                  checkboxIds = "Yes_region",
                  label = "",
                  hint_label = NULL,
                  small = TRUE
                )
              ),
            )
          ))
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
        p(htmlOutput("outcome2_choice_text1"),htmlOutput("outcome2_choice_text2")),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o2 == 'Northamptonshire')",
          p("To view 2021 and onwards data select ", strong("North Northamptonshire"),"or", strong("West Northamptonshire"),". Northamptonshire local authority was replaced with two new unitary authorities, North Northamptonshire and West Northamptonshire, in April 2021.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o2 == 'Poole')",
          p("To view 2020 and onwards data select ", strong("Bournemouth, Christchurch and Poole"),". Bournemouth, Christchurch and Poole local authority was formed in April 2019.") ),
        conditionalPanel(
          condition = "(input.geographic_breakdown_o2 == 'Bournemouth')",
          p("To view 2020 and onwards data select ", strong("Bournemouth, Christchurch and Poole"),". Bournemouth, Christchurch and Poole local authority was formed in April 2019.") ),
      ),
      
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
              ),
              accordion(
                accordion_panel(
                  "Percentage of Child Protection Plans (CPP) longer than 2 years, and repeat CPP (within 12 months)",
                ),
                accordion_panel(
                  "Percentage of repeat CPP (within 12 months)",
                )
                , open = FALSE
              )
            )
          )
        )
      )
    )
  )
}