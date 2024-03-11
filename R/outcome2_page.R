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
          layout_columns(
            selectizeInput(
              inputId = "select_geography_o2",
              label = "Select a geographical level:",
              choices = unique(workforce_data %>% pull('geographic_level')),
              selected = NULL,
              multiple = FALSE,
              options = NULL
            ),
            conditionalPanel(condition = "input.select_geography_o2 != 'National'",
                             selectizeInput(
                               inputId = "geographic_breakdown_o2",
                               label = "Select a breakdown: ",
                               choices = NULL,
                               selected = NULL,
                               multiple = FALSE,
                               options = NULL
                             )),
            col_widths = c(4,8)
          ),
          layout_columns(
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
              )), col_widths = c(4,8)
          )
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
        div(
          tabsetPanel(
            id = "outcome2_panels",
            type = "tabs",
            tabPanel(
              "Families engaging and receiving support from their family network",
              fluidRow(
                br()
              ),
              fluidRow(
                column(
                  width = 6,
                  value_box(
                    title = "Percentage of children who cease being looked after due to moving into Special Guardianship Order (SGO)",
                    value = htmlOutput("SGO_headline_txt")
                  )
                ),
                column(
                  width = 6,
                  value_box(
                    title = "Percentage of children who cease being looked after due to moving into Residence order or Child Arrangement Order (CAO)",
                    value = htmlOutput("CAO_headline_txt")
                  )
                ),
                br(),
                p("Unlocking family networks and kinship carers can be a key source of support where families are experiencing challenges. 
                  Moving children from care arrangements to a SGO or CAO shows that kinship care is being prioritised where children cannot safely live with their parents.")
              ),
              
              accordion(
                accordion_panel(
                  "Percentage of children who cease being looked after due to moving into Special Guardianship Order (SGO)",
                  gov_row(
                    h2("Special Guardianship Order (SGO)"),
                    p("Unlocking family networks and kinship carers can be a key source of support where families are experiencing challenges. 
                  Moving children from care arrangements to a SGO or CAO shows that kinship care is being prioritised where children cannot safely live with their parents."),
                  br(),
                  plotlyOutput("SGO_time_series"),
                  br(),
                  details(
                    inputId = "tbl_sgo_ceased_cla",
                    label = "View chart as table",
                    help_text = (
                      dataTableOutput("table_sgo_ceased")
                    )
                  ),
                  details(
                    inputId = "sgo_info",
                    label = "Additional information:",
                    help_text = (
                    p("For more information on the data and definitions, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/children-looked-after-in-england-including-adoptions/data-guidance", "Children looked after in England including adoptions guidance."),
                      tags$br(),
                      "For more informayion on the methodology, please refer to the", a(href = "https://explore-education-statistics.service.gov.uk/methodology/children-looked-after-in-england-including-adoptions", "Children looked after in Englad including adoptions methodology.")))
                  )
                  ),
                  gov_row(
                    h2("Special Guardianship Order (SGO) by region"),
                    p("This is a static chart and will not react to geographical level and breakdown selected in the filters at the top.
                      
                      The graph represents data from 2023."),
                    br(),
                    plotlyOutput("plot_sgo_ceased_reg"),
                    br(),
                    details(
                      inputId = "tbl_sgo_ceased_cla_reg",
                      label = "View chart as table",
                      help_text = (
                        dataTableOutput("table_sgo_ceased_reg")
                      )
                    )
                  ),
                  gov_row(
                    h2("Special Guardianship Order (SGO) by local authority"),
                    p("This chart is reactive to the Local Authority and Regional filters at the top and will not react to the National filter. The chart will display all Local Authorities overall or every Local Authority in the selected Region.

The graph represents data from 2023."),
                    br(),
                    plotlyOutput("plot_SGO_la"),
                    br(),
                    details(
                      inputId = "tbl_sgo_ceased_la",
                      label = "View chart as table",
                      help_text = (
                        dataTableOutput("table_sgo_la")
                      )
                    )
                  )
                ),
                accordion_panel(
                  "Percentage of children who cease being looked after due to moving into Care Arrangement Order (CAO)",
                  gov_row(
                    h2("Residence order or Child Arrangement Order (CAO)"),
                    p("Unlocking family networks and kinship carers can be a key source of support where families are experiencing challenges. 
                  Moving children from care arrangements to a SGO or CAO shows that kinship care is being prioritised where children cannot safely live with their parents."),
                  br(),
                  plotlyOutput("CAO_time_series"),
                  br(),
                  details(
                    inputId = "tbl_cao_ceased_cla",
                    label = "View chart as table",
                    help_text = (
                      dataTableOutput("table_cao_ceased")
                    )
                  ),
                  ),
                  gov_row(
                    h2("Residence order or Child Arrangement Order (CAO) by region"),
                    p("text"),
                    br(),
                    plotlyOutput("plot_cao_ceased_reg"),
                    br(),
                    details(
                      inputId = "table_cao_ceased_reg",
                      label = "View chart as table",
                      help_text = (
                        dataTableOutput("table_cao_ceased_reg")
                      )
                    ),
                  ),
                  gov_row(
                    h2("Residence order or Child Arrangement Order (CAO) by local authority"),
                    p("text"),
                    br(),
                    plotlyOutput("plot_cao_la"),
                    br(),
                    details(
                      inputId = "table_cao_la",
                      label = "View chart as table",
                      help_text = (
                        dataTableOutput("table_cao_la")
                      )
                    ),
                  )
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