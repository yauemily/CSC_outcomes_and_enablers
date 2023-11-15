# This page is a hidden page for the User Guide
tutorialPanel <- function(){
  tabPanel(
    value = "user_guide",
      "User Guide",
      gov_main_layout(
        # gov_row(
        #   actionButton("go_back", "Go Back", icon = icon("arrow-left", class = NULL, lib = "font-awesome"), style = "margin-top: 10px;float:  right;"),
        # ),
      gov_row(
        column(
          width = 12,
          h1("Dashboard User Guide - EDIT THIS PAGE"),
          h5("How to use this dashboard", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
          "Use the navigation bar on the left to select which tab you want to view.",
          tags$br(),
          tags$h5("Dashboard Structure", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
          tags$ul(
            tags$li(tags$b("Introduction - "), "a basic introduction to the tool and provides links to the research report and the technical report."),
            tags$li(tags$b("Earnings trajectory - "), "looks at the average earnings of individuals in employment. You can build on the
          presented plots by selecting breakdowns you wish to see and compare. There is also a function to compare with the overall average for all individuals."),
          tags$li(tags$b("Main activities - "), "looks at the main activities for individuals. You can compare the main activities by selecting multiple breakdowns."),
          tags$li(tags$b("Accessibility statement - "), "contains the accessibility statement for this tool."),
          tags$li(tags$b("Feedback and suggestions - "), "contains links for a user feedback form and a form for reporting any bugs or errors found within the tool.")
          ),
           tags$br(),
          tags$h5("Interactive Plots User Guide", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
          tags$ul(
            tags$li("Hover over lines/bars in the plot to see specific values."),
            tags$li("The bar along the top of the plots contains extra interactive features such as download as PNG and/or resize plot and zoom."),
            tags$li("Click \"View full screen\" to display the plots as full screen."),
            tags$li("To exit full screen, click \"Exit full screen\" or press the escape key (Esc)."),
            # tags$br(),
            tags$h6("Using the Key", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px; font-weight: bold"),
            tags$li("Double clicking a line/value in the key will isolate the value in the plot."),
            tags$li("Double clicking the same value again will restore the original plot"),
            tags$li("Single clicking a line/value in the key will remove that line/value from the plot")
          )
        )
      )
      
    )
    )
}