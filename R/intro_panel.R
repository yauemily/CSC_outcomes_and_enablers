# This page is a hidden page for the User Guide
introductionPanel <- function() {
  tabPanel(
    value = "intro_panel",
    title = "Introduction",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          heading_text("Introduction - EDIT THIS PAGE", size = "l"),
          h2("Description on what this dashboard is for - does it accompany a research report?"),
          h2("insert some links to the report or data etc"),
          h2("Example links:"),
          insert_text(inputId = "tech_link", text = paste(
            "You can view the published report and data tables at:", "<br>", a(href = "https://www.gov.uk/government/publications/post-16-education-and-labour-market-activities-pathways-and-outcomes-leo", "Longitudinal Education Outcomes (LEO): post-16 education and labour market activities, pathways and outcomes.", style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;"),
            "<br>", "For more information, please refer to the technical report: ", "<br>",
            a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/993969/Technical_Report_for_Education_and_Labour_Market_Pathways_of_Individuals__LEO_.pdf", "Technical Report for Education and Labour Market Pathways of Individuals (LEO)", style = "font-family: GDS Transport, arial, sans-serif; font-size :17px;")
          ))
        ),
      )
    )
  )
}
