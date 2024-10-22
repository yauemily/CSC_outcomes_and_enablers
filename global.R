# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinytitle))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))
shhh(library(shinytest2))
shhh(library(rstudioapi))
shhh(library(bslib))
shhh(library(reshape2))
shhh(library(tidyverse))

# shhh(library(shinya11y))

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# This line enables bookmarking such that input choices are shown in the url.
enableBookmarking("url")

# cs_num ----------------------------------------------------------------------------
# Comma separating function



cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  r_scripts <- eval(styler::style_dir("R/")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, r_scripts, test_scripts)
  return(script_changes)
}


# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

site_title <- "DfE Shiny Template"
site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"
sites_list <- c(site_primary, site_overflow) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Statistical publication" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/" # Update with parent publication link
google_analytics_key <- "Z967JJVQQX"


source("R/read_data.R")

#read in the definitions data
# NOT important
definitions <- read_definitions()
colnames(definitions) <- c("Outcome/Enabler", "Domain", "Indicator", "Rationale/Description")
definitions <- definitions[,1:4]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read in the workforce data
workforce_data <- read_workforce_data()
location_data <- GET_location() # fact table linking LA to its region
location_data_workforce <- GET_location_workforce() # fact table linking LA to its region

#Read in the workforce characteristics data
# workforce_char <- read_workforce_char_data()
workforce_eth <- read_workforce_eth_data()
workforce_eth_seniority <- read_workforce_eth_seniority_data()
population_eth <- read_ethnic_population_data()
combined_ethnicity_data <- merge_eth_dataframes()
cla_rates <- read_cla_rate_data()
cla_placements <- read_cla_placement_data()
combined_cla_data <- merge_cla_dataframes()
#uasc_data <- test_uasc()

#Read in the CIN  data
cin_rates <- read_cin_rate_data()
cin_referrals <- read_cin_referral_data()


#Read in outcome 2 data
ceased_cla_data <- read_outcome2()

#Dropdowns
#choice_breakdown_level <- workforce_data %>% select(geographic_level) %>% filter(geographic_level != "National")%>% distinct()
#choices_LA <- workforce_data %>% filter(geographic_level == "Local authority") %>% select()

#choices_geographic_level <- dropdown_choices %>% select(geographic_level) %>% distinct()

dropdown_choices <- cla_rates #%>%
#   mutate(geo_breakdown = case_when(
#     geographic_level == "National" ~ "National",#NA_character_,
#     geographic_level == "Regional" ~ region_name,
#     geographic_level == "Local authority" ~ la_name
#   )) %>%
#   select(geographic_level, geo_breakdown,turnover_rate_fte_perc,time_period,"time_period","turnover_rate_fte_perc", "absence_rate_fte_perc",
#          "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
#          "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte") %>% distinct()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TEMPLATE code
# Read in the data
dfRevBal <- read_revenue_data()
# Get geographical levels from data
dfAreas <- dfRevBal %>%
  select(
    geographic_level, country_name, country_code,
    region_name, region_code,
    la_name, old_la_code, new_la_code
  ) %>%
  distinct()

choicesLAs <- dfAreas %>%
  filter(geographic_level == "Local authority") %>%
  select(geographic_level, area_name = la_name) %>%
  arrange(area_name)

choicesAreas <- dfAreas %>%
  filter(geographic_level == "National") %>%
  select(geographic_level, area_name = country_name) %>%
  rbind(dfAreas %>% filter(geographic_level == "Regional") %>% select(geographic_level, area_name = region_name)) %>%
  rbind(choicesLAs)

choicesYears <- unique(dfRevBal$time_period)

choicesPhase <- unique(dfRevBal$school_phase)

expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details", id = inputId,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}
