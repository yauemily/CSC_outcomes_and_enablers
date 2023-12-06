# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

# Function to clean column names
colClean <- function(x) {
  colnames(x) <- gsub("\\.", "perc", colnames(x))
  x
}

# function to convert str columns into numerical columns
convert_perc_cols_to_numeric <- function(x) {
  suppressWarnings({perc_cols <- grep("fte", colnames(x))
  x[, perc_cols] <- apply(x[, perc_cols], 2, function(x) as.numeric(as.character(x)))})
  return(x)
}

# sample data functions we dont need this~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_revenue_data <- function(file = "data/la_maintained_schools_revenue_reserve_final.csv") {
  # This reads in an example file. For the purposes of this demo, we're using the
  # latest LA expenditure data downloaded from the EES release.
  dfRevenue <- read.csv(file)
  # The time period column name has some non-ascii characters so we're just going to rename it here.
  colnames(dfRevenue)[1] <- "time_period"
  dfRevenue <- dfRevenue %>% mutate(
    year = as.numeric(paste0("20", substr(format(time_period), 5, 6))),
    area_name = case_when(
      geographic_level == "National" ~ country_name,
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    )
  )
  return(dfRevenue)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test not important
read_definitions <- function(file = "data/definitions.csv") {
  definitions <- read.csv(file)
  # colnames(definitions) <- c("Outcome/Enabler", "Domain", "Indicator", "Rationale/Description")
  #  definitions <- definitions[,1:4]
  return(definitions)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce data
read_workforce_data <- function(file = "data/csww_headline_measures_2017_to_2022.csv") {
  workforce_data <- read.csv(file)
  # Select only the columns we want
  workforce_data <- colClean(workforce_data) %>% select(
    "time_period", "geographic_level", "region_name", "la_name", "turnover_rate_fte_perc", "absence_rate_fte_perc",
    "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
    "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte"
  )
  workforce_data <- convert_perc_cols_to_numeric(workforce_data)
  return(workforce_data)
}

read_workforce_data2 <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
  workforce2 <- read.csv(file)
  workforce2 <- colClean(workforce2)%>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",#NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(geographic_level, geo_breakdown,turnover_rate_fte_perc,time_period,"time_period","turnover_rate_fte_perc", "absence_rate_fte_perc",
           "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
           "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte") %>% distinct()
  
  workforce2 <- convert_perc_cols_to_numeric(workforce2)
  return(workforce2)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce characteristics data
read_workforce_char_data <- function(file = "data/csww_workforce_characteristics_2017_to_2022.csv") {
  workforce_char_data <- read.csv(file)
  # Select only the columns we want
  workforce_char_data <- colClean(workforce_char_data) %>% select(
    "time_period", "geographic_level", "region_name", "characteristic", "characteristic_type", "percentage"
  )
  workforce_char_data <- convert_perc_cols_to_numeric(workforce_char_data)
  return(workforce_char_data)
}
