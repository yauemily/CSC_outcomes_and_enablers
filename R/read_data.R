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
# read_workforce_data <- function(file = "data/csww_headline_measures_2017_to_2022.csv") {
#   workforce_data <- read.csv(file)
#   # Select only the columns we want
#   workforce_data <- colClean(workforce_data) %>% select(
#     "time_period", "geographic_level", "region_name", "la_name", "turnover_rate_fte_perc", "absence_rate_fte_perc",
#     "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
#     "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte"
#   )
#   workforce_data <- convert_perc_cols_to_numeric(workforce_data)
#   return(workforce_data)
# }

#For filters to work nicely, we want to have two levels of grouping: geographic level (national, regional, LA) 
#and level breakdown (region names and la names)

read_workforce_data <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
  workforce_data <- read.csv(file)
  workforce_data <- colClean(workforce_data)%>%
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",#NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
    )) %>%
    select(geographic_level, geo_breakdown,turnover_rate_fte_perc,time_period,"time_period","turnover_rate_fte_perc", "absence_rate_fte_perc",
           "agency_worker_rate_fte_perc", "agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
           "turnover_rate_headcount_perc", "agency_worker_rate_headcount_perc", "caseload_fte") %>% distinct()
  
  workforce_data <- convert_perc_cols_to_numeric(workforce_data)
  return(workforce_data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce characteristics data
read_workforce_char_data <- function(file = "data/csww_workforce_characteristics_2017_to_2022.csv") {
  workforce_characteristics <- read.csv(file)
  # Select only the columns we want
  workforce_char_data <- colClean(workforce_characteristics) 
  workforce_char_data <- workforce_char_data %>% filter(characteristic_type != "Total") %>% select(
    "time_period", "geographic_level", "region_name", "characteristic", "characteristic_type", "percentage"
  )
  workforce_char_data <- convert_perc_cols_to_numeric(workforce_char_data)
  return(workforce_char_data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Workforce ethnicity and seniority level data
read_workforce_eth_data <- function(file = "data/csww_workforce_role_by_ethnicity_2019_to_2022.csv") {
  workforce_ethnicity_data <- read.csv(file)
  # Select only columns we want
  #workforce_eth_data <- colCleanPerc(workforce_ethnicity_data)
  workforce_ethnicity_data <- workforce_ethnicity_data %>% 
    mutate(geo_breakdown = case_when(
      geographic_level == "National" ~ "National",#NA_character_,
      geographic_level == "Regional" ~ region_name,
      geographic_level == "Local authority" ~ la_name
  )) %>%
  select(
    geographic_level, geo_breakdown, time_period, "time_period", "geographic_level", "region_name", "OrgRole", "white_perc", "mixed_perc", "asian_perc", 
    "black_perc", "other_perc"
  )
  workforce_ethnicity_data <- convert_perc_cols_to_numeric(workforce_ethnicity_data)
  return(workforce_ethnicity_data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General population ethnicity data


read_ethnic_population_data <- function(file1 = "data/ons-ethnic-population-reg.csv", file2 = "data/ons-ethnic-population-nat.csv", file3 = "data/ons-ethnic-population-la.csv") {
  # Read the csv files
  df_regions <- read.csv(file1, check.names = FALSE)
  df_countries <- read.csv(file2, check.names = FALSE)
  df_authorities <- read.csv(file3, check.names = FALSE)
  
  # Rename the columns to make them consistent across all data frames
  names(df_regions) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_countries) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  names(df_authorities) <- c("Code", "Name", "EthnicGroupCode", "EthnicGroup", "Observation")
  
  # Add 'geographic_level' column to each dataframe
  df_regions$geographic_level <- "Regional"
  df_countries$geographic_level <- "National"
  df_authorities$geographic_level <- "Local authority"
  
  # Combine the data frames
  ethnic_population_data <- rbind(df_regions, df_countries, df_authorities)
  ethnic_population_data <- subset(ethnic_population_data, select = -c(EthnicGroupCode))
  
  # Remove rows where 'EthnicGroup' equals 'Does not apply'
  ethnic_population_data <- ethnic_population_data[ethnic_population_data$EthnicGroup != "Does not apply", ]
  
  # Convert 'EthnicGroup' to character type
  ethnic_population_data$EthnicGroup <- as.character(ethnic_population_data$EthnicGroup)
  
  # Add a new column 'EthnicGroupShort' that has a value of the first 5 characters in the 'EthnicGroup' column
  ethnic_population_data <- ethnic_population_data %>%
    mutate(EthnicGroupShort = substr(EthnicGroup, 1, 5))
  
  # Calculate the total observation for each 'Name'
  total_observation <- ethnic_population_data %>%
    group_by(Name, geographic_level) %>%
    summarise(TotalObservation = sum(Observation), .groups = "drop")
  
  # Join the total observation back to the original dataframe
  ethnic_population_data <- left_join(ethnic_population_data, total_observation, by = c("Name", "geographic_level"))
  
  # Group by 'Name', 'geographic_level' and 'EthnicGroupShort', and calculate the percentage
  ethnic_population_data <- ethnic_population_data %>%
    group_by(Name, geographic_level, EthnicGroupShort) %>%
    summarise(Percentage = sum(Observation) / TotalObservation * 100, .groups = "drop")
  
  # Pivot the dataframe
  ethnic_population_data <- ethnic_population_data %>%
    pivot_wider(names_from = EthnicGroupShort, values_from = Percentage)
  
  # Select the first element of each list
  ethnic_population_data <- ethnic_population_data %>%
    mutate(across(c(Asian, Black, Mixed, Other, White), ~ purrr::map_dbl(., ~ .x[1])))
  
  return(ethnic_population_data)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merge_dataframes <- function() {
  # Read the data
  workforce_eth <- read_workforce_eth_data()
  population_eth <- read_ethnic_population_data()
  
  
  # Filter to include only the latest year of data
  latest_year <- max(workforce_eth$time_period)
  workforce_eth <- subset(workforce_eth, time_period == latest_year)
  
  # Rename the columns to make it clear which dataset they come from
  workforce_eth <- rename(workforce_eth, 
                          Workforce_WhitePercentage = white_perc,
                          Workforce_BlackPercentage = black_perc,
                          Workforce_MixedPercentage = mixed_perc,
                          Workforce_AsianPercentage = asian_perc,
                          Workforce_OtherPercentage = other_perc)
  
  population_eth <- rename(population_eth, 
                           Population_WhitePercentage = White,
                           Population_BlackPercentage = Black,
                           Population_MixedPercentage = Mixed,
                           Population_AsianPercentage = Asian,
                           Population_OtherPercentage = Other)
  
  # Merge the two data frames
  merged_data <- left_join(workforce_eth, population_eth, by = c("geo_breakdown" = "Name"))
  
  return(merged_data)
}

