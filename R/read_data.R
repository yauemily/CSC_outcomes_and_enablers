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

#Function to clean column names
colClean <- function(x){ colnames(x) <- gsub("\\.", "perc", colnames(x)); x}  

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

read_definitions <- function(file = "data/definitions.csv"){
  definitions <- read.csv(file)
 # colnames(definitions) <- c("Outcome/Enabler", "Domain", "Indicator", "Rationale/Description")
#  definitions <- definitions[,1:4]
  return(definitions)
}

read_workforce_data <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
  workforce_data <- read.csv(file)
  # Select only the columns we want
  workforce_data <- colClean(workforce_data) %>% select("time_period", "geographic_level", "region_name", "la_name", "turnover_rate_fte_perc", "absence_rate_fte_perc",
                                               "agency_worker_rate_fte_perc","agency_cover_rate_fte_perc", "vacancy_rate_fte_perc", "vacancy_agency_cover_rate_fte_perc",
                                               "turnover_rate_headcount_perc","agency_worker_rate_headcount_perc")
  return(workforce_data)
}

