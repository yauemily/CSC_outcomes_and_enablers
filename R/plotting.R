# Template sample data charts
createAvgRevTimeSeries <- function(df, inputArea) {
  ggplot(df, aes(
    x = year,
    y = average_revenue_balance,
    color = area_name
  )) +
    geom_line(size = 1.2) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      legend.position = "top"
    ) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 1, big = ",", prefix = "£")
    ) +
    xlab("Academic year end") +
    ylab("Average revenue balance") +
    scale_color_manual(
      "Area",
      breaks = unique(c("England", inputArea)),
      values = gss_colour_pallette
    )
}

plotAvgRevBenchmark <- function(dfRevenueBalance, inputArea) {
  ggplot(dfRevenueBalance, aes(
    x = area_name,
    y = average_revenue_balance,
    fill = area_name
  )) +
    geom_col() +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      legend.position = "none"
    ) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 1, big = ",", prefix = "£")
    ) +
    xlab("Area") +
    ylab("Average revenue balance") +
    scale_fill_manual(
      "Area",
      breaks = unique(dfRevenueBalance$area_name),
      values = gss_colour_pallette
    )
}

# CSC charts

#This is test code to try and create a function for the plots instead of lots of the same bits of code
#at least a framework for the time series plots ----

plotly_time_series <- function(dataset, level, breakdown, yvalue){
  filtered_data <- dataset %>%
    filter(geographic_level %in% level & geo_breakdown %in% breakdown) %>%
    select(time_period, geo_breakdown, yvalue)
  
  ggplot(filtered_data, aes(`time_period`, yvalue, color = geo_breakdown))+
    geom_line() +
    #ylab("Social worker Turnover rate (FTE) (%)")+
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    labs(color='Breakdown')#+
    #scale_color_manual(
      #"Breakdown",
      #breaks = unique(c("England", inputArea)),
     # values = gss_colour_pallette
    #)
}

plotly_time_series_discrete <- function(dataset, level, breakdown, yvalue){
  filtered_data <- dataset %>%
    filter(geographic_level %in% level & geo_breakdown %in% breakdown) %>%
    select(time_period, geo_breakdown, yvalue)
  
  ggplot(filtered_data, aes(`time_period`, yvalue, color = geo_breakdown))+
    geom_line() +
    #ylab("Social worker Turnover rate (FTE) (%)")+
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 50))+
    labs(color='Breakdown')#+
  #scale_color_manual(
  #"Breakdown",
  #breaks = unique(c("England", inputArea)),
  # values = gss_colour_pallette
  #)
}


# Enabler 1 - Workforce charts ----
# Social Worker Turnover -------
plot_social_worker_turnover <- function(geo_lvl, geo_break){
  social_worker_data <- workforce_data %>%
    filter(geographic_level %in% geo_lvl & geo_breakdown %in% geo_break) %>%
    select(
      time_period, geo_breakdown,
      turnover_rate_fte_perc
    )
  ggplot(social_worker_data, aes(`time_period`, `turnover_rate_fte_perc`, color = geo_breakdown))+
    geom_line() +
    ylab("Social worker Turnover rate (FTE) (%)")+
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    labs(color='Breakdown')+
    scale_color_manual(
      "Breakdown",
      #breaks = unique(c("England", inputArea)),
      values = gss_colour_pallette
    )
}

# Agency Rates ----
plt_agency_rates <- function(geo_lvl, geo_break){
  agency_rates_data <- workforce_data %>%
    filter(geographic_level %in% geo_lvl & geo_breakdown %in% geo_break) %>%
    select(
      time_period, geo_breakdown,
      agency_worker_rate_fte_perc
    )
  ggplot(agency_rates_data, aes(`time_period`, `agency_worker_rate_fte_perc`, color = geo_breakdown))+
    geom_line() +
    ylab("Agency worker rate (FTE) (%)")+
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    labs(color='Breakdown')+
    scale_color_manual(
      "Breakdown",
      #breaks = unique(c("England", inputArea)),
      values = gss_colour_pallette
    )
}


# Vacancy Rate over time ----
plot_vacancy_rate <- function(geo_lvl, geo_break) {
  vacancy_data <- workforce_data %>%
    filter(geographic_level %in% geo_lvl & geo_breakdown %in% geo_break) %>%
    select(
      time_period, geo_breakdown,
      vacancy_rate_fte_perc
    )
  ggplot(vacancy_data, aes(`time_period`, `vacancy_rate_fte_perc`, color = geo_breakdown)) +
    geom_line() +
    ylab("Vacancy rate (FTE) (%)") +
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    labs(color='Breakdown')+
    scale_color_manual(
      "Breakdown",
      #breaks = unique(c("England", inputArea)),
      values = gss_colour_pallette
    )
}

# Worker Caseloads ----
# plotly_caseloads <- function(level, breakdown){
#   filtered_data <- workforce_data %>%
#     filter(geographic_level %in% level & geo_breakdown %in% breakdown) %>%
#     select(time_period, geo_breakdown, `caseload_fte`)
#   
#   ggplot(filtered_data, aes(`time_period`, `caseload_fte`, color = geo_breakdown))+
#     geom_line() +
#     #ylab("Social worker Turnover rate (FTE) (%)")+
#     xlab("Time Period") +
#     theme_classic() +
#     theme(
#       text = element_text(size = 12),
#       axis.title.x = element_text(margin = margin(t = 12)),
#       axis.title.y = element_text(margin = margin(r = 12)),
#       axis.line = element_line(size = 1.0)
#     ) +
#     scale_y_continuous(limits = c(0, 50))+
#     labs(color='Breakdown')#+
#   #scale_color_manual(
#   #"Breakdown",
#   #breaks = unique(c("England", inputArea)),
#   # values = gss_colour_pallette
#   #)
# }

#
#plot_caseloads <- function(){
#   caseload_data <- workforce_data %>%
#     filter(geographic_level == "Regional") %>%
#     select(time_period, geo_breakdown, caseload_fte)
#   
#   ggplot(caseload_data, aes(`time_period`, `caseload_fte`, fill = geo_breakdown)) +
#     geom_col() +
#     ylab("Average Caseload (FTE)") +
#     xlab("Time Period") +
#     theme_classic() +
#     theme(
#       text = element_text(size = 12),
#       axis.text.x = element_text(angle = 300),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(margin = margin(r = 12)),
#       axis.line = element_line(size = 1.0)
#     ) +
#     scale_y_continuous(limits = c(0, 30))+
#     scale_fill_manual(
#       "Breakdown",
#       #breaks = unique(c("England", inputArea)),
#       values = gss_colour_pallette
#     )
# }


plot_caseload_rate <- function(geo_lvl, geo_break) {
  vacancy_data <- workforce_data %>%
    filter(geographic_level %in% geo_lvl & geo_breakdown %in% geo_break) %>%
    select(
      time_period, geo_breakdown,
      caseload_fte
    )
  ggplot(vacancy_data, aes(`time_period`, `caseload_fte`, color = geo_breakdown)) +
    geom_line() +
    ylab("Average Caseload (FTE)") +
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 35))+
    labs(color='Breakdown')+
    scale_color_manual(
      "Breakdown",
      #breaks = unique(c("England", inputArea)),
      values = gss_colour_pallette
    )
}

#bar charts test
plot_caseloads_test1 <- function(){
  caseload_data <- workforce_data %>%
    filter(geographic_level == "Regional") %>%
    select(time_period, geo_breakdown, caseload_fte)
  
  ggplot(caseload_data, aes(`geo_breakdown`, `caseload_fte`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("Caseload (FTE)") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 30))+
    scale_fill_manual(
      "Time Period",
      #breaks = unique(c("England", inputArea)),
      values = gss_colour_pallette
    )
}

plot_ethnicity_rate <- function(geo_breakdown, geographic_level){
  ethnicity_data <- workforce_eth[workforce_eth$geo_breakdown %in% geo_breakdown & workforce_eth$OrgRole == 'All children and family social workers', c("time_period", "geo_breakdown", "white_perc", "mixed_perc", "asian_perc", "black_perc", "other_perc")]
  
  ethnicity_data_long <- reshape(ethnicity_data,
                                 direction = "long",
                                 varying = list(names(ethnicity_data)[3:7]),
                                 v.names = "percentage",
                                 timevar = "ethnicity",
                                 times = c("white_perc", "mixed_perc", "asian_perc", "black_perc", "other_perc"),
                                 new.row.names = 1:1E6)
  
  # Ensure 'percentage' is numeric
  ethnicity_data_long$percentage <- as.numeric(ethnicity_data_long$percentage)
  
  custom_x_order <- c("white_perc", "black_perc", "asian_perc", "mixed_perc", "other_perc")
  
  p <- ggplot(ethnicity_data_long, aes(x = ethnicity, y = percentage, fill = factor(time_period))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylab("Percentage") +
    xlab("Ethnicity") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "Year",  # Change legend title
      values = gss_colour_pallette
    ) +
    scale_x_discrete(
      limits = custom_x_order,
      labels = c("white_perc" = "White", "mixed_perc" = "Mixed", "asian_perc" = "Asian", "black_perc" = "Black", "other_perc" = "Other")
      )
  
  return(p)
}


plot_population_ethnicity_rate <- function(geo_breakdown, geographic_level.x) {
  
  # Filter the data based on 'geo_breakdown', 'geographic_level', and 'OrgRole'
  combined_ethnicity_data <- combined_ethnicity_data[combined_ethnicity_data$geo_breakdown %in% geo_breakdown & 
                                                       combined_ethnicity_data$OrgRole == "All children and family social workers", ]
  
  # Reshape the dataframe to a long format
  combined_ethnicity_data_long <- reshape2::melt(combined_ethnicity_data, 
                                                 id.vars = c("geo_breakdown", "geographic_level.x", "time_period", "region_name", "OrgRole"),
                                                 measure.vars = c("Workforce_WhitePercentage", "Workforce_BlackPercentage", "Workforce_AsianPercentage",
                                                                  "Workforce_MixedPercentage", "Workforce_OtherPercentage",
                                                                  "Population_WhitePercentage", "Population_BlackPercentage", "Population_AsianPercentage", 
                                                                  "Population_MixedPercentage", "Population_OtherPercentage"),
                                                 variable.name = "EthnicGroup",
                                                 value.name = "Percentage")
  
  # Ensure 'percentage' is numeric
  combined_ethnicity_data_long$Percentage <- as.numeric(combined_ethnicity_data_long$Percentage)
  
  combined_ethnicity_data_long$DataSource <- ifelse(grepl("Workforce", combined_ethnicity_data_long$EthnicGroup), sprintf("Workforce (%s)", max(workforce_eth$time_period)), "Population (2021)")
  
  # Create a new column 'Ethnicity' that contains only the ethnic group name
  combined_ethnicity_data_long$Ethnicity <- gsub("Workforce_|Population_", "", combined_ethnicity_data_long$EthnicGroup)
  
  custom_x_order <- c("WhitePercentage", "BlackPercentage", "AsianPercentage", "MixedPercentage", "OtherPercentage")
  
  p <- ggplot(combined_ethnicity_data_long, aes(x = Ethnicity, y = Percentage, fill = factor(DataSource))) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Percentage") +
    xlab("Ethnicity") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "Data",  # Change legend title
      values = c("#3D3D3D", "#F46A25")
    ) +
    scale_x_discrete(
      limits = custom_x_order,
      labels = c("WhitePercentage" = "White", "MixedPercentage" = "Mixed", "AsianPercentage" = "Asian", "BlackPercentage" = "Black", "OtherPercentage" = "Other")
    )
  
  return(p)
}









