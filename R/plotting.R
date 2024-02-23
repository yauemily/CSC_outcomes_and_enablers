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


#testing_plot_function <- function(dataset, level, breakdown, yvalue, yaxis_title){
#  data <- dataset %>%
#    select(time_period, geo_breakdown, `yvalue`)
#  
#  ggplot(data, aes(x = `time_period`, y=!!sym(yvalue), color = geo_breakdown))+
#    geom_line() +
#    ylab(yaxis_title)+
#    xlab("Time Period") +
#    theme_classic() +
#    theme(
#      text = element_text(size = 12),
#      axis.title.x = element_text(margin = margin(t = 12)),
#      axis.title.y = element_text(margin = margin(r = 12)),
#      axis.line = element_line(size = 1.0)
#    ) +
#    scale_y_continuous(limits = c(0, 100))+
#    labs(color='Breakdown')+
#    scale_color_manual(
#      "Breakdown",
#      #breaks = unique(c("England", inputArea)),
#      values = gss_colour_pallette
#    )
#}



#This is test code to try and create a function for the plots instead of lots of the same bits of code
#at least a framework for the time series plots ----

plotly_time_series <- function(dataset, level, breakdown, yvalue, yaxis_title){
  filtered_data <- dataset %>%
    #filter(geographic_level %in% level & geo_breakdown %in% breakdown) %>%
    select(time_period, geo_breakdown, `yvalue`) %>%
    mutate(`Time period` = as.character(`time_period`)) %>%
   rename(`Breakdown` = `geo_breakdown`)
  
    ggplot(filtered_data, aes(x = `Time period`, y=!!sym(yvalue), color = `Breakdown`))+
    geom_path(group = 1) +
       ylab(yaxis_title)+
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

# function for time series with a custom y-axis scale
plotly_time_series_custom_scale <- function(dataset, level, breakdown, yvalue, yaxis_title, ylim_upper){
  filtered_data <- dataset %>%
    select(time_period, geo_breakdown, `yvalue`) %>%
    mutate(`Time period` = as.character(`time_period`)) %>%
    rename(`Breakdown` = `geo_breakdown`)
  
  ggplot(filtered_data, aes(x = `Time period`, y=!!sym(yvalue), color = `Breakdown`))+
    geom_path(group = 1) +
    ylab(yaxis_title)+
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, ylim_upper))+
    labs(color='Breakdown')+
    scale_color_manual(
      "Breakdown",
      values = gss_colour_pallette
    )
}

# function for time series but with a national comparison
t_series_nat_comp_plot <- function(dataset, level, breakdown, yvalue, yaxis_title){
  filtered_data <- dataset %>%
    filter((geographic_level %in% level & geo_breakdown %in% breakdown)|geographic_level == 'National') %>%
    select(time_period, geo_breakdown, `yvalue`)
  
  ggplot(filtered_data, aes(x = `time_period`, y=!!sym(yvalue), color = geo_breakdown))+
    geom_line() +
    ylab(yaxis_title)+
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
  #  "Breakdown",
  #breaks = unique(c("England", inputArea)),
  #  values = gss_colour_pallette
  #)
}

#function just showing la and region
t_series_region_la_plot <- function(dataset, level, breakdown, yvalue, yaxis_title){
  #Need to find the correct Region for this breakdown
  location <- location_data %>%
    filter(la_name %in% breakdown)
  
  filtered_data <- dataset %>%
    filter((geo_breakdown %in% c(breakdown, location[[1]]))) %>%
    select(time_period, geo_breakdown, `yvalue`)
  
  ggplot(filtered_data, aes(x = `time_period`, y=!!sym(yvalue), color = geo_breakdown))+
    geom_line() +
    ylab(yaxis_title)+
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

#function to have both comparisons (regional and national)
t_series_dual_comp_plot <- function(dataset, level, breakdown, yvalue, yaxis_title){
  #Need to find the correct Region for this breakdown
  location <- location_data %>%
    filter(la_name %in% breakdown)
  
  filtered_data <- dataset %>%
    filter((geo_breakdown %in% c(breakdown, location[[1]])|geographic_level == 'National')) %>%
    select(time_period, geo_breakdown, `yvalue`)
  
  ggplot(filtered_data, aes(x = `time_period`, y=!!sym(yvalue), color = geo_breakdown))+
    geom_line() +
    ylab(yaxis_title)+
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



# plotly_time_series_discrete <- function(dataset, level, breakdown, yvalue){
#   filtered_data <- dataset %>%
#     filter(geographic_level %in% level & geo_breakdown %in% breakdown) %>%
#     select(time_period, geo_breakdown, yvalue)
#   
#   ggplot(filtered_data, aes(`time_period`, yvalue, color = geo_breakdown))+
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

#bar chart by region
plot_turnover_reg <- function(){
  turnover_reg_data <- workforce_data %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, turnover_rate_fte_perc) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -turnover_rate_fte_perc)) # Order by turnover rate
  
  ggplot( turnover_reg_data , aes(`geo_breakdown`, `turnover_rate_fte_perc`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("Turnover Rate (FTE) %") +
    xlab("Region") +
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
      "Time Period",
      #breaks = unique(c("England", inputArea)),
      values = '#12436D'#gss_colour_pallette
    )
}

plot_turnover_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    turnover_reg_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, turnover_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -turnover_rate_fte_perc),
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    turnover_reg_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, turnover_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -turnover_rate_fte_perc),
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    turnover_reg_data <- workforce_data %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, turnover_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -turnover_rate_fte_perc),
             is_selected = "Selected")
  }
  
  
  p <- ggplot(turnover_reg_data, aes(`geo_breakdown`, `turnover_rate_fte_perc`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("Turnover Rate (FTE) %") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
}


plot_agency_rate_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    agency_rates_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc), # Order by caseload_fte
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    agency_rates_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc), # Order by caseload_fte
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    agency_rates_data <- workforce_data %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc), # Order by caseload_fte
             is_selected = "Selected")
  }
  
  
  p <- ggplot(agency_rates_data, aes(`geo_breakdown`, `agency_worker_rate_fte_perc`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("Agency Rate %") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
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

plot_vacancy_rate_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    vacancy_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc), # Order by caseload_fte
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    vacancy_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc), # Order by caseload_fte
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    vacancy_data <- workforce_data %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc), # Order by caseload_fte
             is_selected = "Selected")
  }
  
  
  p <- ggplot(vacancy_data, aes(`geo_breakdown`, `vacancy_rate_fte_perc`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("Vacancy Rate (FTE) %") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
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

#bar chart by region
plot_agency_reg <- function(){
 agency_reg_data <- workforce_data %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc)) # Order by turnover rate
  
  ggplot( agency_reg_data , aes(`geo_breakdown`, `agency_worker_rate_fte_perc`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("Agency Worker Rate (FTE) %") +
    xlab("Region") +
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
          "Time Period",
          #breaks = unique(c("England", inputArea)),
          values = '#12436D'#gss_colour_pallette
        )
      }
      
      
plot_agency_rate_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    agency_rates_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc), # Order by agency rate
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    agency_rates_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc), # Order by agency rate
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    agency_rates_data <- workforce_data %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, agency_worker_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -agency_worker_rate_fte_perc), # Order by agency rate
             is_selected = "Selected")
  }
  
  
  p <- ggplot(agency_rates_data, aes(`geo_breakdown`, `agency_worker_rate_fte_perc`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("Agency Worker Rate (FTE) %") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
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


#bar chart by region
plot_vacancy_reg <- function(){
  vacancy_reg_data <- workforce_data %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc)) # Order by turnover rate
  
  ggplot( vacancy_reg_data  , aes(`geo_breakdown`, `vacancy_rate_fte_perc`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("Vacancy Rate (FTE) %") +
    xlab("Region") +
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
      "Time Period",
      #breaks = unique(c("England", inputArea)),
      values = '#12436D'#gss_colour_pallette
    )
}


plot_vacancy_rate_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    vacancy_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc), # Order by vacancy rate
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    vacancy_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc), # Order by vacancy rate
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    vacancy_data <- workforce_data %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, vacancy_rate_fte_perc) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -vacancy_rate_fte_perc), # Order by vacancy rate
             is_selected = "Selected")
  }
  
  
  p <- ggplot(vacancy_data, aes(`geo_breakdown`, `vacancy_rate_fte_perc`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("Vacancy Rate (FTE) %") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 100))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
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
plot_caseloads_reg <- function(){
  caseload_data <- workforce_data %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, caseload_fte) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -caseload_fte)) # Order by caseload_fte
  
  ggplot(caseload_data, aes(`geo_breakdown`, `caseload_fte`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("Average Caseload (FTE)") +
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
      values = '#12436D'#gss_colour_pallette
    )
}

plot_caseload_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    caseload_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, caseload_fte) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -caseload_fte), # Order by caseload_fte
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    caseload_data <- workforce_data %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, caseload_fte) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -caseload_fte), # Order by caseload_fte
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    caseload_data <- workforce_data %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, caseload_fte) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -caseload_fte), # Order by caseload_fte
             is_selected = "Selected")
  }
  
  
  p <- ggplot(caseload_data, aes(`geo_breakdown`, `caseload_fte`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("Average Caseload (FTE)") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 30))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
}



plot_ethnicity_rate <- function(geo_breakdown, geographic_level){
  ethnicity_data <- workforce_eth[workforce_eth$geo_breakdown %in% geo_breakdown & workforce_eth$OrgRole == 'All children and family social workers', 
                                  c("time_period", "geo_breakdown", "white_perc", "mixed_perc", "asian_perc", "black_perc", "other_perc")
                                  ]
  
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

plot_seniority_eth <- function(geo_breakdown, geographic_level){
  ethnicity_data_sen <- workforce_eth_seniority[workforce_eth_seniority$geo_breakdown %in% geo_breakdown & workforce_eth_seniority$seniority != 'All children and family social workers', 
                                  c("time_period", "geo_breakdown", "white_perc", "mixed_perc", "asian_perc", "black_perc", "other_perc", "known_headcount", "seniority")]
  
  # Reshape data using pivot_longer()
  ethnicity_data_long <- ethnicity_data_sen %>%
    pivot_longer(
      cols = c("white_perc", "mixed_perc", "asian_perc", "black_perc", "other_perc"),
      names_to = "ethnicity",
      values_to = "percentage"
    )

  
  # Ensure 'percentage' is numeric
  ethnicity_data_long$percentage <- as.numeric(ethnicity_data_long$percentage)
  
  custom_x_order <- c("white_perc", "black_perc", "asian_perc", "mixed_perc", "other_perc")
  custom_fill_order <- c("Manager",  "Senior practitioner", "Case holder","Qualified without cases")
  
  
  p <- ggplot(ethnicity_data_long, aes(x = ethnicity, y = percentage, fill = factor(seniority,levels = custom_fill_order))) +
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
      "Seniority Level",  # Change legend title
      values = gss_colour_pallette
    ) +
    scale_x_discrete(
      limits = custom_x_order,
      labels = c("white_perc" = "White", "mixed_perc" = "Mixed", "asian_perc" = "Asian", "black_perc" = "Black", "other_perc" = "Other")
    )
  
  return(p)
}

#bar chart by region
plot_cla_rate_reg <- function(){
  cla_reg_data <- cla_rates %>%
    filter(geographic_level == "Regional", time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
    select(time_period, geo_breakdown, rate_per_10000) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -rate_per_10000)) # Order by cla rate
  
  # Set the max y-axis scale
  max_rate <- max(cla_rates$rate_per_10000[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)
  
  # Round the max_rate to the nearest 50
  max_rate <- ceiling(max_rate / 50) * 50
  
  ggplot( cla_reg_data , aes(`geo_breakdown`, `rate_per_10000`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("CLA Rate Per 10,000 Children") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate))+
    scale_fill_manual(
      "Time Period",
      #breaks = unique(c("England", inputArea)),
      values = '#12436D'#gss_colour_pallette
    )
}

plot_cla_rate_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/csww_headline_measures_2017_to_2022.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/csww_headline_measures_2017_to_2022.csv")
  
  if (selected_geo_lvl == "Local authority") {
    cla_data <- cla_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, rate_per_10000) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -rate_per_10000), # Order by rate_per_10000
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    cla_data <- cla_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period), population_count == "Children starting to be looked after each year") %>%
      select(time_period, geo_breakdown, rate_per_10000) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -rate_per_10000), # Order by rate_per_10000
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    cla_data <- cla_rates %>%
      filter(geo_breakdown %in% location, time_period == max(time_period), population_count == "Children starting to be looked after each year", rate_per_10000 != "NA") %>%
      select(time_period, geo_breakdown, rate_per_10000) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -rate_per_10000), # Order by rate_per_10000
             is_selected = "Selected")
  }
  
  # Set the max y-axis scale
  max_rate <- max(cla_rates$rate_per_10000[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)
  
  # Round the max_rate to the nearest 50
  max_rate <- ceiling(max_rate / 50) * 50
  
  p <- ggplot(cla_data, aes(`geo_breakdown`, `rate_per_10000`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("CLA Rate Per 10,000 Children") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
}



# Outcome 1 - Access to support getting help charts ----
# CIN rates -------

#cin rate chart by region
plot_cin_rate_reg <- function(){
  cin_reg_data <- cin_rates %>%
    filter(geographic_level == "Regional", time_period == max(time_period)) %>%
    select(time_period, geo_breakdown, CIN_rate) %>%
    mutate(geo_breakdown = reorder(geo_breakdown, -CIN_rate)) # Order by turnover rate
  
  # Set the max y-axis scale
  max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)
  
  # Round the max_rate to the nearest 50
  max_rate <- ceiling(max_rate / 50) * 50
  
  ggplot( cin_reg_data , aes(`geo_breakdown`, `CIN_rate`, fill = factor(time_period))) +
    geom_col(position = position_dodge()) +
    ylab("CIN rates per 10,000") +
    xlab("Region") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate))+
    scale_fill_manual(
      "Time Period",
      #breaks = unique(c("England", inputArea)),
      values = '#12436D'#gss_colour_pallette
    )
}




#cin rate chart by la
plot_cin_rates_la <- function(selected_geo_breakdown = NULL, selected_geo_lvl = NULL){
  
  GET_location <- function(file = "data/b1_children_in_need_2013_to_2023.csv"){
    FACT_location <- read.csv(file)
    FACT_location <- FACT_location%>%
      select(region_name, la_name) %>%
      filter((la_name != '')) %>%
      unique()
  }
  
  location_data <- GET_location("data/b1_children_in_need_2013_to_2023.csv")
  
  if (selected_geo_lvl == "Local authority") {
    cin_data <- cin_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, CIN_rate) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -CIN_rate), # Order by cin rate
             is_selected = ifelse(geo_breakdown == selected_geo_breakdown, "Selected", "Not Selected"))
  } else if (selected_geo_lvl == "National") {
    cin_data <- cin_rates %>%
      filter(geographic_level == "Local authority", time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, CIN_rate) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -CIN_rate), # Order by cin rate
             is_selected = "Not Selected")
  } else if (selected_geo_lvl == "Regional") {
    
    # Check if the selected region is London
    if (selected_geo_breakdown == "London") {
      # Include both Inner London and Outer London
      location <- location_data %>%
        filter(region_name %in% c("Inner London", "Outer London")) %>%
        pull(la_name)
    } else {
      # Get the la_name values within the selected region_name
      location <- location_data %>%
        filter(region_name == selected_geo_breakdown) %>%
        pull(la_name)
    }
    
    cin_data <- cin_rates %>%
      filter(geo_breakdown %in% location, time_period == max(time_period)) %>%
      select(time_period, geo_breakdown, CIN_rate) %>%
      mutate(geo_breakdown = reorder(geo_breakdown, -CIN_rate), # Order by cin rate
             is_selected = "Selected")
  }
  
  # Set the max y-axis scale
  max_rate <- max(cin_rates$CIN_rate, na.rm = TRUE)
  
  # Round the max_rate to the nearest 50
  max_rate <- ceiling(max_rate / 50) * 50
  
  p <- ggplot(cin_data, aes(`geo_breakdown`, `CIN_rate`, fill = `is_selected`)) +
    geom_col(position = position_dodge()) +
    ylab("CIN rates per 10,000") +
    xlab("") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, max_rate))+
    scale_fill_manual(
      "LA Selection",
      values = c("Selected" = '#12436D', "Not Selected" = '#88A1B5')
    )
  
  # Conditionally set the x-axis labels and ticks
  if (selected_geo_lvl == "Regional") {
    p <- p + theme(axis.text.x = element_text(angle = 300, hjust = 1))
  } else {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
}


# Outcome 1 - Access to support getting help charts ----


