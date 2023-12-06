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

# Enabler 1 - Workforce charts
# Social Worker Turnover
plot_social_worker_turnover <- function(){
  social_worker_data <- workforce_data %>%
    filter(geographic_level == "Regional") %>%
    select(
      time_period, region_name,
      turnover_rate_fte_perc
    )
  ggplot(social_worker_data, aes(`time_period`, `turnover_rate_fte_perc`, color = region_name))+
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
    scale_y_continuous(limits = c(0, 100))
}

# Agency Rates
plt_agency_rates <- function(){
  agency_rates_data <- workforce_data %>%
    filter(geographic_level == "Regional") %>%
    select(
      time_period, region_name,
      agency_worker_rate_fte_perc
    )
  ggplot(agency_rates_data, aes(`time_period`, `agency_worker_rate_fte_perc`, color = region_name))+
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
    scale_y_continuous(limits = c(0, 100))
}


# Vacancy Rate over time
plot_vacancy_rate <- function() {
  vacancy_data <- workforce_data %>%
    filter(geographic_level == "Regional") %>%
    select(
      time_period, region_name,
      vacancy_rate_fte_perc
    )
  ggplot(vacancy_data, aes(`time_period`, `vacancy_rate_fte_perc`, color = region_name, line_type = region_name)) +
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
    scale_y_continuous(limits = c(0, 100))
}

plot_caseloads <- function(){
  caseload_data <- workforce_data %>%
    filter(geographic_level == "Regional") %>%
    select(time_period, region_name, caseload_fte)
  
  ggplot(caseload_data, aes(`time_period`, `caseload_fte`, fill = region_name)) +
    geom_col(position = position_dodge()) +
    ylab("Caseload (FTE)") +
    xlab("Time Period") +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0)
    ) +
    scale_y_continuous(limits = c(0, 30))
}

plot_caseloads_test1 <- function(){
  caseload_data <- workforce_data %>%
    filter(geographic_level == "Regional") %>%
    select(time_period, region_name, caseload_fte)
  
  ggplot(caseload_data, aes(`region_name`, `caseload_fte`, fill = factor(time_period))) +
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
    scale_y_continuous(limits = c(0, 30))
}
