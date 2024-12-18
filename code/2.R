library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# average_mortality_by_state <- covid_data %>%
#   mutate(period = case_when(
#     week_ending_date < dashed_dates[1] ~ "Before 2020-07-01",
#     week_ending_date >= dashed_dates[1] & week_ending_date < dashed_dates[2] ~ "2020-07-01 to 2021-02-01",
#     week_ending_date >= dashed_dates[2] & week_ending_date < dashed_dates[3] ~ "2021-02-01 to 2021-09-01",
#     week_ending_date >= dashed_dates[3] & week_ending_date < dashed_dates[4] ~ "2021-09-01 to 2022-06-01",
#     week_ending_date >= dashed_dates[4] & week_ending_date < dashed_dates[5] ~ "2022-06-01 to 2023-03-01",
#     week_ending_date >= dashed_dates[5] & week_ending_date < dashed_dates[6] ~ "2023-03-01 to 2024-12-01",
#     TRUE ~ "After 2024-12-01"
#   )) %>%
#   group_by(state, period) %>%
#   summarise(average_mortality_rate = mean(crude_COVID_rate_weekly, na.rm = TRUE), .groups = 'drop')


state_list <- unique(average_mortality_by_state$state)
num_states <- length(state_list)


states_per_plot <- 10
num_plots <- ceiling(num_states / states_per_plot)


plots <- list()  
for (i in 1:num_plots) {

  start_index <- (i - 1) * states_per_plot + 1
  end_index <- min(i * states_per_plot, num_states)
  states_to_plot <- state_list[start_index:end_index]
  

  data_to_plot <- average_mortality_by_state %>% filter(state %in% states_to_plot)
  

  p <- ggplot(data_to_plot, aes(x = period, y = average_mortality_rate, group = state, color = state)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Average COVID-19 Mortality Rate by State and Period", sep = ""),
         x = "Period",
         y = "Average Mortality Rate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") + 
    scale_color_brewer(palette = "Paired")
  
  plots[[i]] <- p  
}

combined_plot <- wrap_plots(plots, ncol = 3) +  
  plot_layout(nrow = ceiling(num_plots / 3), ncol = 3) +  
  plot_spacer()  


print(combined_plot)