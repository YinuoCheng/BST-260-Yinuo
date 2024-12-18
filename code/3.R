library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

covid_data <- read_csv("covid_data_filtered.csv")


covid_data$week_ending_date <- as.Date(covid_data$week_ending_date, format = "%Y/%m/%d")


state_list <- unique(average_deaths_by_state$state)
num_states <- length(state_list)


states_per_plot <- 10
num_plots <- ceiling(num_states / states_per_plot)


plots <- list()  
for (i in 1:num_plots) {

  start_index <- (i - 1) * states_per_plot + 1
  end_index <- min(i * states_per_plot, num_states)
  states_to_plot <- state_list[start_index:end_index]
  

  data_to_plot <- average_deaths_by_state %>% filter(state %in% states_to_plot)
  

  p <- ggplot(data_to_plot, aes(x = period, y = crude_COVID_rate_weekly, group = state, color = state)) +
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

