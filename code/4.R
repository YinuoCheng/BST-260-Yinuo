
library(readr)
library(ggplot2)
library(dplyr)


covid_data <- read_csv("covid_data_filtered.csv")

covid_data$week_ending_date <- as.Date(covid_data$week_ending_date, format = "%Y/%m/%d")

p1 <- ggplot(covid_data, aes(x = week_ending_date, y = COVID_deaths_weekly, group = state)) +
  geom_line(aes(color = state), alpha = 0.5) +
  labs(title = "Weekly COVID-19 Deaths by State",
       x = "Week Ending Date",
       y = "Weekly COVID-19 Deaths") +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  geom_vline(xintercept = as.Date("2020-07-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2021-09-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2023-03-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2024-12-01"), linetype = "dashed", color = "red", alpha = 0.5)


print(p1)

p2 <- covid_data %>%
  group_by(state, week_ending_date) %>%
  summarise(weekly_death_rate = mean(crude_COVID_rate_weekly, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = week_ending_date, y = weekly_death_rate, color = state)) +
  geom_line(alpha = 0.7) +
  labs(title = "Weekly COVID-19 Death Rate by State",
       x = "Week Ending Date",
       y = "Weekly COVID-19 Death Rate (per 100,000)") +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m")


print(p2)


p3 <- covid_data %>%
  group_by(week_ending_date) %>%
  summarise(weekly_death_rate = mean(crude_COVID_rate_weekly, na.rm = TRUE)) %>%
  ggplot(aes(x = week_ending_date, y = weekly_death_rate)) +
  geom_line(color = "blue", alpha = 0.8) +
  labs(title = "Weekly COVID-19 Death Rate",
       x = "Week Ending Date",
       y = "Weekly COVID-19 Death Rate (per 100,000)") +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  geom_vline(xintercept = as.Date("2020-07-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2021-09-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2023-03-01"), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2024-12-01"), linetype = "dashed", color = "red", alpha = 0.5)


print(p3)