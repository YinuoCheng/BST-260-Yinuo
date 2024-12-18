library(dplyr)
library(readr)

# read
covid_data <- read_csv("covid_data_filtered.csv")

# date frame
covid_data$week_ending_date <- as.Date(covid_data$week_ending_date, format = "%Y/%m/%d")

dashed_dates <- as.Date(c("2020-07-01", "2021-02-01", "2021-09-01", "2022-06-01", "2023-03-01", "2024-12-01"))

# average death rate
average_mortality_by_state <- covid_data %>%
  # 
  mutate(period = case_when(
    week_ending_date < dashed_dates[1] ~ "Before 2020-07-01",
    week_ending_date >= dashed_dates[1] & week_ending_date < dashed_dates[2] ~ "2020-07-01 to 2021-02-01",
    week_ending_date >= dashed_dates[2] & week_ending_date < dashed_dates[3] ~ "2021-02-01 to 2021-09-01",
    week_ending_date >= dashed_dates[3] & week_ending_date < dashed_dates[4] ~ "2021-09-01 to 2022-06-01",
    week_ending_date >= dashed_dates[4] & week_ending_date < dashed_dates[5] ~ "2022-06-01 to 2023-03-01",
    week_ending_date >= dashed_dates[5] & week_ending_date < dashed_dates[6] ~ "2023-03-01 to 2024-12-01",
    TRUE ~ "After 2024-12-01"
  )) %>%
  

  group_by(state, period) %>%
  summarise(average_mortality_rate = mean(crude_COVID_rate_weekly, na.rm = TRUE), .groups = 'drop')


print(average_mortality_by_state)


write_csv(average_mortality_by_state, "C:\\Users\\20321\\Downloads\\average_mortality_by_state.csv")