library(dplyr)
data <- read.csv("Trips_by_Distance_20231030.csv")
summary(data)
str(data)
df <- as.data.frame(data)
# Calculate average number of trips for a specific date
christmas_date <- "2022/12/25"
# Filter the data for Philadelphia
pa_data <- df %>%
  filter(State.Postal.Code == "PA")
year_2022_data <- pa_data %>%
  filter(substr(Date, 1, 4) == "2022")

selected_counties <- c("Philadelphia County", "Montgomery County", "Chester County", "Bucks County", "Delaware County")

filtered_data <- pa_data %>%
  filter(substr(Date, 1, 4) == "2022", County.Name %in% selected_counties)

# Group and summarize data by County, Month, and Year, and order by total trips
county_monthly_summary <- filtered_data %>%
  group_by(County.Name, Month) %>%
  summarise(total_trips = sum(Number.of.Trips, na.rm = TRUE)) %>%
  arrange(Month, desc(total_trips))

# Print individual tables for each month
unique_months <- unique(county_monthly_summary$Month)

for (month in unique_months) {
  cat("Month:", month, "\n")
  month_data <- county_monthly_summary %>%
    filter(Month == month)
  
  print(month_data)
  cat("Total for Month:", sum(month_data$total_trips), "\n")
  cat("\n")
}



philadelphia_county_data <- year_2022_data %>%
  filter(County.Name == "Philadelphia County")
montegomery_county_data <- year_2022_data %>%
  filter(County.Name == "Montegomery County")
chester_county_data <- year_2022_data %>%
  filter(County.Name == "Chester County")
bucks_county_data <- year_2022_data %>%
  filter(County.Name == "Bucks County")
delaware_county_data <- year_2022_data %>%
  filter(County.Name == "Delaware County")


average_trips_home_christmas <- df %>%
  filter(County.Name == county, Date == christmas_date) %>%
  summarise(average_trips = mean(Number.of.Trips, na.rm = TRUE))

average_population_home_christmas <- df %>%
  filter(County.Name == county, Date == christmas_date) %>%
  summarise(average_population = mean(Population.Staying.at.Home, na.rm = TRUE))

print(average_trips_home_christmas)
print(average_population_home_christmas)

