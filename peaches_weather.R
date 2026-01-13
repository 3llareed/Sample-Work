library(dplyr)
library(readr)
library(tidyr)

# Read the data
temp_data <- read_csv("avg_temp.csv")

#This code is used to take monthly averaged weather data, and average the variable, temperature, precipitation, etc., across the three main portion of the peach growing and harvest seasons 

# Extract Year and Month from the 'Date' column
temp_data <- temp_data %>%
  mutate(
    Year = as.integer(substr(Date, 1, 4)),
    Month = as.integer(substr(Date, 5, 6))
  )

# Assign season and adjust year for Winter (Oct–Dec shift to next year)
temp_data <- temp_data %>%
  mutate(
    Season = case_when(
      Month %in% c(10, 11, 12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ NA_character_
    ),
    Season_Year = case_when(
      # Shift Oct–Dec to the following year's Winter
      Month %in% c(10, 11, 12) ~ Year + 1,
      TRUE ~ Year
    )
  )

# Calculate seasonal average temperature
season_avg <- temp_data %>%
  filter(!is.na(Season)) %>%
  group_by(Season_Year, Season) %>%
  summarise(Average_Temp = sum(Value, na.rm = TRUE), .groups = "drop")

# Pivot to wide format
season_summary <- season_avg %>%
  pivot_wider(
    names_from = Season,
    values_from = Average_Temp
  ) %>%
  rename(Year = Season_Year)

# Save to CSV
write_csv(season_summary, "seasonal_avg_prec.csv")

# Output
print(season_summary)
