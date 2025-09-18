# -----------PHASE ONE-----------
#install required packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("corrplot")
install.packages("forecast")
install.packages('tidyr')
# load library()
library("readr")
library("dplyr")
library("ggplot2")
library("dplyr")
library(tidyr)
library(ggplot2)
library(forecast)
#setting working directory
setwd("/home/setup/Desktop/Airquality_Data_Analysis_with_R")
# load the air quality data set
df_aqi <- read_csv("Air_Quality.csv")

# Display the first few rows of the data frame
head(df_aqi)
#getting info about the data
glimpse(df_aqi)

# Statistical summary for numeric columns
summary(df_aqi)
#---------PHASE TWO --------------------
# ---    A -cleaning the Datas
#    ---- 80% of Co2 data is missing we can exclude it from our main correlation analysis
#---- The Date column is currently a character string.
#For any time-series analysis, R needs to recognize it as a date object.
#We can use the ymd_hms() function from the lubridate
#load the lubridate library
library("lubridate")
# Converting the Date column to a proper date-time object
df_aqi <- df_aqi %>%
  mutate(Date = ymd_hms(Date))

#as we got errors in converting some dates to proper date object
#we cleaned the rows rows as part of data cleaning by filtering by if is not [NA]
df_aqi_clean <- df_aqi %>%
  filter(!is.na(Date))

# Check the number of rows to confirm the cleaning worked
print("The number of rows after cleaning:")
print(nrow(df_aqi_clean))

#also lets remove c02 as part of our data cleaning as it has almost 45,000 NA values
#i.e 80% of the values are missing by excluding with - sign
df_final <- df_aqi_clean %>%
  select(-CO2)
# Checking the column names to confirm it was removed
print("The columns in the final data frame are:")
print(names(df_final))
print(nrow(df_final))

# --------------  PHASE3 (DATA ANALYSIS) -------------
# =========   (ONE) Pollutant Correlation Analysis ===========
# Our goal here is to identify which pollutants have the strongest relationship with the Air Quality Index (AQI).
# We'll do this by creating a Correlation matrix and then visualizing it with a correlation heatmap
# corrplot package is required and loaded
library("corrplot")
# (A): we  Select the numeric columns for the analysis
# by  selecting  all columns except 'Date' and 'City'
df_numeric <- df_final %>%
  select(-Date, -City)

# (B):Compute the correlation matrix for the prepared data
correlation_matrix <- cor(df_numeric)
# (C): # Plotting the correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper",
         addCoef.col = "black", # Add correlation coefficients
         tl.cex = 0.7, # Font size for labels
         tl.col = "black")

#============ (TWO) Health Impact Analysis ========
#Our task is to find out how many days each city exceeded these healthy thresholds during 2024.
#We will then visualize this to make a  statement in presentation.
# (A): we calculate the Days Exceeding WHO Guidelines per City
# with dplyr we can group the data by City and count the number of daily measurements
#that are above the WHO guidelines for both PM2.5 and PM10

# Calculate the number of days each city exceeded the WHO PM2.5 guideline
# ------------------ According to WHO
# PM2.5(fine dust ,most dangerous) : harmful when concentration is greater than [15μg/m³],
# PM10(Coarse Dust): Harmful when concentration is greater than [45μg/m³]
#----------------------
pm25_exceedance <- df_final %>%
  group_by(City) %>%
  summarise(
    Days_Above_PM25_Guideline = sum(PM2.5 > 15, na.rm = TRUE)
  )

# Calculate the number of days each city exceeded the WHO PM10 guideline
pm10_exceedance <- df_final %>%
  group_by(City) %>%
  summarise(
    Days_Above_PM10_Guideline = sum(PM10 > 45, na.rm = TRUE)
  )

# Print the results
print("Days each city exceeded the WHO PM2.5 guideline:")
print(pm25_exceedance)

print("Days each city exceeded the WHO PM10 guideline:")
print(pm10_exceedance)


# Plotting the values in a a Bar chart for easier visualization & disparity between the cities
# Combine the two data frames for easier plotting
combined_exceedance <- left_join(pm25_exceedance, pm10_exceedance, by = "City")

# Reshaping  the data for a better bar chart layout
plot_data <- combined_exceedance %>%
  pivot_longer(
    cols = c("Days_Above_PM25_Guideline", "Days_Above_PM10_Guideline"),
    names_to = "Pollutant",
    values_to = "Days_Exceeded"
  )

# Create the bar chart with reordered cities
ggplot(plot_data, aes(x = reorder(City, Days_Exceeded), y = Days_Exceeded, fill = Pollutant)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Days_Exceeded), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Number of Days Exceeding WHO Air Quality Guidelines (2024)",
    subtitle = "Cities Ranked by Total Exceedance Days (PM2.5 >15 µg/m³ & PM10 >45 µg/m³)",
    x = "City",
    y = "Number of Days Exceeded",
    fill = "Pollutant Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------(THREE) Time-Series Forecasting for Cairo -----------
# Let's use Cairo, as its high number of exceedance days will
# likely show some interesting trends.
#  we need to filter our data for Cairo and then combine  it to a weekly level.
# Making the time series easier to model, forecast library will be used

# Filtering the data to get only Cairo's records
cairo_data <- df_final %>%
  filter(City == "Cairo")

# combine the data to a weekly average for a cleaner time series
#i.e hours from jan [1-7] becomes week
cairo_ts_data <- cairo_data %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(
    AQI_Weekly = mean(AQI, na.rm = TRUE)
  )

# converting dataframe into a time series object using ts(startdate, frequency)
cairo_ts <- ts(cairo_ts_data$AQI_Weekly, start = c(2024, 1), frequency = 52)

# Plot the time series to visualize the trend
plot(cairo_ts, main = "Cairo Weekly AQI (2024)", xlab = "Time", ylab = "AQI",col = "dodgerblue" ,lwd=2)

# using auto.arima() function to select the best arima model for the forecast
# Fitting an ARIMA model to the time series data
fit <- auto.arima(cairo_ts)

# Print the model summary to see the chosen parameters
print(summary(fit))
# We can Forecast the next 4 weeks of AQI(Air quality Index for Cairo City)
forecast_results <- forecast(fit, h = 5)


# Plot the forecast using autoplot, which uses ggplot2
# We will use this to set up the plot
autoplot(forecast_results, main = "Cairo AQI Forecast (Next 4 Weeks)",
         xlab = "Time", ylab = "Weekly AQI") +
  # Using autolayer to add the historical data
  autolayer(cairo_ts, series = "Historical Data") +
  # Using autolayer to add the forecast with its confidence intervals
  autolayer(forecast_results, series = "Forecasted Data", PI = TRUE) +
  scale_color_manual(values = c("Historical Data" = "dodgerblue", "Forecasted Data" = "firebrick")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())



# ------------(Four) Time-Series Forecasting for Newyork -----------
#it has high risk:
# Filter the data to get only New York's records
ny_data <- df_final %>%
  filter(City == "New York")

# Aggregate the data to a weekly average
ny_ts_data <- ny_data %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(
    AQI_Weekly = mean(AQI, na.rm = TRUE)
  )

# 2. Create a Time-Series Object for New York
ny_ts <- ts(ny_ts_data$AQI_Weekly, start = c(2024, 1), frequency = 52)
plot(ny_ts, main = "New York Weekly AQI (2024)", xlab = "Time", ylab = "AQI", col = "brown" ,lwd=2)

# 3. Build and Forecast the Model for New York
fit_ny <- auto.arima(ny_ts)
print(summary(fit_ny))
forecast_results_ny <- forecast(fit_ny, h = 4)
# Plot the New York forecast using autoplot
autoplot(forecast_results_ny, main = "New York AQI Forecast (Next 4 Weeks)",
         xlab = "Time", ylab = "Weekly AQI") +
  # Adding the historical data
  autolayer(ny_ts, series = "Historical Data") +
  # Add the forecast with confidence intervals
  autolayer(forecast_results_ny, series = "Forecasted Data", PI = TRUE) +
  # Customize the color palette and labels
  scale_color_manual(values = c("Historical Data" = "purple", "Forecasted Data" = "orange")) +
  # Add a clean theme for a professional look
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())



# ------------(Five) Time-Series Forecasting for London -----------
#low risk city
# 1. Prepare the Data for London
# Filter the data to get only London's records
london_data <- df_final %>%
  filter(City == "London")

# Aggregate the data to a weekly average
london_ts_data <- london_data %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(
    AQI_Weekly = mean(AQI, na.rm = TRUE)
  )

# 2. Create a Time-Series Object for London
london_ts <- ts(london_ts_data$AQI_Weekly, start = c(2024, 1), frequency = 52)
plot(london_ts, main = "London Weekly AQI (2024)", xlab = "Time", ylab = "AQI", col="magenta", lwd=2)

# 3. Build and Forecast the Model for London
fit_london <- auto.arima(london_ts)
print(summary(fit_london))
forecast_results_london <- forecast(fit_london, h = 4)
# Plot the London forecast using autoplot
autoplot(forecast_results_london, main = "London AQI Forecast (Next 4 Weeks)",
         xlab = "Time", ylab = "Weekly AQI") +
  # Add the historical data
  autolayer(london_ts, series = "Historical Data") +
  # Add the forecast with confidence intervals
  autolayer(forecast_results_london, series = "Forecasted Data", PI = TRUE) +
  # Customize the color palette and labels
  scale_color_manual(values = c("Historical Data" = "green", "Forecasted Data" = "blue")) +
  # Add a clean theme for a professional look
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())


# 1. Prepare the Data for Dubai
dubai_data <- df_aqi_clean %>%
  filter(City == "Dubai") %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(AQI_Weekly = mean(AQI, na.rm = TRUE))

# 2. Create a Time-Series Object for Dubai
dubai_ts <- ts(dubai_data$AQI_Weekly, start = c(2024, 1), frequency = 52)
plot(dubai_ts, main = "Dubai Weekly AQI (2024)", xlab = "Time", ylab = "AQI", col="magenta", lwd=2)

# 3. Build and Forecast the Model for Dubai
fit_dubai <- auto.arima(dubai_ts)
print(summary(fit_dubai))
forecast_results_dubai <- forecast(fit_dubai, h = 4)

# Plot the Dubai forecast using autoplot
autoplot(forecast_results_dubai, main = "Dubai AQI Forecast (Next 4 Weeks)",
         xlab = "Time", ylab = "Weekly AQI") +
  autolayer(dubai_ts, series = "Historical Data") +
  autolayer(forecast_results_dubai, series = "Forecasted Data", PI = TRUE) +
  scale_color_manual(values = c("Historical Data" = "orange", "Forecasted Data" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())



# --brasilia

# 1. Prepare the Data for Brasilia
brasilia_data <- df_aqi_clean %>%
  filter(City == "Brasilia") %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(AQI_Weekly = mean(AQI, na.rm = TRUE))

# 2. Create a Time-Series Object for Brasilia
brasilia_ts <- ts(brasilia_data$AQI_Weekly, start = c(2024, 1), frequency = 52)
plot(brasilia_ts, main = "Brasilia Weekly AQI (2024)", xlab = "Time", ylab = "AQI", col="magenta", lwd=2)

# 3. Build and Forecast the Model for Brasilia
fit_brasilia <- auto.arima(brasilia_ts)
print(summary(fit_brasilia))
forecast_results_brasilia <- forecast(fit_brasilia, h = 4)

# Plot the Brasilia forecast using autoplot
autoplot(forecast_results_brasilia, main = "Brasilia AQI Forecast (Next 4 Weeks)",
         xlab = "Time", ylab = "Weekly AQI") +
  autolayer(brasilia_ts, series = "Historical Data") +
  autolayer(forecast_results_brasilia, series = "Forecasted Data", PI = TRUE) +
  scale_color_manual(values = c("Historical Data" = "dodgerblue", "Forecasted Data" = "skyblue")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())



# sydney
# 1. Prepare the Data for Sydney
sydney_data <- df_aqi_clean %>%
  filter(City == "Sydney") %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(AQI_Weekly = mean(AQI, na.rm = TRUE))

# 2. Create a Time-Series Object for Sydney
sydney_ts <- ts(sydney_data$AQI_Weekly, start = c(2024, 1), frequency = 52)
plot(sydney_ts, main = "Sydney Weekly AQI (2024)", xlab = "Time", ylab = "AQI", col="magenta", lwd=2)

# 3. Build and Forecast the Model for Sydney
fit_sydney <- auto.arima(sydney_ts)
print(summary(fit_sydney))
forecast_results_sydney <- forecast(fit_sydney, h = 4)

# Plot the Sydney forecast using autoplot
autoplot(forecast_results_sydney, main = "Sydney AQI Forecast (Next 4 Weeks)",
         xlab = "Time", ylab = "Weekly AQI") +
  autolayer(sydney_ts, series = "Historical Data") +
  autolayer(forecast_results_sydney, series = "Forecasted Data", PI = TRUE) +
  scale_color_manual(values = c("Historical Data" = "forestgreen", "Forecasted Data" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())





# scatter plot showing exceedence
# Make sure you have the tidyverse library loaded
library(tidyverse)

# Filter the data for Cairo and add a column to flag exceedances
cairo_exceedance_plot_data <- df_final %>%
  filter(City == "Cairo") %>%
  mutate(
    Exceeds_PM25 = PM2.5 > 15,
    Exceeds_PM10 = PM10 > 45,
    Exceedance_Type = case_when(
      Exceeds_PM25 & Exceeds_PM10 ~ "Both PM2.5 & PM10",
      Exceeds_PM25 ~ "PM2.5 Only",
      Exceeds_PM10 ~ "PM10 Only",
      TRUE ~ "No Exceedance"
    )
  ) %>%
  # Filter out rows with no exceedance to keep the plot clean and focused
  filter(Exceedance_Type != "No Exceedance")

# Create the scatter plot
ggplot(cairo_exceedance_plot_data, aes(x = Date, y = AQI, color = Exceedance_Type)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Cairo AQI vs. Days Exceeding WHO Guidelines",
    subtitle = "Highlighting days when PM2.5 (>15 µg/m³) or PM10 (>45 µg/m³) were above limits",
    x = "Date",
    y = "AQI",
    color = "Exceedance Type"
  ) +
  theme_minimal()


# Make sure you have the tidyverse library loaded
library(tidyverse)

# Filter the data for Cairo and add a column to flag exceedances
raw_aqi_plot <- ggplot(df_aqi_clean, aes(x = Date, y = AQI, group = City, color = City)) +
  geom_line(alpha = 0.6) +
  labs(
    title = "AQI Over Time (All Cities)",
    subtitle = "Raw data before aggregation or modeling",
    x = "Date",
    y = "AQI",
    color = "City"
  ) +
  theme_minimal()

print(raw_aqi_plot)
