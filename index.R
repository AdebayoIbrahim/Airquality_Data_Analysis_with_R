# -----------PHASE ONE-----------
#install required packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("corrplot")
# load library()
library("readr")
library("dplyr")
library("ggplot2")
library("dplyr")
library(tidyr)
library(ggplot2)
#setting working directory
setwd("/home/setup/Desktop/Sta-334-Project")
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









