# 🌍 Global Air Quality Analysis: A Data-Driven Perspective

This project conducts a comprehensive analysis of global air quality data from six major cities over the year 2024.  
The goal is to identify key pollution drivers, compare air quality trends across different regions, assess public health impacts based on WHO guidelines, and build predictive models to forecast future trends.  

The entire analysis is performed using the **R programming language**.  

---

## 💡 Key Findings

- **Primary Pollutants**: Particulate matter (**PM2.5** and **PM10**) are the most significant contributors to poor air quality, with strong correlations to the overall Air Quality Index (AQI).  
- **Global Disparity**: A major difference in air quality was observed, with some cities consistently exceeding WHO health benchmarks far more often than others.  
- **Predictive Power**: Time-series models can effectively forecast future air quality, but model complexity and accuracy are highly dependent on the unique air quality patterns of each city.  

---

## 🔬 Methodology

The project follows a standard data science workflow:

1. **Data Wrangling**  
   - Raw dataset: 52,000+ hourly measurements  
   - Cleaned for missing values and date formatting  
   - Final dataset: **50,508 records**  

2. **Exploratory Data Analysis (EDA)**  
   - Correlation analysis  
   - Geospatial comparisons  

3. **Statistical Modeling**  
   - ARIMA models used to forecast weekly AQI trends for selected cities  

---

## 📊 Visualizations

👉 Drag and drop your image files directly into the placeholders below after running the analysis.

### 🔗 Pollutant Correlation
Heatmap showing relationships between pollutants and AQI.  

<img width="701" height="304" alt="CorrelationHeatmapPlot" src="https://github.com/user-attachments/assets/e2a4bdbc-8977-47d5-802c-09538e7f1c08" />


---

### ⚕️ Health Impact Comparison
Bar chart comparing the number of days each city's **PM2.5** and **PM10** exceeded WHO guidelines.  
<img width="718" height="433" alt="HealtImpactForeachcities" src="https://github.com/user-attachments/assets/1fe0fb81-d81f-48a9-beb8-f3c43f98faa9" />


---

### 📈 Time-Series Forecasts
Plots showing historical AQI and **4-week forecasts** for three cities.  
<img width="718" height="433" alt="NewyorkTimeForecast AQI" src="https://github.com/user-attachments/assets/31d95a27-3a2a-4a7a-b420-e822813a18b1" />
<img width="718" height="433" alt="LondonTimeForecast AQI" src="https://github.com/user-attachments/assets/878f985b-2165-487b-b8d6-ea6d7b4ae7e1" />
<img width="718" height="433" alt="CairoTimeForecast AQI" src="https://github.com/user-attachments/assets/c581228f-b5d9-4e1e-bca4-38e9356ce46c" />

---

## 🚀 Getting Started

This project was built using **R**. To reproduce the analysis:

### 1. Clone the Repository
```
git clone https://github.com/yourusername/your-repo-name.git
cd your-repo-name

```

## 🚀 Install Required Packages

###### You can install everything with a single command:

```
source("requirements.R")
```

## 🙌 Credits & Licensing

- Developed by **Ibrahim**  
- Data sourced from a **global air quality dataset for 2024**  
- Licensed under the **MIT License**  
