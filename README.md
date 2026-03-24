#  US Housing Market Analysis (2024 - 2025)

This project analyzes the US housing market using city-level data to understand pricing behavior, market competitiveness, and supply-demand dynamics.

The analysis is performed using **R** and focuses on testing three key hypotheses related to housing prices and market trends.

---

## Key Takeaways
- ESG factors can predict profitability without using direct financial variables
- Random Forest achieved 86% accuracy after calibration
- Environmental and Governance scores were the strongest predictors
- Model is suitable for financial decision support

##  Dataset Overview

-  831,771 rows and 58 variables :contentReference[oaicite:0]{index=0}  
-  City-level housing data  
-  Monthly observations for 2024 - 2025  
-  Includes:
  - Prices (list & sale)
  - Inventory & supply metrics
  - Sales activity
  - Market competitiveness indicators

---

##  Objectives

The project investigates the following hypotheses:

###  H1: Pricing Behavior
> Cities with higher median list prices tend to have a larger gap between list price and sale price.

###  H2: Market Competitiveness
> More competitive cities show higher sale-to-list ratios and lower days on market.

###  H3: Supply vs Demand
> Cities with stronger demand relative to supply experience higher short-term price growth.

---

##  Tools & Technologies

- **R Programming**
- Libraries:
  - `tidyverse`
  - `ggplot2`
  - `data.table`
  - `corrplot`
  - `pheatmap`
- Data Visualization & Statistical Analysis

---

##  Methodology

###  Data Preparation
- Converted date fields to proper format
- Filtered missing values
- Created derived variables such as:
  - Price Gap (List - Sale)
  - Price Gap %
  - Demand Pressure = (Pending Sales + Homes Sold) / Inventory :contentReference[oaicite:1]{index=1}  

---

###  Analysis Approach
- City-level aggregation
- Correlation analysis
- Linear regression models
- Outlier removal using percentile trimming
- Visualization using scatter plots, boxplots, and heatmaps

---

##  Key Findings

###  H1 - Strongly Supported
- Positive correlation between list price and price gap: **+0.72** :contentReference[oaicite:2]{index=2}  
- High-priced cities show larger **absolute gaps**
- No significant relationship in **percentage terms**

---

###  H2 - Supported
- Sale-to-list ratio vs days on market: **-0.216 correlation** :contentReference[oaicite:3]{index=3}  
- More competitive markets:
  - Sell faster
  - Have higher sale-to-list ratios

---

###  H3 - Weakly Supported
- Very weak correlation after cleaning: **~ +0.035** :contentReference[oaicite:4]{index=4}  
- Demand pressure has limited impact on price growth
- Suggests other factors influence price changes

---

##  Visualizations

The project includes:
- Scatter plots (price vs gap, demand vs price change)
- Boxplots (market competitiveness levels)
- Correlation heatmaps
- Regression trend lines

---

##  Academic Context

This project was developed as part of the course **"Preparation and Analysis of Data"**.

The objective of the course is to apply statistical thinking, data preprocessing, and exploratory data analysis techniques to real-world datasets. This project demonstrates the practical implementation of those concepts using a large-scale housing dataset.

---

##  Concepts Applied from the Course

The following key concepts from the course were explored and implemented:

###  Data Preparation & Cleaning
- Handling missing values and filtering relevant observations
- Converting raw date fields into usable formats
- Creating structured datasets for analysis

###  Feature Engineering
- Constructed new variables such as:
  - **Price Gap** (List Price − Sale Price)
  - **Price Gap %**
  - **Demand Pressure Metric**
- Enabled deeper analytical insights beyond raw data

###  Exploratory Data Analysis (EDA)
- Summary statistics and distributions
- Identification of patterns and relationships
- Visualization of trends using scatter plots, boxplots, and heatmaps

###  Statistical Analysis
- Correlation analysis to measure relationships between variables
- Linear regression models to test hypotheses
- Interpretation of coefficients, p-values, and model fit (R²)

###  Data Aggregation
- Aggregated large-scale data (800K+ rows) to **city-level insights**
- Improved interpretability and reduced noise

###  Outlier Handling
- Applied percentile-based trimming (1st–99th percentile)
- Ensured robust and reliable statistical results

###  Data Visualization
- Built clear and interpretable visualizations to support findings
- Used plots to communicate relationships and validate hypotheses

---

##  Final Conclusion

This project provides a data-driven analysis of the US housing market and demonstrates how statistical techniques can be applied to real-world datasets.

- **H1 (Pricing Behavior)**: Strongly supported - higher-priced cities show larger absolute price gaps, but not relative overpricing.
- **H2 (Market Competitiveness)**: Supported - competitive markets sell faster and have higher sale-to-list ratios.
- **H3 (Supply vs Demand)**: Weakly supported - demand pressure alone is not a strong predictor of price growth.

###  Overall Insight

The analysis highlights that:
- Housing market behavior is influenced by **multiple interacting factors**, not just price or demand alone.
- **Market competitiveness** plays a more significant role in transaction speed than in price growth.
- Proper data preparation and statistical analysis are crucial for extracting meaningful insights from large datasets.

---

##  Learning Outcome

Through this project, the following skills were strengthened:

- Real-world data handling and preprocessing
- Applying statistical methods to test hypotheses
- Interpreting analytical results critically
- Communicating insights through visualization and structured reporting

This project reflects the successful application of theoretical concepts from the course into a practical, real-world data analysis scenario.
