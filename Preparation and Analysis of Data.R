df <- read.csv("D:/Preparation and Analysis of Data/archive/city_market_tracker_2024_2025.csv")

head(df)
nrow(df)
str(df)

chooseCRANmirror()

install.packages("tidyverse")
install.packages("lubridate")
install.packages("corrplot")
install.packages("data.table")
install.packages("ggplot2")
install.packages("reshape2")

rm(df)   # remove the wrong “df function”

df <- read.csv("D:/Preparation and Analysis of Data/archive/city_market_tracker_2024_2025.csv")



###########################################################
# 1. Basic cleaning / preparation
###########################################################

# 1.1 Convert PERIOD_BEGIN and PERIOD_END from text to Date
# (this will make it easier to work with years and time)
df$PERIOD_BEGIN <- as.Date(df$PERIOD_BEGIN)
df$PERIOD_END   <- as.Date(df$PERIOD_END)


#Dataset Check
head(df)

# 1.2 Extract the year from PERIOD_BEGIN
# This confirms the file really contains only 2024 and 2025.
df$YEAR <- as.integer(format(df$PERIOD_BEGIN, "%Y"))
table(df$YEAR)  # I expect to see counts for 2024 and 2025 only.


###########################################################
# 2. Filter data for H1 and create variables
###########################################################

# H1: Cities with higher median list prices tend to have a larger
#     gap between list price and sale price, indicating that sellers
#     overprice in higher-priced markets.

# For this, I need both MEDIAN_LIST_PRICE and MEDIAN_SALE_PRICE.
# Rows without either of these are not useful, so I drop them.
df_h1 <- df[!is.na(df$MEDIAN_LIST_PRICE) & !is.na(df$MEDIAN_SALE_PRICE), ]

nrow(df_h1)  # how many observations remain for H1?

# 2.1 Create the price gap in dollars
# (How much higher the list price is compared to the sale price)
df_h1$PRICE_GAP <- df_h1$MEDIAN_LIST_PRICE - df_h1$MEDIAN_SALE_PRICE

# 2.2 Create the price gap as a percentage of the list price
# This normalises the gap so I can compare across cheap vs expensive cities.
df_h1$PRICE_GAP_PCT <- df_h1$PRICE_GAP / df_h1$MEDIAN_LIST_PRICE

# Quick summaries to understand the new variables
summary(df_h1$PRICE_GAP)
summary(df_h1$PRICE_GAP_PCT)

###########################################################
# 3. Aggregate to city level for H1
###########################################################

# For H1 I want city-level metrics:
#  - average median list price per city (across 2024–2025)
#  - average price gap per city (absolute and percentage)
#
# I use base R's aggregate(), grouped by CITY + STATE_CODE

city_summary <- aggregate(
  cbind(MEDIAN_LIST_PRICE, MEDIAN_SALE_PRICE, PRICE_GAP, PRICE_GAP_PCT) ~ CITY + STATE_CODE,
  data = df_h1,
  FUN = mean,
  na.rm = TRUE
)

head(city_summary)
nrow(city_summary)  # number of unique city–state combinations

###########################################################
# 4. Correlation + basic analysis for H1
###########################################################

# 4.1 Correlation between list price and dollar price gap
cor_gap_dollar <- cor(
  city_summary$MEDIAN_LIST_PRICE,
  city_summary$PRICE_GAP,
  use = "complete.obs"
)

# 4.2 Correlation between list price and percentage gap
cor_gap_pct <- cor(
  city_summary$MEDIAN_LIST_PRICE,
  city_summary$PRICE_GAP_PCT,
  use = "complete.obs"
)

cor_gap_dollar
cor_gap_pct


# Interpretation note:
# - Positive correlation: higher-priced cities tend to have larger gaps.
# - Negative correlation: higher-priced cities tend to have smaller gaps.
# - Near 0: very weak/no linear relationship.

###########################################################
# 5. Plots for H1 (for presentation)
###########################################################

# 5.1 Scatter plot: list price vs. price gap (dollars)
# Create a blue gradient palette
blue_pal <- colorRampPalette(c("#e0f3ff", "#74a9cf", "#045a8d"))

# Map colors to PRICE_GAP values
cols <- blue_pal(100)[cut(city_summary$PRICE_GAP, breaks = 100)]

plot(
  city_summary$MEDIAN_LIST_PRICE,
  city_summary$PRICE_GAP,
  main = "H1: List Price vs Price Gap (City Level, 2024–2025)",
  xlab = "Avg Median List Price (USD)",
  ylab = "Avg Price Gap (List - Sale, USD)",
  pch = 16,
  col = cols,   # ← apply gradient colors
  cex = 0.8
)

# Add regression line in dark navy (fits palette)
abline(
  lm(PRICE_GAP ~ MEDIAN_LIST_PRICE, data = city_summary),
  col = "#023858",   # navy blue for clarity
  lwd = 2
)


# 5.2 Scatter plot: list price vs. price gap percentage
# Create a blue gradient color palette
blue_pal <- colorRampPalette(c("#e0f3ff", "#74a9cf", "#045a8d"))

# Map colors to the PRICE_GAP_PCT values
cols <- blue_pal(100)[cut(city_summary$PRICE_GAP_PCT, breaks = 100)]

plot(
  city_summary$MEDIAN_LIST_PRICE,
  city_summary$PRICE_GAP_PCT,
  main = "H1: List Price vs Relative Price Gap (City Level)",
  xlab = "Avg Median List Price (USD)",
  ylab = "Avg Price Gap / List Price",
  pch = 16,
  col = cols,      # ← use gradient colors
  cex = 0.8
)

# Regression line (keep in blue or a darker navy)
abline(
  lm(PRICE_GAP_PCT ~ MEDIAN_LIST_PRICE, data = city_summary),
  col = "#023858",  # dark navy from the palette family
  lwd = 2
)

)

###########################################################
# 6. Correlation “heatmap” with key variables (H1 + H2)
###########################################################

# Here I prepare a small set of important variables at city level:
#   - MEDIAN_LIST_PRICE
#   - MEDIAN_SALE_PRICE
#   - PRICE_GAP
#   - AVG_SALE_TO_LIST      (market competitiveness)
#   - SOLD_ABOVE_LIST       (share sold above list)
#   - MEDIAN_DOM            (days on market)

# First, compute city-level averages for these “competitiveness” variables.
city_extra <- aggregate(
  cbind(AVG_SALE_TO_LIST, SOLD_ABOVE_LIST, MEDIAN_DOM) ~ CITY + STATE_CODE,
  data = df_h1,
  FUN = mean,
  na.rm = TRUE
)

# Merge price-related summary with competitiveness-related summary
city_all <- merge(
  city_summary,
  city_extra,
  by = c("CITY", "STATE_CODE"),
  all.x = TRUE
)

# Select only numeric variables for the correlation matrix
corr_vars <- c(
  "MEDIAN_LIST_PRICE",
  "MEDIAN_SALE_PRICE",
  "PRICE_GAP",
  "AVG_SALE_TO_LIST",
  "SOLD_ABOVE_LIST",
  "MEDIAN_DOM"
)


corr_data <- city_all[, corr_vars]

# Compute correlation matrix (pairwise to deal with some NAs)
corr_mat <- cor(corr_data, use = "pairwise.complete.obs")

corr_mat  # I can inspect the numeric values in the console

# Basic heatmap using only base R

install.packages("pheatmap")
library(pheatmap)

pheatmap(
  corr_mat,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Correlation Heatmap (Key Housing Metrics, 2024–2025)",
  color = colorRampPalette(c("#e0f3ff", "#74a9cf", "#045a8d"))(100),
  fontsize = 8,
  angle_col = 90
)


vars <- c(
  "MEDIAN_LIST_PRICE", "MEDIAN_SALE_PRICE",
  "AVG_SALE_TO_LIST", "SOLD_ABOVE_LIST",
  "MEDIAN_DOM", "INVENTORY",
  "NEW_LISTINGS", "HOMES_SOLD", "MONTHS_OF_SUPPLY"
)

df_heat <- df_h1[, vars]
corr    <- cor(df_heat, use = "pairwise.complete.obs")

############################################################
# 2. Colour palette (light blue → white → light red)
############################################################
my_colors <- colorRampPalette(c(
  "#3498db", "#a8d5f2", "#ffffff",
  "#f7b2a6", "#e74c3c"
))(300)

############################################################
# 3. Heatmap with CORRECT orientation
############################################################
par(mar = c(8, 10, 4, 2))

# Flip the matrix vertically so bottom-left matches top-left variable
corr_flipped <- corr[nrow(corr):1, ]

image(
  x    = 1:ncol(corr_flipped),
  y    = 1:nrow(corr_flipped),
  z    = t(corr_flipped),  # transpose for correct orientation
  col  = my_colors,
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "Correlation Heatmap",
  zlim = c(-1, 1)  # ensures color scale goes from -1 to 1
)

# X-axis (bottom) - same order as variable list
axis(1,
     at = 1:ncol(corr),
     labels = colnames(corr),
     las = 2, 
     cex.axis = 0.6)

# Y-axis (left) - same order as variable list (flipped for display)
axis(2,
     at = 1:nrow(corr),
     labels = rownames(corr_flipped),
     las = 2, 
     cex.axis = 0.6)

# Add correlation values inside each cell
for (i in 1:ncol(corr_flipped)) {
  for (j in 1:nrow(corr_flipped)) {
    text(i, j, sprintf("%.2f", corr_flipped[j, i]), 
         cex = 0.65, 
         col = ifelse(abs(corr_flipped[j, i]) > 0.5, "white", "black"))
  }
}

# Add a grid for better readability
abline(h = 0.5:(nrow(corr) + 0.5), col = "white", lwd = 1)
abline(v = 0.5:(ncol(corr) + 0.5), col = "white", lwd = 1)

# Optional: Add a box around the plot
box()

###############################################################
# NEW STRONG PLOT FOR H1: Boxplot of Price Gap by Price Level
###############################################################

# Step 1: Categorize cities into price groups
city_summary$PRICE_LEVEL <- cut(
  city_summary$MEDIAN_LIST_PRICE,
  breaks = quantile(city_summary$MEDIAN_LIST_PRICE, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
  labels = c("Low-Price Markets", "Mid-Price Markets", "High-Price Markets"),
  include.lowest = TRUE
)

# Step 2: Create boxplot of PRICE_GAP across categories
boxplot(
  PRICE_GAP ~ PRICE_LEVEL,
  data = city_summary,
  col = c("#76c7c0", "#4fa3b6", "#2a6f97"),
  main = "H1: Price Gap Across Low, Mid, and High Price Markets",
  ylab = "Average Price Gap (List – Sale, USD)",
  xlab = "Market Price Category (City Level)"
)

# Add mean points to make the trend more visible
means <- tapply(city_summary$PRICE_GAP, city_summary$PRICE_LEVEL, mean, na.rm = TRUE)
points(1:3, means, pch = 19, col = "red", cex = 1.5)




# Create bins for list price (10 groups)
df_h1$list_price_bin <- cut(
    df_h1$MEDIAN_LIST_PRICE,
    breaks = quantile(df_h1$MEDIAN_LIST_PRICE, probs = seq(0, 1, 0.1), na.rm = TRUE),
    include.lowest = TRUE
)

# Compute average gap per bin
bin_summary <- aggregate(
    PRICE_GAP ~ list_price_bin,
    data = df_h1,
    FUN = mean
)

# Plot
plot(
    bin_summary$list_price_bin,
    bin_summary$PRICE_GAP,
    type = "b", pch = 19, col = "blue",
    main = "H1: Average Price Gap Across Increasing List Price Bins",
    xlab = "List Price Bins (Low → High)",
    ylab = "Average Price Gap (USD)"
)


boxplot(
    PRICE_GAP ~ list_price_bin,
    data = df_h1,
    col = colorRampPalette(c("lightblue", "steelblue"))(10),
    main = "Price Gap by Market Price",
    xlab = "Market Price Category (Ascending List Price)",
    ylab = "Price Gap (USD)"
)


city_summary_ordered <- city_summary[order(city_summary$MEDIAN_LIST_PRICE), ]

plot(
    city_summary_ordered$MEDIAN_LIST_PRICE,
    city_summary_ordered$PRICE_GAP,
    type = "l", col = "darkgreen", lwd = 2,
    main = "H1: Relationship Between List Price and Price Gap",
    xlab = "Median List Price (USD)",
    ylab = "Price Gap (USD)"
)

###########################################################
# H2 — Market Competitiveness (Base R)
# H2: More competitive cities show higher sale-to-list ratios
#     and lower days on market.
###########################################################

# NOTE:
# This analysis is done at CITY LEVEL by taking the mean over 2024–2025.
# That makes results easier to interpret than row-level noise.

###########################################################
# 1) Keep only rows where the key H2 variables exist
###########################################################

df_h2 <- df[!is.na(df$AVG_SALE_TO_LIST) & !is.na(df$MEDIAN_DOM), ]

# Quick check: how much data remains?
nrow(df_h2)

###########################################################
# 2) Aggregate to city-level averages (CITY + STATE_CODE)
###########################################################

city_h2 <- aggregate(
  cbind(AVG_SALE_TO_LIST, MEDIAN_DOM, SOLD_ABOVE_LIST) ~ CITY + STATE_CODE,
  data = df_h2,
  FUN = mean,
  na.rm = TRUE
)

head(city_h2)
nrow(city_h2)

###########################################################
# 3) Correlation tests (the key evidence for H2)
###########################################################

# 3.1 Expected: AVG_SALE_TO_LIST vs MEDIAN_DOM should be NEGATIVE
cor_h2_dom <- cor(city_h2$AVG_SALE_TO_LIST, city_h2$MEDIAN_DOM, use = "complete.obs")

# 3.2 Optional: SOLD_ABOVE_LIST vs MEDIAN_DOM should also be NEGATIVE
cor_h2_above <- cor(city_h2$SOLD_ABOVE_LIST, city_h2$MEDIAN_DOM, use = "complete.obs")

cor_h2_dom
cor_h2_above

# NOTE for interpretation:
# - Negative correlation means: higher competitiveness -> lower days on market (supports H2)

###########################################################
# 4) Regression analysis (stronger test than correlation)
###########################################################

# 4.1 Simple regression: Days on market explained by sale-to-list ratio
m_h2_simple <- lm(MEDIAN_DOM ~ AVG_SALE_TO_LIST, data = city_h2)
summary(m_h2_simple)

# 4.2 Multivariate perspective:
# Control for SOLD_ABOVE_LIST too (two competitiveness measures together)
m_h2_multi <- lm(MEDIAN_DOM ~ AVG_SALE_TO_LIST + SOLD_ABOVE_LIST, data = city_h2)
summary(m_h2_multi)

# NOTE:
# In regression:
# - If coefficient of AVG_SALE_TO_LIST is negative -> supports H2
# - p-value tells if the relationship is statistically strong (not random)

###########################################################
# 5) Plot (for presentation)
###########################################################

plot(
  city_h2$AVG_SALE_TO_LIST,
  city_h2$MEDIAN_DOM,
  main = "H2: Sale-to-List Ratio vs Days on Market (City Level, 2024–2025)",
  xlab = "Average Sale-to-List Ratio",
  ylab = "Average Median Days on Market (DOM)",
  pch = 16,
  cex = 0.6
)

abline(m_h2_simple, col = "red", lwd = 2)

# Add correlation on the plot (simple and clear)
legend(
  "topright",
  legend = paste("Correlation =", round(cor_h2_dom, 3)),
  bty = "n"
)
###########################################################
# Additional H2 Plot: DOM by Competitiveness Level
###########################################################

# Create competitiveness groups using sale-to-list ratio
city_h2$COMPETITIVENESS <- cut(
  city_h2$AVG_SALE_TO_LIST,
  breaks = quantile(city_h2$AVG_SALE_TO_LIST, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
  labels = c("Low Competitiveness", "Medium Competitiveness", "High Competitiveness"),
  include.lowest = TRUE
)

# Boxplot
boxplot(
  MEDIAN_DOM ~ COMPETITIVENESS,
  data = city_h2,
  main = "H2: Days on Market by Market Competitiveness (2024–2025)",
  xlab = "Market Competitiveness Level",
  ylab = "Average Median Days on Market",
  col = c("#d9d9d9", "#9ecae1", "#3182bd"),
  outline = FALSE
)


###########################################################
# H3 — Supply/Demand Balance and Short-Term Price Change
# H3: Strong demand relative to supply leads to short-term price increases.
###########################################################

###########################################################
# 1) Keep only rows with the needed variables
###########################################################

df_h3 <- df[
  !is.na(df$MEDIAN_SALE_PRICE_MOM) &
  !is.na(df$INVENTORY) &
  !is.na(df$PENDING_SALES) &
  !is.na(df$HOMES_SOLD) &
  df$INVENTORY > 0,   # avoid division by zero
]

nrow(df_h3)

###########################################################
# 2) Create a simple demand pressure measure
###########################################################

# NOTE:
# If demand is high and inventory is low, this number increases.
# Higher value = stronger demand relative to supply.
df_h3$DEMAND_PRESSURE <- (df_h3$PENDING_SALES + df_h3$HOMES_SOLD) / df_h3$INVENTORY

summary(df_h3$DEMAND_PRESSURE)

###########################################################
# 3) Aggregate to CITY LEVEL (average over 2024–2025)
###########################################################

city_h3 <- aggregate(
  cbind(MEDIAN_SALE_PRICE_MOM, DEMAND_PRESSURE, INVENTORY, PENDING_SALES, HOMES_SOLD) ~ CITY + STATE_CODE,
  data = df_h3,
  FUN = mean,
  na.rm = TRUE
)

head(city_h3)
nrow(city_h3)

###########################################################
# 4) Correlation (direct evidence for H3)
###########################################################

# Expected: DEMAND_PRESSURE vs MEDIAN_SALE_PRICE_MOM should be POSITIVE
cor_h3 <- cor(city_h3$DEMAND_PRESSURE, city_h3$MEDIAN_SALE_PRICE_MOM, use = "complete.obs")
cor_h3

###########################################################
# 5) Regression analysis (stronger perspective)
###########################################################

# 5.1 Simple regression: MoM price change explained by demand pressure
m_h3_simple <- lm(MEDIAN_SALE_PRICE_MOM ~ DEMAND_PRESSURE, data = city_h3)
summary(m_h3_simple)

# 5.2 Multivariate regression (control for supply and demand components)
# This checks whether demand pressure still matters after considering inventory and sales.
m_h3_multi <- lm(MEDIAN_SALE_PRICE_MOM ~ DEMAND_PRESSURE + INVENTORY + PENDING_SALES + HOMES_SOLD, data = city_h3)
summary(m_h3_multi)

###########################################################
# 6) Plot (for presentation)
###########################################################

plot(
  city_h3$DEMAND_PRESSURE,
  city_h3$MEDIAN_SALE_PRICE_MOM,
  main = "H3: Demand Pressure vs MoM Sale Price Change (City Level, 2024–2025)",
  xlab = "Demand Pressure = (Pending + Sold) / Inventory",
  ylab = "Average MoM Change in Median Sale Price",
  pch = 16,
  cex = 0.6
)

abline(m_h3_simple, col = "red", lwd = 2)

legend(
  "topright",
  legend = paste("Correlation =", round(cor_h3, 3)),
  bty = "n"
)




###########################################################
# H3 (Extended) — Demand Pressure and YoY Price Change
# Refined hypothesis:
# Strong demand relative to supply affects prices over time
# (Year-over-Year), not immediately Month-over-Month.
###########################################################

###########################################################
# 1) Keep only rows with required variables
###########################################################

df_h3_yoy <- df[
  !is.na(df$MEDIAN_SALE_PRICE_YOY) &
  !is.na(df$INVENTORY) &
  !is.na(df$PENDING_SALES) &
  !is.na(df$HOMES_SOLD) &
  df$INVENTORY > 0,   # avoid division by zero
]

# Check remaining rows
nrow(df_h3_yoy)

###########################################################
# 2) Create Demand Pressure variable
###########################################################

# Demand Pressure = (Pending Sales + Homes Sold) / Inventory
# Higher value = stronger demand relative to supply
df_h3_yoy$DEMAND_PRESSURE <-
  (df_h3_yoy$PENDING_SALES + df_h3_yoy$HOMES_SOLD) / df_h3_yoy$INVENTORY

summary(df_h3_yoy$DEMAND_PRESSURE)

###########################################################
# 3) Aggregate to CITY LEVEL (average across 2024–2025)
###########################################################

city_h3_yoy <- aggregate(
  cbind(MEDIAN_SALE_PRICE_YOY, DEMAND_PRESSURE) ~ CITY + STATE_CODE,
  data = df_h3_yoy,
  FUN = mean,
  na.rm = TRUE
)

head(city_h3_yoy)
nrow(city_h3_yoy)

###########################################################
# 4) Correlation Analysis (Key Evidence)
###########################################################

# Expected sign: POSITIVE
# Higher demand pressure → higher YoY price growth
cor_h3_yoy <- cor(
  city_h3_yoy$DEMAND_PRESSURE,
  city_h3_yoy$MEDIAN_SALE_PRICE_YOY,
  use = "complete.obs"
)

cor_h3_yoy

###########################################################
# 5) Regression Analysis (Stronger Test)
###########################################################

# Simple linear regression
m_h3_yoy <- lm(
  MEDIAN_SALE_PRICE_YOY ~ DEMAND_PRESSURE,
  data = city_h3_yoy
)

summary(m_h3_yoy)

###########################################################
# 6) Plot for Presentation
###########################################################

plot(
  city_h3_yoy$DEMAND_PRESSURE,
  city_h3_yoy$MEDIAN_SALE_PRICE_YOY,
  main = "H3: Demand Pressure vs YoY Sale Price Change\n(City Level, 2024–2025)",
  xlab = "Demand Pressure = (Pending + Sold) / Inventory",
  ylab = "Average YoY Change in Median Sale Price",
  pch = 16,
  cex = 0.6
)

# Add regression line
abline(m_h3_yoy, col = "red", lwd = 2)

# Add correlation value to plot
legend(
  "topright",
  legend = paste("Correlation =", round(cor_h3_yoy, 3)),
  bty = "n"
)


# Spearman correlation (rank-based, robust to outliers)
cor(city_h3_yoy$DEMAND_PRESSURE, city_h3_yoy$MEDIAN_SALE_PRICE_YOY,
    method="spearman", use="complete.obs")

# Kendall correlation (even more robust)
cor(city_h3_yoy$DEMAND_PRESSURE, city_h3_yoy$MEDIAN_SALE_PRICE_YOY,
    method="kendall", use="complete.obs")

# Create tertiles (3 groups)
q <- quantile(city_h3_yoy$DEMAND_PRESSURE, probs=c(0, 1/3, 2/3, 1), na.rm=TRUE)
city_h3_yoy$DP_GROUP <- cut(city_h3_yoy$DEMAND_PRESSURE,
                           breaks=q, include.lowest=TRUE,
                           labels=c("Low", "Medium", "High"))

# Summary by group
tapply(city_h3_yoy$MEDIAN_SALE_PRICE_YOY, city_h3_yoy$DP_GROUP, summary)

# Boxplot (very presentation-friendly)
boxplot(MEDIAN_SALE_PRICE_YOY ~ DP_GROUP, data=city_h3_yoy,
        main="YoY Price Change by Demand Pressure Group",
        xlab="Demand Pressure (Low → High)", ylab="Avg YoY Change in Median Sale Price")





# Remove extreme YoY changes (example: keep between 1st and 99th percentile)
lo <- quantile(df$MEDIAN_SALE_PRICE_YOY, 0.01, na.rm=TRUE)
hi <- quantile(df$MEDIAN_SALE_PRICE_YOY, 0.99, na.rm=TRUE)

df_clean <- df[!is.na(df$MEDIAN_SALE_PRICE_YOY) &
               df$MEDIAN_SALE_PRICE_YOY >= lo &
               df$MEDIAN_SALE_PRICE_YOY <= hi, ]
###########################################################
# H3 — Re-do after OUTLIER CLEANING (Base R only)
# Goal: Check if outliers in price-change are hiding the relationship.
#
# H3: Strong demand relative to supply leads to short-term price increases.
# Here we test again using:
#  - MoM change (MEDIAN_SALE_PRICE_MOM)
#  - YoY change (MEDIAN_SALE_PRICE_YOY)
# after trimming extreme outliers.
###########################################################

###########################################################
# 0) Safety checks (make sure df exists)
###########################################################
# NOTE:
# If df is not loaded, run your read.csv(...) step first.
stopifnot(exists("df"))

###########################################################
# 1) Helper function: trim outliers using percentiles
###########################################################
# NOTE:
# This keeps only "typical" values and removes extreme spikes.
# 1% and 99% is a common, defensible choice for large datasets.
trim_by_percentile <- function(x, p_low = 0.01, p_high = 0.99) {
  lo <- as.numeric(quantile(x, p_low, na.rm = TRUE))
  hi <- as.numeric(quantile(x, p_high, na.rm = TRUE))
  list(lo = lo, hi = hi)
}

###########################################################
# 2) H3 (MoM) — clean outliers + redo analysis
###########################################################
# NOTE:
# Variables needed:
# - MEDIAN_SALE_PRICE_MOM  (outcome)
# - INVENTORY, PENDING_SALES, HOMES_SOLD (to build demand pressure)
# Also: INVENTORY > 0 to avoid division by zero

df_h3_mom_raw <- df[
  !is.na(df$MEDIAN_SALE_PRICE_MOM) &
  !is.na(df$INVENTORY) &
  !is.na(df$PENDING_SALES) &
  !is.na(df$HOMES_SOLD) &
  df$INVENTORY > 0,
]

# 2.1 Trim outliers in MoM change
mom_cut <- trim_by_percentile(df_h3_mom_raw$MEDIAN_SALE_PRICE_MOM, 0.01, 0.99)

df_h3_mom <- df_h3_mom_raw[
  df_h3_mom_raw$MEDIAN_SALE_PRICE_MOM >= mom_cut$lo &
  df_h3_mom_raw$MEDIAN_SALE_PRICE_MOM <= mom_cut$hi,
]

cat("H3 MoM rows BEFORE trimming:", nrow(df_h3_mom_raw), "\n")
cat("H3 MoM rows AFTER  trimming:", nrow(df_h3_mom), "\n")
cat("MoM kept range:", mom_cut$lo, "to", mom_cut$hi, "\n\n")

# 2.2 Create demand pressure
df_h3_mom$DEMAND_PRESSURE <- (df_h3_mom$PENDING_SALES + df_h3_mom$HOMES_SOLD) / df_h3_mom$INVENTORY

# 2.3 Aggregate to city-level means (2024–2025)
city_h3_mom <- aggregate(
  cbind(MEDIAN_SALE_PRICE_MOM, DEMAND_PRESSURE, INVENTORY, PENDING_SALES, HOMES_SOLD) ~ CITY + STATE_CODE,
  data = df_h3_mom,
  FUN = mean,
  na.rm = TRUE
)

# 2.4 Correlation + regression (after cleaning)
cor_h3_mom_clean <- cor(city_h3_mom$DEMAND_PRESSURE, city_h3_mom$MEDIAN_SALE_PRICE_MOM, use = "complete.obs")
m_h3_mom_clean   <- lm(MEDIAN_SALE_PRICE_MOM ~ DEMAND_PRESSURE, data = city_h3_mom)

cat("Cleaned H3 (MoM) correlation:", round(cor_h3_mom_clean, 4), "\n")
print(summary(m_h3_mom_clean))

# 2.5 Plot (after cleaning)
plot(
  city_h3_mom$DEMAND_PRESSURE,
  city_h3_mom$MEDIAN_SALE_PRICE_MOM,
  main = "H3 (Cleaned): Demand Pressure vs MoM Sale Price Change (City Level)",
  xlab = "Demand Pressure = (Pending + Sold) / Inventory",
  ylab = "Avg MoM Change in Median Sale Price",
  pch = 16,
  cex = 0.6
)
abline(m_h3_mom_clean, col = "red", lwd = 2)
legend("topright", legend = paste("Correlation =", round(cor_h3_mom_clean, 3)), bty = "n")

###########################################################
# 3) H3 (YoY) — clean outliers + redo analysis
###########################################################
# NOTE:
# Same idea as MoM, but the outcome is MEDIAN_SALE_PRICE_YOY.

df_h3_yoy_raw <- df[
  !is.na(df$MEDIAN_SALE_PRICE_YOY) &
  !is.na(df$INVENTORY) &
  !is.na(df$PENDING_SALES) &
  !is.na(df$HOMES_SOLD) &
  df$INVENTORY > 0,
]

# 3.1 Trim outliers in YoY change
yoy_cut <- trim_by_percentile(df_h3_yoy_raw$MEDIAN_SALE_PRICE_YOY, 0.01, 0.99)

df_h3_yoy <- df_h3_yoy_raw[
  df_h3_yoy_raw$MEDIAN_SALE_PRICE_YOY >= yoy_cut$lo &
  df_h3_yoy_raw$MEDIAN_SALE_PRICE_YOY <= yoy_cut$hi,
]

cat("\nH3 YoY rows BEFORE trimming:", nrow(df_h3_yoy_raw), "\n")
cat("H3 YoY rows AFTER  trimming:", nrow(df_h3_yoy), "\n")
cat("YoY kept range:", yoy_cut$lo, "to", yoy_cut$hi, "\n\n")

# 3.2 Create demand pressure
df_h3_yoy$DEMAND_PRESSURE <- (df_h3_yoy$PENDING_SALES + df_h3_yoy$HOMES_SOLD) / df_h3_yoy$INVENTORY

# 3.3 Aggregate to city-level means (2024–2025)
city_h3_yoy <- aggregate(
  cbind(MEDIAN_SALE_PRICE_YOY, DEMAND_PRESSURE, INVENTORY, PENDING_SALES, HOMES_SOLD) ~ CITY + STATE_CODE,
  data = df_h3_yoy,
  FUN = mean,
  na.rm = TRUE
)

# 3.4 Correlation + regression (after cleaning)
cor_h3_yoy_clean <- cor(city_h3_yoy$DEMAND_PRESSURE, city_h3_yoy$MEDIAN_SALE_PRICE_YOY, use = "complete.obs")
m_h3_yoy_clean   <- lm(MEDIAN_SALE_PRICE_YOY ~ DEMAND_PRESSURE, data = city_h3_yoy)

cat("Cleaned H3 (YoY) correlation:", round(cor_h3_yoy_clean, 4), "\n")
print(summary(m_h3_yoy_clean))

# 3.5 Plot (after cleaning)
plot(
  city_h3_yoy$DEMAND_PRESSURE,
  city_h3_yoy$MEDIAN_SALE_PRICE_YOY,
  main = "H3 (Cleaned): Demand Pressure vs YoY Sale Price Change (City Level)",
  xlab = "Demand Pressure = (Pending + Sold) / Inventory",
  ylab = "Avg YoY Change in Median Sale Price",
  pch = 16,
  cex = 0.6
)
abline(m_h3_yoy_clean, col = "red", lwd = 2)
legend("topright", legend = paste("Correlation =", round(cor_h3_yoy_clean, 3)), bty = "n")

###########################################################
# 4) Interpretation notes (for yourself)
###########################################################
# If correlation/regression becomes stronger AFTER trimming:
#   -> Outliers were hiding a real relationship in typical markets.
#
# If correlation stays ~0 even AFTER trimming:
#   -> Demand pressure (as defined here) does not explain price change well.
#      It may explain market speed (DOM) better, like H2.
#
# Next best step (if still ~0):
#   -> Replace demand pressure with MONTHS_OF_SUPPLY, AVG_SALE_TO_LIST,
#      SOLD_ABOVE_LIST, PRICE_DROPS and repeat the same workflow.
###########################################################

###########################################################
# H3 (YoY) — Memory-safe redo:
# Aggregate to city-level FIRST, then remove outlier cities.
###########################################################

# 0) Keep only the columns needed (saves memory)
df_yoy_small <- df[, c("CITY","STATE_CODE","MEDIAN_SALE_PRICE_YOY",
                       "INVENTORY","PENDING_SALES","HOMES_SOLD")]

# 1) Filter only valid rows (no NAs, inventory > 0)
keep <- !is.na(df_yoy_small$MEDIAN_SALE_PRICE_YOY) &
        !is.na(df_yoy_small$INVENTORY) &
        !is.na(df_yoy_small$PENDING_SALES) &
        !is.na(df_yoy_small$HOMES_SOLD) &
        df_yoy_small$INVENTORY > 0

df_yoy_small <- df_yoy_small[keep, ]

# (optional) free memory from the logical vector
rm(keep); gc()

# 2) Create Demand Pressure at row level
df_yoy_small$DEMAND_PRESSURE <- (df_yoy_small$PENDING_SALES + df_yoy_small$HOMES_SOLD) / df_yoy_small$INVENTORY

# 3) Aggregate to CITY LEVEL (this becomes much smaller)
city_h3_yoy <- aggregate(
  cbind(MEDIAN_SALE_PRICE_YOY, DEMAND_PRESSURE) ~ CITY + STATE_CODE,
  data = df_yoy_small,
  FUN = mean,
  na.rm = TRUE
)

# Free memory
rm(df_yoy_small); gc()

# 4) Outlier trimming at CITY level (much safer)
lo <- as.numeric(quantile(city_h3_yoy$MEDIAN_SALE_PRICE_YOY, 0.01, na.rm = TRUE))
hi <- as.numeric(quantile(city_h3_yoy$MEDIAN_SALE_PRICE_YOY, 0.99, na.rm = TRUE))

city_h3_yoy_clean <- city_h3_yoy[
  city_h3_yoy$MEDIAN_SALE_PRICE_YOY >= lo &
  city_h3_yoy$MEDIAN_SALE_PRICE_YOY <= hi,
]

cat("City rows BEFORE trimming:", nrow(city_h3_yoy), "\n")
cat("City rows AFTER  trimming:", nrow(city_h3_yoy_clean), "\n")
cat("YoY kept range:", lo, "to", hi, "\n\n")

# 5) Correlation + regression (city-level, cleaned)
cor_yoy <- cor(city_h3_yoy_clean$DEMAND_PRESSURE,
               city_h3_yoy_clean$MEDIAN_SALE_PRICE_YOY,
               use = "complete.obs")

m_yoy <- lm(MEDIAN_SALE_PRICE_YOY ~ DEMAND_PRESSURE, data = city_h3_yoy_clean)

cat("Cleaned City-level YoY correlation:", round(cor_yoy, 4), "\n")
print(summary(m_yoy))

# 6) Plot
plot(city_h3_yoy_clean$DEMAND_PRESSURE,
     city_h3_yoy_clean$MEDIAN_SALE_PRICE_YOY,
     main = "H3 (Cleaned, City-Level): Demand Pressure vs YoY Price Change",
     xlab = "Demand Pressure = (Pending + Sold) / Inventory",
     ylab = "Avg YoY Change in Median Sale Price",
     pch = 16, cex = 0.6)

abline(m_yoy, col = "red", lwd = 2)
legend("topright", legend = paste("Correlation =", round(cor_yoy, 3)), bty = "n")



