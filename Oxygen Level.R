# ============================================================================
# Fish Survival Analysis Based on Dissolved Oxygen (DO) – Cleaned & Visualized
# ============================================================================

rm(list = ls())

# ---------------------------
# Load required packages
# ---------------------------
required_packages <- c("readxl", "dplyr", "ggplot2")
for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---------------------------
# Load the data
# ---------------------------
file_path <- "D:/R Projects/Water Quality/labdatamain-8-23-2022.xlsx"
water_data <- read_excel(file_path)

# ---------------------------
# Inspect columns
# ---------------------------
names(water_data)

# ---------------------------
# Identify dissolved oxygen column
# Replace with actual column name if different
# ---------------------------
do_col <- "nResult"

# ---------------------------
# Clean DO data: convert to numeric and remove extreme/unrealistic values
# ---------------------------
water_data <- water_data %>%
  mutate(DO_mgL = as.numeric(as.character(.data[[do_col]]))) %>%
  filter(!is.na(DO_mgL) & DO_mgL >= 0 & DO_mgL <= 20)  # realistic DO range

# ---------------------------
# Take a random sample (10,000 rows or less if dataset smaller)
# ---------------------------
set.seed(123)
sample_size <- min(10000, nrow(water_data))
water_sample <- water_data %>% sample_n(sample_size)

# ---------------------------
# Define critical thresholds for risk levels
# ---------------------------
critical_DO <- 2      # mg/L, high risk threshold
low_DO <- 4           # mg/L, moderate risk threshold

# ---------------------------
# Flag risk levels
# ---------------------------
water_sample <- water_sample %>%
  mutate(
    FishRisk = case_when(
      DO_mgL < critical_DO ~ "High Risk",
      DO_mgL >= critical_DO & DO_mgL < low_DO ~ "Moderate Risk",
      TRUE ~ "Safe"
    )
  )

# ---------------------------
# Summary statistics
# ---------------------------
risk_summary <- water_sample %>%
  group_by(FishRisk) %>%
  summarise(
    Count = n(),
    Mean_DO = mean(DO_mgL, na.rm = TRUE),
    Min_DO = min(DO_mgL, na.rm = TRUE),
    Max_DO = max(DO_mgL, na.rm = TRUE),
    .groups = "drop"
  )
print(risk_summary)

# ---------------------------
# Percentage of samples in each risk category
# ---------------------------
risk_percent <- water_sample %>%
  group_by(FishRisk) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = round(100 * Count / sum(Count), 1))
print(risk_percent)

# ---------------------------
# Visualization: Clear bar chart of Percent by FishRisk
# ---------------------------
ggplot(risk_percent, aes(x = FishRisk, y = Percent, fill = FishRisk)) +
  geom_col(color = "black", alpha = 0.8) +
  geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("High Risk" = "#D73027",
                               "Moderate Risk" = "#FC8D59",
                               "Safe" = "#1A9850")) +
  labs(
    title = "Fish Survival Risk Distribution",
    subtitle = "Percentage of samples per risk category after cleaning DO values",
    x = "Fish Risk Category",
    y = "Percent of Samples"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

high_risk_locations <- water_sample %>%
  filter(FishRisk == "High Risk") %>%
  select(SiteID, Latitude, Longitude, DO_mgL)
