# ============================================================================
# Water Quality Lab Data Analysis - Mass.gov
# Fully fixed version with plots guaranteed to display
# ============================================================================

# Clear environment
rm(list = ls())

# ============================================================================
# Load packages
# ============================================================================
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "randomForest", "caret",
              "corrplot", "rpart", "rpart.plot", "gridExtra", "reshape2", "scales")

for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# Load Data
# ============================================================================
file_path <- "D:/R Projects/Water Quality/labdatamain-8-23-2022.xlsx"
water_data <- read_excel(file_path)
original_count <- nrow(water_data)

# ============================================================================
# Clean Data (Mass.gov Rules)
# ============================================================================
# Rule 1: Keep only QC4 and QC5
if("QCStatus" %in% names(water_data)){
  water_data <- water_data %>% filter(QCStatus %in% c("QC4", "QC5"))
}

# Rule 2: Remove censored data
censored_columns <- c("ResVal", "SECCHI", "MAXDEPTH")
for(col in censored_columns){
  if(col %in% names(water_data)){
    water_data <- water_data %>%
      filter(is.na(.data[[col]]) | !grepl("##", as.character(.data[[col]]), fixed = TRUE))
  }
}

# Rule 3: Handle missing data symbols
water_data <- water_data %>%
  mutate(across(where(is.character), ~na_if(., "--"))) %>%
  mutate(across(where(is.character), ~na_if(., "**"))) %>%
  mutate(across(where(is.character), ~na_if(., "^^"))) %>%
  mutate(across(where(is.character), ~na_if(., "")))

# Rule 4: Remove quality flags
if("DWMQual" %in% names(water_data)){
  water_data <- water_data %>%
    filter(is.na(DWMQual) | !grepl("[im]", as.character(DWMQual)))
}

# Create numeric dataset
numeric_cols <- c("Datayear", "nMILEPT", "Latitude", "Longitude",
                  "nMAXDEPTH", "SampDepth", "RelDepth", "nSECCHI", "nResult")

available_cols <- numeric_cols[numeric_cols %in% names(water_data)]

water_numeric <- water_data %>%
  select(all_of(available_cols)) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  filter(if_any(everything(), ~ !is.na(.))) %>%
  select(where(~sum(!is.na(.)) > 0))

# ============================================================================
# GRAPH 1: Data Cleaning Summary
# ============================================================================
cleaning_summary <- data.frame(
  Stage = c("Original\nData", "After QC\nFilter", "After Quality\nFlags", "Final Clean\nData"),
  Count = c(original_count, nrow(water_data), nrow(water_data), nrow(water_numeric))
)

p1 <- ggplot(cleaning_summary, aes(x = factor(Stage, levels = Stage), y = Count, fill = Stage)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = scales::comma(Count)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Water Quality Data Cleaning Journey",
       subtitle = "Following Mass.gov Rules",
       x = "Cleaning Stage", y = "Number of Samples") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1)))

print(p1)

# ============================================================================
# GRAPH 2: Data Distributions
# ============================================================================
plot_list <- list()
for(i in 1:min(6, ncol(water_numeric))){
  col_name <- names(water_numeric)[i]
  p <- ggplot(water_numeric, aes_string(x = col_name)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
    labs(title = col_name, x = col_name, y = "Count") +
    theme_minimal(base_size = 10)
  plot_list[[i]] <- p
}

p2 <- grid.arrange(grobs = plot_list, ncol = 3, 
                   top = "Distribution of Water Quality Measurements")
print(p2)

# ============================================================================
# GRAPH 3: Correlation Heatmap
# ============================================================================
if(ncol(water_numeric) >= 2){
  cor_matrix <- cor(water_numeric, use = "pairwise.complete.obs")
  cor_melted <- melt(cor_matrix)
  
  p3 <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_fill_gradient2(low = "#3B9AB2", mid = "white", high = "#F21A00",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Correlation") +
    labs(title = "Correlation Between Measurements", x = "", y = "") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank())
  
  print(p3)
}

# ============================================================================
# GRAPH 4: Missing Data Pattern
# ============================================================================
missing_data <- water_numeric %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Percent = (Missing_Count / nrow(water_numeric)) * 100)

p4 <- ggplot(missing_data, aes(x = reorder(Variable, -Missing_Percent),
                               y = Missing_Percent, fill = Missing_Percent)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(Missing_Percent, 1), "%")), vjust = -0.5, size = 4) +
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(title = "Missing Data Pattern",
       subtitle = "Percentage of missing values by measurement",
       x = "Measurement", y = "% Missing") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p4)

# ============================================================================
# Prepare for Machine Learning
# ============================================================================
target_variable <- intersect(c("nMAXDEPTH", "nSECCHI", "nResult"), names(water_numeric))[1]
ml_data <- water_numeric %>%
  filter(!is.na(.data[[target_variable]])) %>%
  drop_na()

set.seed(123)
train_index <- createDataPartition(ml_data[[target_variable]], p = 0.7, list = FALSE)
train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]
predictors <- setdiff(names(train_data), target_variable)
tree_formula <- as.formula(paste(target_variable, "~", paste(predictors, collapse = " + ")))

# ============================================================================
# MODEL 1: Decision Tree
# ============================================================================
tree_model <- rpart(tree_formula, data = train_data, method = "anova",
                    control = rpart.control(minsplit = 10, cp = 0.01))

rpart.plot(tree_model, main = paste("Decision Tree: Predicting", target_variable),
           box.palette = "Blues", shadow.col = "gray", nn = TRUE, cex = 0.8)

tree_predictions <- predict(tree_model, test_data)
tree_results <- data.frame(Actual = test_data[[target_variable]], Predicted = tree_predictions)
tree_rmse <- sqrt(mean((tree_results$Actual - tree_results$Predicted)^2, na.rm = TRUE))
tree_r2 <- cor(tree_results$Actual, tree_results$Predicted, use = "complete.obs")^2

p5 <- ggplot(tree_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#2E86AB", alpha = 0.6, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#A23B72", linetype = "dashed", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#F18F01", linetype = "dotted") +
  labs(title = "Decision Tree: Real vs Predicted",
       subtitle = paste0("Accuracy: ", round(tree_r2*100,1), "% | RMSE: ", round(tree_rmse,2)),
       x = paste("Real", target_variable), y = paste("Predicted", target_variable)) +
  theme_minimal(base_size = 14)
print(p5)

# ============================================================================
# MODEL 2: Random Forest
# ============================================================================
set.seed(123)
forest_model <- randomForest(tree_formula, data = train_data, ntree = 100, importance = TRUE, na.action = na.omit)
forest_predictions <- predict(forest_model, test_data)
forest_results <- data.frame(Actual = test_data[[target_variable]], Predicted = forest_predictions)
forest_rmse <- sqrt(mean((forest_results$Actual - forest_results$Predicted)^2, na.rm = TRUE))
forest_r2 <- cor(forest_results$Actual, forest_results$Predicted, use = "complete.obs")^2

importance_data <- as.data.frame(importance(forest_model))
importance_data$Variable <- rownames(importance_data)
importance_data <- importance_data %>% arrange(desc(`%IncMSE`))

p6 <- ggplot(importance_data, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "#2D6E4F", color = "black", alpha = 0.8) +
  coord_flip() +
  geom_text(aes(label = round(`%IncMSE`,1)), hjust = -0.2, size = 4, fontface = "bold") +
  labs(title = "Variable Importance - Random Forest",
       x = "Variable", y = "Importance Score") +
  theme_minimal(base_size = 14)
print(p6)

p7 <- ggplot(forest_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#2D6E4F", alpha = 0.6, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#A23B72", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "#F18F01", linetype = "dotted") +
  labs(title = "Random Forest: Real vs Predicted",
       subtitle = paste0("Accuracy: ", round(forest_r2*100,1), "% | RMSE: ", round(forest_rmse,2)),
       x = paste("Real", target_variable), y = paste("Predicted", target_variable)) +
  theme_minimal(base_size = 14)
print(p7)

# ============================================================================
# MODEL 3: Linear Regression
# ============================================================================
linear_model <- lm(tree_formula, data = train_data)
linear_predictions <- predict(linear_model, test_data)
linear_results <- data.frame(Actual = test_data[[target_variable]], Predicted = linear_predictions)
linear_rmse <- sqrt(mean((linear_results$Actual - linear_results$Predicted)^2, na.rm = TRUE))
linear_r2 <- summary(linear_model)$r.squared

p8 <- ggplot(linear_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#7209B7", alpha = 0.6, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#A23B72", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "#F18F01", linetype = "dotted") +
  labs(title = "Linear Regression: Real vs Predicted",
       subtitle = paste0("R²: ", round(linear_r2*100,1), "% | RMSE: ", round(linear_rmse,2)),
       x = paste("Real", target_variable), y = paste("Predicted", target_variable)) +
  theme_minimal(base_size = 14)
print(p8)

# ============================================================================
# Model Comparison
# ============================================================================
results_comparison <- data.frame(
  Model = c("Decision Tree", "Random Forest", "Linear Regression"),
  RMSE = c(tree_rmse, forest_rmse, linear_rmse),
  R_Squared = c(tree_r2, forest_r2, linear_r2)
) %>% arrange(RMSE) %>% mutate(Rank = row_number())
results_comparison$Color <- c("#FFD700", "#C0C0C0", "#CD7F32")[results_comparison$Rank]

p9 <- ggplot(results_comparison, aes(x = reorder(Model, -RMSE), y = RMSE, fill = Color)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(RMSE,2)), vjust=-0.5, size=5) +
  scale_fill_identity() +
  labs(title="Average Error (Lower = Better)", x="Model", y="RMSE") +
  theme_minimal(base_size = 14)
p10 <- ggplot(results_comparison, aes(x = reorder(Model, R_Squared), y = R_Squared*100, fill=Color)) +
  geom_bar(stat="identity", color="black") +
  geom_text(aes(label=paste0(round(R_Squared*100,1),"%")), vjust=-0.5, size=5) +
  scale_fill_identity() +
  labs(title="Accuracy Score (Higher = Better)", x="Model", y="R² (%)") +
  theme_minimal(base_size=14)
grid.arrange(p9, p10, ncol=2, top="Model Performance Comparison")

# ============================================================================
# Residual Analysis
# ============================================================================
residuals_df <- data.frame(Predicted = forest_predictions,
                           Residuals = test_data[[target_variable]] - forest_predictions)
p11 <- ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point(color="#2D6E4F", alpha=0.6, size=3) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_smooth(method="loess", se=TRUE, color="#F18F01", fill="#F18F01", alpha=0.2) +
  labs(title="Prediction Errors Analysis (Residuals)",
       subtitle="Random Forest Model", x=paste("Predicted", target_variable),
       y="Error (Actual - Predicted)") +
  theme_minimal(base_size=14)
print(p11)

# ============================================================================
# Summary
# ============================================================================
summary(water_numeric)
print(results_comparison)
