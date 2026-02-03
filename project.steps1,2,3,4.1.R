# --------------------------------------------------------------
# 1. INSTALL & LOAD PACKAGES
# --------------------------------------------------------------
pkgs <- c("tidyverse", "skimr", "DataExplorer", "patchwork")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, dependencies = TRUE)
  if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
  if (!requireNamespace("plm", quietly = TRUE)) install.packages("plm")
  if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
  if (!requireNamespace("margins", quietly = TRUE)) install.packages("margins")
}
invisible(lapply(pkgs, install_if_missing))

library(tidyverse)
library(skimr)
library(DataExplorer)
library(patchwork)

# --------------------------------------------------------------
# 2. READ THE CSV
# --------------------------------------------------------------
# The first column is an unnamed index – we drop it and keep the header
df <- read_csv("marketing_AB.csv", col_names = TRUE, show_col_types = FALSE) %>%
  select(-`...1`)   # remove the unnamed first column

# Quick look
glimpse(df)

# --------------------------------------------------------------
# 3. PRE-CLEANING
# --------------------------------------------------------------

# 3.1 Missing values ------------------------------------------------
cat("\n--- Missing values ---\n")
df %>% summarise_all(~ sum(is.na(.))) %>% print()

# 3.2 Duplicates ----------------------------------------------------
cat("\n--- Duplicate rows ---\n")
anyDuplicated(df)   # 0 = no duplicates

cat("\n--- Unique user ids ---\n")
n_distinct(df$`user id`) == nrow(df)   # TRUE = all ids unique

# 3.3 Data types ----------------------------------------------------
df <- df %>%
  mutate(
    `test group` = factor(`test group`),
    converted    = as.logical(converted),
    `most ads day` = factor(`most ads day`,
                            levels = c("Monday","Tuesday","Wednesday",
                                       "Thursday","Friday","Saturday","Sunday")),
    `most ads hour` = as.integer(`most ads hour`)
  )

# 3.4 Outliers in total ads (IQR rule) -------------------------------
Q1 <- quantile(df$`total ads`, 0.25)
Q3 <- quantile(df$`total ads`, 0.75)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val

outliers <- df %>% filter(`total ads` > upper | `total ads` < lower)
cat(sprintf("\n--- Outliers in total ads (IQR) ---\n"))
cat(sprintf("Lower bound: %.1f  Upper bound: %.1f  Outliers: %d\n",
            lower, upper, nrow(outliers)))

# OPTIONAL: cap at 99th percentile (uncomment if you want to keep the data bounded)
# df <- df %>% mutate(`total ads` = pmin(`total ads`, quantile(`total ads`, 0.99)))

# 3.5 Summary statistics --------------------------------------------
skim(df)


# --------------------------------------------------------------
# 4. VISUALISATIONS (ggplot2)
# --------------------------------------------------------------

# Helper theme
my_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5))

# 4.1 Total ads – histogram + boxplot
p1 <- ggplot(df, aes(x = `total ads`)) +
  geom_histogram(bins = 60, fill = "#4C78A8", colour = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of Total Ads (log-scale)",
       x = "Total ads seen", y = "Count") +
  my_theme

p1b <- ggplot(df, aes(y = `total ads`)) +
  geom_boxplot(fill = "#F58518", colour = "black") +
  labs(title = "Boxplot of Total Ads", y = "Total ads") +
  my_theme

# 4.2 Most ads hour – bar chart
p2 <- ggplot(df, aes(x = `most ads hour`)) +
  geom_bar(fill = "#72B7A1") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Most Ads by Hour of Day", x = "Hour (0-23)", y = "Count") +
  my_theme

# 4.3 Test group – count bar
p3 <- ggplot(df, aes(x = `test group`, fill = `test group`)) +
  geom_bar() +
  scale_fill_manual(values = c(ad = "#E45756", psa = "#54A24B")) +
  labs(title = "Users per Test Group", x = "Test Group", y = "Count") +
  my_theme

# 4.4 Converted – count bar
p4 <- ggplot(df, aes(x = converted, fill = converted)) +
  geom_bar() +
  scale_fill_manual(values = c(`FALSE` = "#B2B2B2", `TRUE` = "#F28E2B")) +
  labs(title = "Conversion Count", x = "Converted", y = "Count") +
  my_theme

# 4.5 Most ads day – ordered bar
p5 <- df %>%
  count(`most ads day`) %>%
  ggplot(aes(x = `most ads day`, y = n)) +
  geom_col(fill = "#8CD17D") +
  labs(title = "Most Ads by Day of Week", x = "Day", y = "Count") +
  my_theme

# 4.6 Total ads by test group – boxplot
p6 <- ggplot(df, aes(x = `test group`, y = `total ads`, fill = `test group`)) +
  geom_boxplot() +
  scale_fill_manual(values = c(ad = "#E45756", psa = "#54A24B")) +
  labs(title = "Total Ads by Test Group", x = "Test Group", y = "Total ads") +
  my_theme

# 4.7 Conversion rate by test group (proportions)
conv_rate <- df %>%
  group_by(`test group`) %>%
  summarise(conversion_rate = mean(converted, na.rm = TRUE) * 100) %>%
  ungroup()

p7 <- ggplot(conv_rate, aes(x = `test group`, y = conversion_rate, fill = `test group`)) +
  geom_col() +
  scale_fill_manual(values = c(ad = "#E45756", psa = "#54A24B")) +
  labs(title = "Conversion Rate by Test Group",
       x = "Test Group", y = "Conversion Rate (%)") +
  geom_text(aes(label = sprintf("%.2f%%", conversion_rate)), vjust = -0.5) +
  my_theme

# --------------------------------------------------------------
# 5. ARRANGE ALL PLOTS
# --------------------------------------------------------------
layout <- "
AB
CD
EF
G#
"
(p1 + p1b) /
  (p2 + p3) /
  (p4 + p5) /
  (p6 + p7) +
  plot_annotation(title = "Marketing A/B Test – Pre-Cleaning & Visualisation",
                  theme = theme(plot.title = element_text(face = "bold", size = 16))) &
  plot_layout(design = layout)

# Save the combined figure (optional)
ggsave("marketing_AB_visualisations.png", width = 14, height = 16, dpi = 300)

# =====================================================================
# PART 2: DESCRIPTIVE ANALYSIS & HYPOTHESIS TESTING
# =====================================================================

library(dplyr)
library(ggplot2)
library(broom)

# --------------------------------------------------------------
# 1. RELABEL TEST GROUPS
# --------------------------------------------------------------
# "ad" = Control group (existing Ad campaign)
# "psa" = Test group (new PSA campaign)
df <- df %>%
  mutate(`test group` = ifelse(`test group` == "ad",
                               "Campaign A (Ad - Control)",
                               "Campaign B (PSA - Test)"))

# --------------------------------------------------------------
# 2. DESCRIPTIVE SUMMARY
# --------------------------------------------------------------
# Summarize total users, conversions, and conversion rate for each campaign
summary_tbl <- df %>%
  group_by(`test group`) %>%
  summarise(
    total_users = n(),
    conversions = sum(converted),
    conversion_rate = mean(converted)
  )
print(summary_tbl)

# --------------------------------------------------------------
# 3. VISUALIZATION: CONVERSION RATE BY CAMPAIGN
# --------------------------------------------------------------
ggplot(summary_tbl, aes(x=`test group`, y=conversion_rate, fill=`test group`)) +
  geom_col() +
  geom_text(aes(label=scales::percent(conversion_rate, accuracy=0.01)),
            vjust=-0.5, size=4) +
  scale_fill_manual(values=c("#E45756", "#54A24B")) +
  labs(title="Conversion Rate by Campaign",
       x="Campaign",
       y="Conversion Rate") +
  theme_minimal(base_size=13) +
  theme(legend.position="none")

# --------------------------------------------------------------
# 4. HYPOTHESIS TESTING: CHI-SQUARE TEST (TWO-SIDED)
# --------------------------------------------------------------
# H0: Conversion rate is equal for Ad and PSA
# H1: Conversion rate differs between the two campaigns
cont_tbl <- table(df$`test group`, df$converted)
print(cont_tbl)

chi_result <- chisq.test(cont_tbl)
tidy(chi_result)

# Interpretation:
# - If p < 0.05, reject H0 → significant difference between campaigns.
# - Here, p < 0.001 → significant difference.
# - Ad campaign has higher conversion rate (2.55%) than PSA (1.79%).

# =====================================================================
# PART 3: A/B TESTING FRAMEWORK & CAUSAL INFERENCE
# =====================================================================

# --------------------------------------------------------------
# 1. RANDOMIZATION CHECK
# --------------------------------------------------------------
# Compare average number of ads seen across groups to ensure fair assignment.
# If p > 0.05, randomization worked properly.
rand_check <- t.test(`total ads` ~ `test group`, data=df)
tidy(rand_check)

# --------------------------------------------------------------
# 2. AVERAGE TREATMENT EFFECT (ATE)
# --------------------------------------------------------------
# Difference in mean conversion rates between campaigns.
p1 <- summary_tbl$conversion_rate[summary_tbl$`test group` == "Campaign A (Ad - Control)"]
p2 <- summary_tbl$conversion_rate[summary_tbl$`test group` == "Campaign B (PSA - Test)"]
n1 <- summary_tbl$total_users[summary_tbl$`test group` == "Campaign A (Ad - Control)"]
n2 <- summary_tbl$total_users[summary_tbl$`test group` == "Campaign B (PSA - Test)"]

ATE <- p2 - p1
cat(sprintf("\nAverage Treatment Effect (ATE): %.4f (%.2f%% difference)\n",
            ATE, ATE * 100))

# --------------------------------------------------------------
# 3. CONFIDENCE INTERVAL FOR ATE (MANUAL CALCULATION)
# --------------------------------------------------------------
# Based on difference in proportions
SE <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
CI_low  <- ATE - 1.96 * SE
CI_high <- ATE + 1.96 * SE
cat(sprintf("95%% Confidence Interval for ATE: [%.4f, %.4f]\n", CI_low, CI_high))

# --------------------------------------------------------------
# 4. VISUALIZATION: CONVERSION RATE COMPARISON
# --------------------------------------------------------------
df_ci <- data.frame(
  Campaign = summary_tbl$`test group`,
  Rate = summary_tbl$conversion_rate
)

ggplot(df_ci, aes(x=Campaign, y=Rate, fill=Campaign)) +
  geom_col(width=0.6) +
  geom_errorbar(aes(ymin=Rate - (CI_high - CI_low)/2,
                    ymax=Rate + (CI_high - CI_low)/2),
                width=0.15, color="black") +
  scale_fill_manual(values=c("#E45756", "#54A24B")) +
  labs(title="Conversion Rate Comparison with 95% Confidence Interval",
       y="Conversion Rate", x="Campaign") +
  theme_minimal(base_size=13) +
  theme(legend.position="none")

# --------------------------------------------------------------
# 5. SUMMARY OF RESULTS
# --------------------------------------------------------------
# Chi-square: χ² = 54.01, p < 0.001 → reject H0
# Ad (Control) = 2.55% | PSA (Test) = 1.79%
# ATE = -0.0077 → PSA reduced conversions by ~0.77 percentage points
# 95% CI = [-0.0094, -0.0059] → significant negative difference
# Conclusion: The PSA campaign underperformed compared to Ad.
# --------------------------------------------------------------

# Step 4: Predictive Modelling – Logistic Regression

# Load necessary libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
library(patchwork)
library(caret)
library(margins)
library(pROC)

# Assume your original dataset is df
# Rename columns to valid R variable names (no spaces or special characters)
names(df) <- make.names(names(df))

# Convert 'converted' variable to factor with levels suitable for classification
if (is.logical(df$converted)) {
  df$converted <- factor(ifelse(df$converted, "Yes", "No"), levels = c("No", "Yes"))
} else if (is.factor(df$converted)) {
  levels(df$converted) <- make.names(levels(df$converted))
} else {
  stop("converted variable must be logical or factor")
}

# Split data into train/test for validation
set.seed(123)
train_index <- createDataPartition(df$converted, p = 0.7, list = FALSE)
train_df <- df[train_index, ]
test_df <- df[-train_index, ]

# Define logistic regression formula with interaction between test.group and device (if device exists)
if ("device" %in% names(df)) {
  train_df$device <- as.factor(train_df$device)
  test_df$device <- as.factor(test_df$device)
  formula_base <- converted ~ test.group + total.ads + device + test.group:device
} else {
  formula_base <- converted ~ test.group + total.ads
}

# Set up 5-fold cross-validation with ROC optimization
cv_ctrl <- trainControl(method = "cv",
                        number = 5,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "final")

# Train logistic regression model
log_model_cv <- train(formula_base,
                      data = train_df,
                      method = "glm",
                      family = binomial(link = "logit"),
                      trControl = cv_ctrl,
                      metric = "ROC")

# Print model summary
print(log_model_cv)
summary(log_model_cv$finalModel)

# Predictions on test set
test_pred_prob <- predict(log_model_cv, test_df, type = "prob")[, "Yes"]
test_pred_class <- predict(log_model_cv, test_df)
conf_mat <- confusionMatrix(test_pred_class, test_df$converted, positive = "Yes")
print(conf_mat)

# ROC curve and AUC on test data
roc_obj <- roc(test_df$converted, test_pred_prob, levels = c("No", "Yes"))
auc_val <- auc(roc_obj)
cat(sprintf("Test set AUC: %.3f\n", auc_val))
plot(roc_obj, main = "ROC Curve on Test Data")

# Marginal Effects using margins package on cleaned glm model explicitly fitted
glm_model_clean <- glm(formula_base,
                       data = train_df,
                       family = binomial(link = "logit"))

marginal_effects <- margins(glm_model_clean)
summary(marginal_effects)


# Visualization of predicted conversion probabilities by test.group
test_df$predicted_prob <- test_pred_prob
ggplot(test_df, aes(x = test.group, y = predicted_prob)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue") +
  labs(title = "Predicted Conversion Probabilities by Campaign Type",
       y = "Predicted Conversion Probability") +
  theme_minimal()

# Save model results for further reporting (odds ratios with CI)
library(broom)
model_odds <- broom::tidy(glm_model_clean, conf.int = TRUE, exponentiate = TRUE)
write.csv(model_odds, "enhanced_log_model_results.csv")
