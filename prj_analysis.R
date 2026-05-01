# ---- Setup ----
knitr::opts_chunk$set(echo = TRUE)
# ---- Import the 1st Dataset ----
library(CASdatasets)
data("freMTPLfreq", package = "CASdatasets")
View(freMTPLfreq)

# Function to check if a variable has unique values
has_unique_values <- function(data, column_name) {
  unique_count <- length(unique(data[[column_name]]))
  total_count <- nrow(data)
  unique_count == total_count
}
column_to_check <- "PolicyID"
if (has_unique_values(freMTPLfreq, column_to_check)) {
  cat("The variable", column_to_check, "has all unique values.\n")
} else {
  cat("The variable", column_to_check, "does NOT have all unique values.\n")
}

# ---- Import the 2nd Dataset ----
data("freMTPLsev", package = "CASdatasets")
View(freMTPLsev)
if (has_unique_values(freMTPLsev, column_to_check)) {
  cat("The variable", column_to_check, "has all unique values.\n")
} else {
  cat("The variable", column_to_check, "does NOT have all unique values.\n")
}

library(dplyr)
freMTPLsev <- freMTPLsev %>%
  group_by(PolicyID) %>%
  summarise(ClaimAmount = sum(ClaimAmount))
print(freMTPLsev)

# ---- Data merging ----
freMTPLfreq$PolicyID = as.integer(freMTPLfreq$PolicyID)
Dataset <- freMTPLfreq %>% left_join(freMTPLsev, by = "PolicyID")
print(Dataset)

# ---- Exploring the final Database ----
head(Dataset)
glimpse(Dataset)

# ---- Data cleaning: Missing values ----
is.na(Dataset)
colSums(is.na(Dataset))
complete.cases(Dataset)

# Check if all instances where claimNB is 0 also have claimAmount as 0
check_claim <- all(is.na(Dataset$claimAmount[Dataset$claimNB == 0]))
if (check_claim) {
  cat("All instances where claimNb is 0 also have claimAmount as NA.\n")
} else {
  cat("There are instances where claimNb is 0 but claimAmount is NOT NA.\n")
}

Dataset <- Dataset %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
print("Data after replacing NA with 0:")
print(Dataset)
summary(Dataset)

# ---- Duplicates ----
has_duplicates <- any(duplicated(Dataset))
if (has_duplicates) {
  cat("The dataset contains duplicate rows.\n")
} else {
  cat("The dataset does NOT contain duplicate rows.\n")
}
print(Dataset)

# ---- Outliers ----
library(tidyr)
library(tidyselect)
library(ggplot2)
Dataset <- Dataset %>% dplyr::select(-PolicyID)
numeric_columns <- Dataset %>% dplyr::select(where(is.numeric))
factor_columns <- Dataset %>% dplyr::select(where(is.factor) | where(is.character))

if (ncol(numeric_columns) > 0) {
  data_numeric_long <- numeric_columns %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  data_numeric_long <- data_numeric_long %>%
    filter(!variable %in% c("ClaimAmount", "ClaimNb"))
  ggplot(data_numeric_long, aes(x = value)) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = "Histograms of Numeric Variables",
         x = "Value",
         y = "Frequency") +
    theme_minimal()
} else {
  cat("No numeric columns found.\n")
}
ggplot(numeric_columns['ClaimAmount'], aes(x = ClaimAmount + 1)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  scale_x_log10() +
  labs(title = "Distribution of Claim Amounts (Log Scale)",
       x = "Claim Amount (log scale)") +
  theme_minimal()
ggplot(numeric_columns['ClaimNb'], aes(x = factor(ClaimNb))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Number of Claims",
       x = "Number of Claims",
       y = "Frequency") +
  theme_minimal() +
  scale_x_discrete(limits = function(x) as.character(sort(as.numeric(unique(x)))))
if (ncol(factor_columns) > 0) {
  data_factor_long <- factor_columns %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  ggplot(data_factor_long, aes(x = value)) +
    geom_bar(fill = "pink", color = "black") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = "Bar Plots of Factor Variables",
         x = "Value",
         y = "Frequency") +
    theme_minimal()
} else {
  cat("No factor columns found.\n")
}
print(Dataset)

# ---- Normality Testing ----
test_normality_ks <- function(data) {
  results <- data %>%
    summarise(across(everything(), ~ ks.test(., "pnorm", mean = mean(.), sd = sd(.))$p.value))
  return(results)
}
normality_results <- test_normality_ks(numeric_columns)
normally_distributed <- normality_results %>% dplyr::select(where(~ all(. > 0.05)))
non_normally_distributed <- normality_results %>% dplyr::select(where(~ all(. <= 0.05)))
cat("Normally Distributed Variables:\n")
print(names(normally_distributed))
cat("\nNon-Normally Distributed Variables:\n")
print(names(non_normally_distributed))
for (col in names(numeric_columns)) {
  density_plot <- ggplot(Dataset, aes(x = .data[[col]])) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Density Plot of", col),
         x = col,
         y = "Density") +
    theme_minimal()
  qq_plot <- ggplot(Dataset, aes(sample = .data[[col]])) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(title = paste("Q-Q Plot of", col),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  print(density_plot)
  print(qq_plot)
}

# ---- Outlier Detection (IQR) ----
check_outliers_iqr <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  any(outliers, na.rm = TRUE)
}
numeric_columns_names <- names(Dataset)[sapply(Dataset, is.numeric)]
outlier_summary <- sapply(numeric_columns_names, function(col) check_outliers_iqr(Dataset, col))
print(outlier_summary)
for (col in numeric_columns_names) {
  boxplot <- ggplot(Dataset, aes(x = .data[[col]])) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Boxplot of", col),
         x = col,
         y = "Value") +
    theme_minimal()
  print(boxplot)
}
print(Dataset)

# ---- Winsorization and Transformation ----
library(MASS)
library(e1071)
lower_cap <- quantile(Dataset$ClaimNb, 0.01, na.rm = TRUE)
upper_cap <- quantile(Dataset$ClaimNb, 0.99, na.rm = TRUE)
Dataset$ClaimNb <- pmin(pmax(Dataset$ClaimNb, lower_cap), upper_cap)
lower_cap <- quantile(Dataset$CarAge, 0.01, na.rm = TRUE)
upper_cap <- quantile(Dataset$CarAge, 0.99, na.rm = TRUE)
Dataset$CarAge <- pmin(pmax(Dataset$CarAge, lower_cap), upper_cap)
Dataset$ClaimAmount_log <- log(Dataset$ClaimAmount + 1)
cap_value <- quantile(Dataset$DriverAge, 0.95, na.rm = TRUE)
Dataset$DriverAge[Dataset$DriverAge > cap_value] <- cap_value
skew_value <- skewness(Dataset$Density, na.rm = TRUE)
if (skew_value > 1) {
  lower_cap <- quantile(Dataset$Density, 0.01, na.rm = TRUE)
  upper_cap <- quantile(Dataset$Density, 0.90, na.rm = TRUE)
} else if (skew_value < -1) {
  lower_cap <- quantile(Dataset$Density, 0.05, na.rm = TRUE)
  upper_cap <- quantile(Dataset$Density, 0.99, na.rm = TRUE)
} else {
  lower_cap <- quantile(Dataset$Density, 0.05, na.rm = TRUE)
  upper_cap <- quantile(Dataset$Density, 0.95, na.rm = TRUE)
}
Dataset$Density <- pmin(pmax(Dataset$Density, lower_cap), upper_cap)
par(mfrow = c(1, 2))
hist(Dataset$ClaimAmount, main = "Original ClaimAmount")
hist(Dataset$ClaimAmount_log, main = "Transformed ClaimAmount")
summary(Dataset)

# ---- Univariate Analysis ----
qualitative_vars <- names(Dataset)[sapply(Dataset, function(x) is.character(x) | is.factor(x))]
for (var in qualitative_vars) {
  p <- ggplot(Dataset, aes(x = Dataset[[var]])) +
    geom_bar(fill = "steelblue") +
    labs(
      title = paste("Barplot of", var),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}
numeric_cols <- sapply(Dataset, is.numeric)
numeric_vars <- Dataset[, numeric_cols, drop = FALSE]
summary(numeric_vars)
for (col in colnames(numeric_vars)) {
  hist(numeric_vars[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "steelblue")
}
for (col in colnames(numeric_vars)) {
  boxplot(numeric_vars[[col]],
          main = paste("Boxplot of", col),
          col = "orange")
}

# ---- Bivariate Analysis ----
library(corrplot)
cor_matrix <- cor(Dataset[sapply(Dataset, is.numeric)], method = "pearson")
cor_matrix


# ---- Discretization of CarAge and DriverAge ----
Dataset <- Dataset %>%
  mutate(
    DriverAge_Class = cut(
      DriverAge,
      breaks = c(0, 25, 40, 60, Inf),
      labels = c("Young", "Adult", "Senior", "Elderly"),
      right = FALSE
    ),
    CarAge_Class = cut(
      CarAge,
      breaks = c(0, 5, 10, 15, Inf),
      labels = c("New", "Young", "Middle-aged", "Old"),
      right = FALSE
    )
  )
print(Dataset)

# ---- Average claim amount for each Brand and DriverAge ----
average_claim <- Dataset %>%
  group_by(Brand, DriverAge) %>%
  summarise(
    AverageClaimAmount = mean(ClaimAmount_log, na.rm = TRUE)
  ) %>%
  ungroup()
print(average_claim)

# ---- Plots for DriverAge and CarAge classes ----
ggplot(Dataset, aes(x = DriverAge_Class)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Driver Age Classes", x = "Driver Age Class", y = "Frequency") +
  theme_minimal()
ggplot(Dataset, aes(x = CarAge_Class)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Car Age Classes", x = "Car Age Class", y = "Frequency") +
  theme_minimal()

# ---- ANOVA for categorical variables (ClaimAmount_log) ----
categorical_vars <- c("Brand", "Region", "Power", "Gas", "CarAge_Class", "DriverAge_Class")
for (var in categorical_vars) {
  formula <- as.formula(paste("ClaimAmount_log ~", var))
  anova_result <- aov(formula, data = Dataset)
  cat("\n\n===== ANOVA for", var, "=====\n")
  print(summary(anova_result))
}

# ---- ANOVA for categorical variables (ClaimNb) ----
for (var in categorical_vars) {
  formula <- as.formula(paste("ClaimNb ~", var))
  anova_result <- aov(formula, data = Dataset)
  cat("\n\n===== ANOVA for", var, "=====\n")
  print(summary(anova_result))
}

# ---- Chi-square and Cramér's V for categorical variables ----
library(vcd)
categorical_cols <- c("Power", "Brand", "Region", "Gas")
for (var in categorical_cols) {
  Dataset[[var]] <- as.character(Dataset[[var]])
  Dataset[[var]] <- as.factor(Dataset[[var]])
}
for (i in 1:(length(categorical_cols) - 1)) {
  for (j in (i + 1):length(categorical_cols)) {
    table_contingence <- table(Dataset[[categorical_cols[i]]], Dataset[[categorical_cols[j]]])
    if (all(table_contingence > 0)) {
      test_chi2 <- chisq.test(table_contingence)
      cat("Chi-square test for", categorical_cols[i], "and", categorical_cols[j], "\n")
      print(test_chi2)
      cat("\n")
    } else {
      cat("Skipping Chi-square test for", categorical_cols[i], "and", categorical_cols[j],
          "due to zero counts in contingency table.\n\n")
    }
    v_value <- assocstats(table_contingence)$cramer
    cat("Cramér's V for", categorical_cols[i], "and", categorical_cols[j], ":", round(v_value, 3), "\n")
  }
}

# ---- Distribution fitting for ClaimAmount ----
library(fitdistrplus)
Dataset$ClaimAmount[Dataset$ClaimAmount <= 0] <- 1e-6
fit_gamma <- fitdist(Dataset$ClaimAmount, "gamma")
fit_lnorm <- fitdist(Dataset$ClaimAmount, "lnorm")
fit_weibull <- fitdist(Dataset$ClaimAmount, "weibull")

# ---- Kolmogorov-Smirnov test ----
library(MASS)
clean_claims <- Dataset$ClaimAmount[Dataset$ClaimAmount > 0]
ks_gamma <- ks.test(clean_claims, "pgamma",
                    shape = fit_gamma$estimate["shape"],
                    rate = fit_gamma$estimate["rate"])
ks_lnorm <- ks.test(clean_claims, "plnorm",
                    meanlog = fit_lnorm$estimate["meanlog"],
                    sdlog = fit_lnorm$estimate["sdlog"])
ks_weibull <- ks.test(clean_claims, "pweibull",
                      shape = fit_weibull$estimate["shape"],
                      scale = fit_weibull$estimate["scale"])
cat("Kolmogorov-Smirnov test for Gamma distribution:\n")
print(ks_gamma)
cat("\nKolmogorov-Smirnov test for Lognormal distribution:\n")
print(ks_lnorm)
cat("\nKolmogorov-Smirnov test for Weibull distribution:\n")
print(ks_weibull)
ks_stats <- c(Gamma = ks_gamma$statistic,
              Lognormal = ks_lnorm$statistic,
              Weibull = ks_weibull$statistic)
cat("\nK-S statistic values (lower is better):\n")
print(ks_stats)
cat("\nBest fitting distribution based on K-S statistic:",
    names(ks_stats)[which.min(ks_stats)], "\n")

# ---- Cramér-von Mises test ----
library(goftest)
cvm_gamma <- cvm.test(clean_claims, "pgamma",
                      shape = fit_gamma$estimate["shape"],
                      rate = fit_gamma$estimate["rate"])
cvm_lnorm <- cvm.test(clean_claims, "plnorm",
                      meanlog = fit_lnorm$estimate["meanlog"],
                      sdlog = fit_lnorm$estimate["sdlog"])
cvm_weibull <- cvm.test(clean_claims, "pweibull",
                        shape = fit_weibull$estimate["shape"],
                        scale = fit_weibull$estimate["scale"])
cat("Cramér-von Mises test for Gamma distribution:\n")
print(cvm_gamma)
cat("\nCramér-von Mises test for Lognormal distribution:\n")
print(cvm_lnorm)
cat("\nCramér-von Mises test for Weibull distribution:\n")
print(cvm_weibull)
cvm_stats <- c(Gamma = cvm_gamma$statistic,
               Lognormal = cvm_lnorm$statistic,
               Weibull = cvm_weibull$statistic)
cat("\nCramér-von Mises statistic values (lower is better):\n")
print(cvm_stats)
cat("\nBest fitting distribution based on Cramér-von Mises statistic:",
    names(cvm_stats)[which.min(cvm_stats)], "\n")

# ---- Anderson-Darling Test ----
library(ADGofTest)
pgamma_fitted <- function(x) {
  pgamma(x, shape = fit_gamma$estimate["shape"],
         rate = fit_gamma$estimate["rate"])
}
plnorm_fitted <- function(x) {
  plnorm(x, meanlog = fit_lnorm$estimate["meanlog"],
         sdlog = fit_lnorm$estimate["sdlog"])
}
pweibull_fitted <- function(x) {
  pweibull(x, shape = fit_weibull$estimate["shape"],
           scale = fit_weibull$estimate["scale"])
}
ad_gamma <- ad.test(clean_claims, pgamma_fitted)
ad_lnorm <- ad.test(clean_claims, plnorm_fitted)
ad_weibull <- ad.test(clean_claims, pweibull_fitted)
cat("Anderson-Darling test for Gamma distribution:\n")
print(ad_gamma)
cat("\nAnderson-Darling test for Lognormal distribution:\n")
print(ad_lnorm)
cat("\nAnderson-Darling test for Weibull distribution:\n")
print(ad_weibull)
ad_stats <- c(Gamma = ad_gamma$statistic,
              Lognormal = ad_lnorm$statistic,
              Weibull = ad_weibull$statistic)
cat("\nAnderson-Darling statistic values (lower is better):\n")
print(ad_stats)
cat("\nBest fitting distribution based on Anderson-Darling statistic:",
    names(ad_stats)[which.min(ad_stats)], "\n")

# ---- Poisson and Negative Binomial for ClaimNb ----
fit_poisson <- fitdist(Dataset$ClaimNb, "pois")
fit_nbinom <- fitdist(Dataset$ClaimNb, "nbinom")
lambda <- fit_poisson$estimate["lambda"]
size <- fit_nbinom$estimate["size"]
mu <- fit_nbinom$estimate["mu"]
observed <- table(Dataset$ClaimNb)
calculate_expected <- function(observed, dist, ...) {
  values <- as.numeric(names(observed))
  probs <- dist(values, ...)
  expected <- probs * length(Dataset$ClaimNb)
  return(expected)
}
expected_poisson <- calculate_expected(observed, dpois, lambda = lambda)
expected_nbinom <- calculate_expected(observed, dnbinom, size = size, mu = mu)
chisq_poisson <- chisq.test(observed, p = expected_poisson / sum(expected_poisson))
chisq_nbinom <- chisq.test(observed, p = expected_nbinom / sum(expected_nbinom))
resultats <- data.frame(
  test = c("Poisson", "Negative binomial"),
  Chi2_statistique = c(chisq_poisson$statistic, chisq_nbinom$statistic),
  P_value = c(chisq_poisson$p.value, chisq_nbinom$p.value)
)
print(resultats)
comparison_table <- data.frame(
  Distribution = c("Poisson", "Negative Binomial"),
  AIC = c(fit_poisson$aic, fit_nbinom$aic),
  BIC = c(fit_poisson$bic, fit_nbinom$bic)
)
print(comparison_table)
best_distribution <- comparison_table[which.min(comparison_table$AIC), "Distribution"]
cat("\nBest Distribution by AIC:", best_distribution, "\n")
best_distribution_bic <- comparison_table[which.min(comparison_table$BIC), "Distribution"]
cat("Best Distribution by BIC:", best_distribution_bic, "\n")

# ---- GLM: Average cost model ----
Dataset$ClaimAmountMoyen <- ifelse(Dataset$ClaimNb != 0 & Dataset$ClaimAmount != 0, Dataset$ClaimAmount / Dataset$ClaimNb, 1e-6)
print(Dataset)
y<- Dataset$ClaimAmountMoyen
y_<-Dataset$ClaimNb
Dataset <- Dataset %>%
  dplyr::select(
    -ClaimAmountMoyen,
    -ClaimAmount_log,
    -ClaimAmount,
    -ClaimNb,
    -CarAge_Class,
    -DriverAge_Class
  )
# 1. Determine the number of rows for the training set (70%)
num_total_rows <- nrow(Dataset)
num_train_samples <- floor(0.7 * num_total_rows) # Ensures an integer

# 2. Create a single set of row indices for the training data
set.seed(123) # For reproducible splits. Change or remove if you want different splits each time.
train_index <- sample(1:num_total_rows, size = num_train_samples, replace = FALSE)

# --- Splitting 'Dataset' (features) ---
# 70% for training
train_data <- Dataset[train_index, ]
# Remaining 30% for testing
test_data <- Dataset[-train_index, ]

# --- Splitting 'y' (first target variable) ---
# 70% for training, corresponding to train_data
y_train <- y[train_index]
# Remaining 30% for testing, corresponding to test_data
y_test <- y[-train_index]

# --- Splitting 'y_' (second target variable or version, if it exists) ---
# Check if y_ exists before trying to split it
if (exists("y_") && !is.null(y_)) {
  # Ensure y_ has the same length as the original dataset rows
  if (length(y_) == num_total_rows) {
    # 70% for training, corresponding to train_data
    y__train <- y_[train_index] # Using y__train as in your previous example
    # Remaining 30% for testing, corresponding to test_data
    y__test <- y_[-train_index] # Using y__test
    print("y_ has been split.")
  } else {
    warning(
      "y_ exists but does not have the same number of elements as rows in Dataset. Skipping split for y_."
    )
  }
}


# ---- Fit Weibull GLM Model ----
library(survival)
pos_claims <- y_train[y_train > 0]
weibull_fit <- fitdist(pos_claims, "weibull")
print(weibull_fit)
train_data$Brand <- as.factor(train_data$Brand)
train_data$Region <- as.factor(train_data$Region)
train_data$Power <- as.factor(train_data$Power)
surv_obj <- Surv(time = y_train, event = rep(1, nrow(train_data)))
weibull_model <- survreg(
  surv_obj ~ Brand + Region + Power + DriverAge + offset(log(Exposure)),
  data = train_data,
  dist = "weibull"
)
summary(weibull_model)
coef_table <- summary(weibull_model)$table
print(coef_table)
predicted <- predict(weibull_model, newdata = test_data, type = "response")

# ---- Model Evaluation ----

actual <- y_test
aic <- AIC(weibull_model)
mae <- mean(abs(actual - predicted), na.rm = TRUE)
rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
mape <- mean(abs((actual - predicted)/actual), na.rm = TRUE) * 100
cat("\nModel Evaluation Metrics:\n")
cat("AIC:", aic, "\n")
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

# ---- Gini index for model evaluation ----
calculate_gini <- function(actual, predicted) {
  ord <- order(predicted, decreasing = TRUE)
  actual_sorted <- actual[ord]
  cum_actual <- cumsum(actual_sorted)
  cum_actual <- cum_actual / max(cum_actual)
  n <- length(actual)
  index <- 1:n / n
  auc <- sum((cum_actual[-1] + cum_actual[-n]) / 2 * diff(index))
  gini <- 2 * auc - 1
  return(gini)
}

gini_score <- calculate_gini(y_test, predicted)
print(paste("Gini coefficient:", round(gini_score, 4)))

# ---- Plot Lorenz curve ----
plot_lorenz_curve <- function(actual, predicted) {
  ord <- order(predicted, decreasing = TRUE)
  actual_sorted <- actual[ord]
  cum_actual <- cumsum(actual_sorted)
  cum_actual <- cum_actual / max(cum_actual)
  n <- length(actual)
  index <- 0:n / n
  plot(c(0, index[-1]), c(0, cum_actual),
       type = "l", col = "blue", lwd = 2,
       xlab = "Cumulative % of policies",
       ylab = "Cumulative % of claims",
       main = "Lorenz Curve")
  lines(c(0,1), c(0,1), col = "red", lty = 2, lwd = 2)
  legend("topleft", legend = c("Model", "Random"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  gini <- 2 * (sum((cum_actual[-1] + cum_actual[-n]) / 2 * diff(index[-1]))) - 1
  text(0.7, 0.3, paste("Gini =", round(gini, 4)), cex = 1.2)
}
plot_lorenz_curve(y_test,predicted)

# ---- Lognormal GLM Model ----
# 1. Fit Lognormal distribution to ClaimAmount
pos_claims <- y_train[y_train > 0]
lnorm_fit <- fitdist(pos_claims, "lnorm")
print(lnorm_fit)

# 2. Convert categorical variables to factors if not already
train_data$Brand <- as.factor(train_data$Brand)
train_data$Region <- as.factor(train_data$Region)
train_data$Power <- as.factor(train_data$Power)

# 3. Prepare for lognormal GLM
surv_obj <- Surv(time = y_train, event = rep(1, nrow(train_data)))

# 4. Fit the lognormal GLM model with offset
lognormal_model <- survreg(
  surv_obj ~ Brand + Region + Power + DriverAge + offset(log(Exposure)),
  data = train_data,
  dist = "lognormal"
)

# 5. Summarize the model
summary(lognormal_model)
coef_table <- summary(lognormal_model)$table
print(coef_table)

# 6. Predict on test data
predicted <- predict(lognormal_model, newdata = test_data, type = "response")

# ---- Lognormal Model Evaluation ----
actual <- y_test
aic <- AIC(lognormal_model)
mae <- mean(abs(actual - predicted), na.rm = TRUE)
rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
mape <- mean(abs((actual - predicted)/actual), na.rm = TRUE) * 100
cat("\nLognormal Model Evaluation Metrics:\n")
cat("AIC:", aic, "\n")
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

# ---- Gini index for lognormal model evaluation ----
calculate_gini <- function(actual, predicted) {
  ord <- order(predicted, decreasing = TRUE)
  actual_sorted <- actual[ord]
  cum_actual <- cumsum(actual_sorted)
  cum_actual <- cum_actual / max(cum_actual)
  n <- length(actual)
  index <- 1:n / n
  auc <- sum((cum_actual[-1] + cum_actual[-n]) / 2 * diff(index))
  gini <- 2 * auc - 1
  return(gini)
}
actual_severity <- y_test
predicted_severity <-predicted
lognormal_gini <- calculate_gini(actual_severity, predicted_severity)
print(paste("Lognormal model Gini coefficient:", round(lognormal_gini, 4)))

# ---- Plot Lorenz curve for lognormal model ----
plot_lorenz_curve <- function(actual, predicted) {
  ord <- order(predicted, decreasing = TRUE)
  actual_sorted <- actual[ord]
  cum_actual <- cumsum(actual_sorted)
  cum_actual <- cum_actual / max(cum_actual)
  n <- length(actual)
  index <- 0:n / n
  plot(c(0, index[-1]), c(0, cum_actual),
       type = "l", col = "blue", lwd = 2,
       xlab = "Cumulative % of policies",
       ylab = "Cumulative % of claim amount",
       main = "Lorenz Curve - Lognormal Model")
  lines(c(0,1), c(0,1), col = "red", lty = 2, lwd = 2)
  legend("topleft", legend = c("Lognormal Model", "Random"),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  gini <- 2 * (sum((cum_actual[-1] + cum_actual[-n]) / 2 * diff(index[-1]))) - 1
  text(0.7, 0.3, paste("Gini =", round(gini, 4)), cex = 1.2)
}
plot_lorenz_curve(actual_severity,predicted_severity)



# ---- Stepwise selection using AIC (lognormal model) ----
library(MASS)
full_model <- survreg(
  Surv(time = y_train, event = rep(1, nrow(train_data))) ~
    Brand + Region + Power + Gas + DriverAge + Density + CarAge + Density + offset(log(Exposure)),
  data = train_data,
  dist = "lognormal"
)
stepwise_model <- stepAIC(full_model, direction = "backward")
summary(stepwise_model)

# ---- Residuals analysis and performance metrics ----
residuals <- residuals(stepwise_model, type = "deviance")
fitted_values <- predict(stepwise_model, type = "response")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(fitted_values, residuals, main = "Residuals vs. Fitted",
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, lty = 2)
qqnorm(residuals)
qqline(residuals)
hist(residuals, breaks = 30, main = "Histogram of Residuals")
test_predictions <- predict(stepwise_model, newdata = test_data, type = "response")
MAE <- mean(abs(y_test - test_predictions))
RMSE <- sqrt(mean((y_test - test_predictions)^2))
cat("Mean Absolute Error:", MAE, "\n")
cat("Root Mean Squared Error:", RMSE, "\n")
cat("AIC of stepwise model:", AIC(stepwise_model), "\n")
cat("AIC of previous lognormal model:", AIC(lognormal_model), "\n")
plot(y_test, test_predictions,
     main = "Actual vs. Predicted ClaimAmount Average",
     xlab = "Actual ClaimAmount Average", ylab = "Predicted ClaimAmountAverage")
abline(0, 1, col = "red", lty = 2)

# ---- Gini Index for stepwise model ----
library(pROC)
tpred <- predict(stepwise_model, newdata = train_data, type = "response")
tepred <- predict(stepwise_model, newdata = test_data, type = "response")
auc_train <- roc(y_train, tpred)$auc
auc_test <- roc(y_test, tepred)$auc
gini_train <- 2 * auc_train - 1
gini_test <- 2 * auc_test - 1
cat("Gini Index (Train):", gini_train, "\n")
cat("Gini Index (Test):", gini_test, "\n")
train_data=cbind(train_data,y_)
# ---- Poisson model for frequency ----
glm_poisson <- glm(
  y__train ~ DriverAge + Brand + Region + Power,
  data = train_data,
  family = poisson(link = "log"),
  offset = log(Exposure)
)
print(train_data)
summary(glm_poisson)
predicted <- predict(glm_poisson, newdata = test_data, type = "response")

# ---- Gini Index for Poisson model ----
pred <- predict(glm_poisson, newdata = train_data, type = "response")
tpred <- predict(glm_poisson, newdata = test_data, type = "response")
auc_train <- roc(y__train,pred)$auc
auc_test <- roc(y__test, tpred)$auc
ginii_train <- 2 * auc_train - 1
ginii_test <- 2 * auc_test - 1
cat("Gini Index (Train):", gini_train, "\n")
cat("Gini Index (Test):", gini_test, "\n")

# ---- Negative Binomial model ----
library(MASS)
glm_nb <- glm.nb(
  y__train~ DriverAge + Brand + Region + Power + offset(log(Exposure)),
  data = train_data
)
summary(glm_nb)
predicted <- predict(glm_nb, newdata = test_data, type = "response")

# ---- Gini Index for Negative Binomial model ----
pred <- predict(glm_nb, newdata = train_data, type = "response")
tpred <- predict(glm_nb, newdata = test_data, type = "response")
auc_train <- roc(y__train, pred)$auc
auc_test <- roc(y__test, tpred)$auc
gini_trainn <- 2 * auc_train - 1
gini_testt <- 2 * auc_test - 1
cat("Gini Index (Train):", gini_train, "\n")
cat("Gini Index (Test):", gini_test, "\n")

# ---- Stepwise selection for Poisson model ----
full_model <- glm(
  y__train ~ Brand + Region + Power + Gas + DriverAge + Density + CarAge + Density + offset(log(Exposure)),
  data = train_data,
  family = poisson(link = "log")
)
step_model <- stepAIC(
  full_model,
  direction = "backward"
)
summary(step_model)

# ---- Gini Index for stepwise Poisson model ----
pred <- predict(step_model, newdata = train_data, type = "response")
tpred <- predict(step_model, newdata = test_data, type = "response")
auc_train <- roc(y__train, pred)$auc
auc_test <- roc(y__test, tpred)$auc
gini_ttrain <- 2 * auc_train - 1
gini_ttest <- 2 * auc_test - 1
cat("Gini Index (Train):", gini_train, "\n")
cat("Gini Index (Test):", gini_test, "\n")

saveRDS(numeric_cols, "C:/at/app/numeric_vars.rds")
saveRDS(qualitative_vars, "C:/at/app/qualitative_vars.rds")
saveRDS(cor_matrix, "C:/at/app/cor.rds")
saveRDS(weibull_model, "C:/at/app/weibull.rds")
saveRDS(gini_score, "C:/at/app/gini_scores.rds")
saveRDS(lognormal_model, "C:/at/app/lognormal_model.rds")
saveRDS(lognormal_gini, "C:/at/app/lognormal_gini.rds")
saveRDS(stepwise_model, "C:/at/app/stepwise_models.rds")
saveRDS(gini_train, "C:/at/app/gini_train.rds")
saveRDS(gini_test, "C:/at/app/gini_test.rds")
saveRDS(glm_poisson, "C:/at/app/glm_poisson.rds")
saveRDS(ginii_train, "C:/at/app/ginii_train.rds")
saveRDS(ginii_test, "C:/at/app/ginii_test.rds")
saveRDS(glm_nb, "C:/at/app/glm_nb.rds")
saveRDS(gini_trainn, "C:/at/app/gini_trainn.rds")
saveRDS(gini_testt, "C:/at/app/gini_testt.rds")
saveRDS(step_model, "C:/at/app/step_model.rds")
saveRDS(gini_ttrain, "C:/at/app/gini_ttrain.rds")
saveRDS(gini_ttest, "C:/at/app/gini_ttest.rds")
saveRDS(step_model, "C:/at/app/ginii_train.rds")


