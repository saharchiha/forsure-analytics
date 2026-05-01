# ------------------------ Librairies ------------------------ #
library(keras)
library(tensorflow)
library(caret)
library(dplyr)
library(recipes)

# ------------------------ Chargement & Prétraitement ------------------------ #

data <- readRDS("data/Dataset.rds")
data <- data %>% select(-ClaimAmount_log, -ClaimAmountMoyen, -TargetFrequency, -DriverAge_Class, -CarAge_Class)

# Nettoyage
data$ClaimNb[is.na(data$ClaimNb) | data$ClaimNb < 0] <- 0

X <- data %>% select(-ClaimNb, -Exposure)
y <- data$ClaimNb
exposure <- data$Exposure

# Encodage
dummy <- dummyVars(" ~ .", data = X)
X_encoded <- predict(dummy, newdata = X)
X_encoded <- cbind(X_encoded, log_exposure = log(exposure))

# Normalisation
preproc <- preProcess(X_encoded, method = c("center", "scale"))
X_scaled <- predict(preproc, X_encoded)

# Split
set.seed(0)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_scaled[train_index, ]
X_test <- X_scaled[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]
exposure_test <- exposure[-train_index]

# Example: Training the model
set.seed(123)

# Create the model
create_tweedie_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(64, activation = "relu", input_shape = ncol(X_train)) %>%
    layer_dropout(0.3) %>%
    layer_dense(32, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(1, activation = "softplus")  # Ensure positive predictions
  
  model %>% compile(
    loss = loss_poisson,
    optimizer = optimizer_adam(learning_rate = 0.0005),
    metrics = list("mean_absolute_error")
  )
  return(model)
}

# Train the model
model <- create_tweedie_model()

history <- model %>% fit(
  X_train,
  y_train,
  epochs = 100,
  batch_size = 32,
  validation_data = list(X_test, y_test),
  callbacks = list(
    callback_early_stopping(patience = 10),
    callback_reduce_lr_on_plateau(factor = 0.1, patience = 5)
  ),
  verbose = 1
)

# Evaluate the model
evaluation <- model %>% evaluate(X_test, y_test, verbose = 0)
cat("Validation Loss:", evaluation["loss"], "\n")
cat("Validation MAE:", evaluation["mean_absolute_error"], "\n")

# Predictions
y_pred <- model %>% predict(X_test)

# Adjust predictions for exposure (if applicable)
y_pred_adjusted <- y_pred * exposure_test

# Plot training history
plot(history)

calculate_gini <- function(actual, predicted) {
  # Order predictions and actual values by predicted values (descending)
  ord <- order(predicted, decreasing = TRUE)
  actual_sorted <- actual[ord]
  
  # Cumulative sum of actual values
  cum_actual <- cumsum(actual_sorted)
  cum_actual_pct <- cum_actual / sum(actual_sorted)  # Normalize to percentages
  
  # Perfect ordering (ideal model)
  perfect_ord <- order(actual, decreasing = TRUE)
  perfect_actual <- actual[perfect_ord]
  perfect_cum <- cumsum(perfect_actual)
  perfect_cum_pct <- perfect_cum / sum(perfect_actual)
  
  # Random ordering (baseline model)
  n <- length(actual)
  random_cum_pct <- (1:n) / n
  
  # Calculate areas under the curves
  model_area <- sum(cum_actual_pct) / n
  random_area <- 0.5
  max_area <- sum(perfect_cum_pct) / n
  
  # Gini index formula
  gini <- (model_area - random_area) / (max_area - random_area)
  return(gini)
}
gini_index <- calculate_gini(y_test, y_pred_adjusted)
cat("Gini Index:", round(gini_index, 4), "\n")

# 6) PREDICTIONS & VISUALIZATION
results <- data.frame(
  Actual = y_test,
  Predicted = y_pred_adjusted
)

# Plot actual vs predicted
plot<-ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Claim Frequency",
       x = "Actual Frequency",
       y = "Predicted Frequency") +
  theme_minimal()
print(plot)
save_model_hdf5(model, "C:/at/app/ann_model.h5")
saveRDS(history, "C:/at/app/ann_history.rds")
saveRDS(gini_index, "C:/at/app/gini_index.rds")
saveRDS(X, "C:/at/app/X_train.rds")
saveRDS(y, "C:/at/app/X_test.rds")




