
# ============================================================
# AGRICULTURAL COMMODITY PRICE PREDICTION IN INDIA USING R
# Models: Baseline, Moving Average, SARIMA
# Outputs: Minimum, Maximum and Modal Prices
# Units: Rupees per Quintal and Rupees per Kg
# ============================================================


# STEP 1: INSTALL AND LOAD PACKAGES

packages <- c("readr", "dplyr", "ggplot2",
              "lubridate", "forecast", "zoo", "tidyr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(zoo)
library(tidyr)


# STEP 2: DATASET LOCATION

data_folder <- "C:/v sem/IDS/csv"

if (!dir.exists(data_folder)) {
  stop("Check the dataset folder path.")
}

years_to_use <- 2015:2025

for (yr in years_to_use) {
  if (!file.exists(file.path(data_folder, paste0(yr, ".csv")))) {
    stop(paste("Missing file:", yr, ".csv"))
  }
}


# STEP 3: READ AND PREPARE DATA YEAR BY YEAR

monthly_data <- data.frame()

for (yr in years_to_use) {
  
  cat("\nProcessing year:", yr, "\n")
  
  file_path <- file.path(data_folder, paste0(yr, ".csv"))
  
  raw_data <- read_csv(
    file_path,
    col_types = cols(
      State = col_character(),
      District = col_character(),
      Market = col_character(),
      Commodity = col_character(),
      Variety = col_character(),
      Arrival_Date = col_character(),
      Min_Price = col_double(),
      Max_Price = col_double(),
      Modal_Price = col_double(),
      .default = col_guess()
    ),
    show_col_types = FALSE
  )
  
  needed <- c("State", "Commodity", "Variety", "Arrival_Date",
              "Min_Price", "Max_Price", "Modal_Price")
  
  if (!all(needed %in% names(raw_data))) {
    stop(paste("Required columns missing in", yr, ".csv"))
  }
  
  raw_data$Arrival_Date <- as.Date(
    parse_date_time(
      raw_data$Arrival_Date,
      orders = c("ymd", "dmy", "mdy"),
      quiet = TRUE
    )
  )
  
  clean_data <- raw_data %>%
    filter(
      !is.na(State), !is.na(Commodity), !is.na(Variety),
      !is.na(Arrival_Date),
      is.finite(Min_Price), is.finite(Max_Price),
      is.finite(Modal_Price),
      Min_Price > 0, Max_Price > 0, Modal_Price > 0
    ) %>%
    mutate(
      State = trimws(State),
      Commodity = trimws(Commodity),
      Variety = trimws(Variety),
      Month = floor_date(Arrival_Date, unit = "month")
    ) %>%
    group_by(State, Commodity, Variety, Month) %>%
    summarise(
      Min_Price = mean(Min_Price, na.rm = TRUE),
      Max_Price = mean(Max_Price, na.rm = TRUE),
      Modal_Price = mean(Modal_Price, na.rm = TRUE),
      .groups = "drop"
    )
  
  monthly_data <- bind_rows(monthly_data, clean_data)
  
  rm(raw_data, clean_data)
  gc()
  
  cat("Finished:", yr, "\n")
}


# STEP 4: CHECK THE PRICE UNIT

# This conversion assumes source prices are Rs per quintal.
# Verify the original dataset documentation before reporting results.

monthly_data <- monthly_data %>%
  mutate(
    Min_Price_Kg = Min_Price / 100,
    Max_Price_Kg = Max_Price / 100,
    Modal_Price_Kg = Modal_Price / 100
  )

cat("\nAvailable states:\n")
print(sort(unique(monthly_data$State)))

cat("\nAvailable commodities:\n")
print(sort(unique(monthly_data$Commodity)))


# STEP 5: USER SELECTION

selected_state <- "Telangana"
selected_commodity <- "Tomato"
selected_variety <- "Local"
prediction_year <- 2026

# Check that the selection exists in the dataset
available_varieties <- monthly_data %>%
  filter(
    State == selected_state,
    Commodity == selected_commodity
  ) %>%
  distinct(Variety)

if (nrow(available_varieties) == 0) {
  stop("State or commodity not found in the dataset.")
}

if (!selected_variety %in% available_varieties$Variety) {
  stop("Variety not found. Choose a variety from available_varieties.")
}

if (is.na(prediction_year) || prediction_year <= 2025) {
  stop("Enter a year later than 2025.")
}

cat("State:", selected_state, "\n")
cat("Commodity:", selected_commodity, "\n")
cat("Variety:", selected_variety, "\n")
cat("Prediction year:", prediction_year, "\n")




# STEP 6: FILTER THE SELECTED DATA

selected_data <- monthly_data %>%
  filter(
    tolower(State) == tolower(trimws(selected_state)),
    tolower(Commodity) == tolower(trimws(selected_commodity)),
    tolower(Variety) == tolower(trimws(selected_variety))
  ) %>%
  group_by(Month) %>%
  summarise(
    Min_Price = mean(Min_Price, na.rm = TRUE),
    Max_Price = mean(Max_Price, na.rm = TRUE),
    Modal_Price = mean(Modal_Price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Month)

if (nrow(selected_data) == 0) {
  stop("No matching records. Check the spelling of your selections.")
}


# STEP 7: CREATE A CONTINUOUS MONTHLY SERIES

selected_data <- selected_data %>%
  complete(
    Month = seq(min(Month), max(Month), by = "month")
  ) %>%
  arrange(Month)

price_names <- c("Min_Price", "Max_Price", "Modal_Price")

for (nm in price_names) {
  selected_data[[nm]] <- zoo::na.approx(
    selected_data[[nm]], na.rm = FALSE
  )
  selected_data[[nm]] <- zoo::na.locf(
    selected_data[[nm]], na.rm = FALSE
  )
  selected_data[[nm]] <- zoo::na.locf(
    selected_data[[nm]], fromLast = TRUE, na.rm = FALSE
  )
}

if (nrow(selected_data) < 36) {
  stop("At least 36 monthly observations are required for this comparison.")
}


# STEP 8: EVALUATION METRICS

calculate_metrics <- function(actual, predicted) {
  
  errors <- actual - predicted
  
  mae <- mean(abs(errors))
  rmse <- sqrt(mean(errors^2))
  
  # MAPE is calculated only for nonzero actual prices.
  valid <- actual != 0 & is.finite(actual) & is.finite(predicted)
  
  mape <- if (any(valid)) {
    mean(abs(errors[valid] / actual[valid])) * 100
  } else {
    NA_real_
  }
  
  data.frame(MAE = mae, RMSE = rmse, MAPE = mape)
}


# STEP 9: BASELINE MODEL

# Predict each future month using the last training observation.

baseline_forecast <- function(train, h) {
  rep(tail(train, 1), h)
}


# STEP 10: MOVING AVERAGE MODEL

# Use the previous 3 observations.
# For multi-step forecasts, each prediction is added
# to the rolling window for the next prediction.

moving_average_forecast <- function(train, h, window = 3) {
  
  history <- as.numeric(train)
  predictions <- numeric(h)
  
  for (i in seq_len(h)) {
    predictions[i] <- mean(tail(history, window))
    history <- c(history, predictions[i])
  }
  
  predictions
}


# STEP 11: SARIMA MODEL

# Seasonal ARIMA with a 12-month seasonal period.
# Fixed order: ARIMA(1,1,1)(1,1,1)[12]

sarima_forecast <- function(train, h) {
  
  model <- forecast::Arima(
    ts(as.numeric(train), frequency = 12),
    order = c(1, 1, 1),
    seasonal = list(
      order = c(1, 1, 1),
      period = 12
    )
  )
  
  as.numeric(forecast::forecast(model, h = h)$mean)
}


# STEP 12: EVALUATE ALL THREE MODELS

# Keep the final 12 observed months aside for testing.
n <- nrow(selected_data)

train_data <- selected_data[1:(n - 12), ]
test_data <- selected_data[(n - 11):n, ]

evaluate_models <- function(price_column) {
  
  train_values <- train_data[[price_column]]
  actual_values <- test_data[[price_column]]
  
  pred_baseline <- baseline_forecast(train_values, 12)
  pred_ma <- moving_average_forecast(train_values, 12, 3)
  
  pred_sarima <- tryCatch(
    sarima_forecast(train_values, 12),
    error = function(e) {
      warning(paste("SARIMA failed:", e$message))
      rep(NA_real_, 12)
    }
  )
  
  bind_rows(
    cbind(
      Model = "Baseline",
      calculate_metrics(actual_values, pred_baseline)
    ),
    cbind(
      Model = "Moving Average (3 months)",
      calculate_metrics(actual_values, pred_ma)
    ),
    cbind(
      Model = "SARIMA",
      calculate_metrics(actual_values, pred_sarima)
    )
  )
}

evaluation_results <- bind_rows(
  cbind(
    Price_Type = "Minimum",
    evaluate_models("Min_Price")
  ),
  cbind(
    Price_Type = "Maximum",
    evaluate_models("Max_Price")
  ),
  cbind(
    Price_Type = "Modal",
    evaluate_models("Modal_Price")
  )
)

cat("\nMODEL EVALUATION RESULTS\n")
print(evaluation_results)


# STEP 13: FORECAST THE CHOSEN YEAR

# Fit each model using all available historical observations.

last_month <- max(selected_data$Month)

forecast_end <- as.Date(
  paste0(prediction_year, "-12-01")
)

h <- (year(forecast_end) - year(last_month)) * 12 +
  month(forecast_end) - month(last_month)

if (h <= 0) {
  stop("The selected year must be after the last historical month.")
}

forecast_price <- function(values, model_name, horizon) {
  
  if (model_name == "Baseline") {
    return(baseline_forecast(values, horizon))
  }
  
  if (model_name == "Moving Average") {
    return(moving_average_forecast(values, horizon, 3))
  }
  
  if (model_name == "SARIMA") {
    return(sarima_forecast(values, horizon))
  }
}

# Select the model with the lowest MAE for each price type.
# If SARIMA failed during evaluation, its MAE will be NA.

best_model <- function(price_type) {
  result <- evaluation_results %>%
    filter(Price_Type == price_type, is.finite(MAE)) %>%
    arrange(MAE)
  
  if (nrow(result) == 0) {
    stop(paste("No valid model evaluation for", price_type))
  }
  
  result$Model[1]
}

best_min <- best_model("Minimum")
best_max <- best_model("Maximum")
best_modal <- best_model("Modal")

cat("\nSelected models based on validation MAE:\n")
cat("Minimum price:", best_min, "\n")
cat("Maximum price:", best_max, "\n")
cat("Modal price:", best_modal, "\n")

future_months <- seq(
  from = last_month %m+% months(1),
  by = "month",
  length.out = h
)

all_predictions <- data.frame(
  Month = future_months,
  Predicted_Min_Price = forecast_price(
    selected_data$Min_Price, best_min, h
  ),
  Predicted_Max_Price = forecast_price(
    selected_data$Max_Price, best_max, h
  ),
  Predicted_Modal_Price = forecast_price(
    selected_data$Modal_Price, best_modal, h
  )
)

prediction_results <- all_predictions %>%
  filter(year(Month) == prediction_year) %>%
  mutate(
    Min_Price_Kg = Predicted_Min_Price / 100,
    Max_Price_Kg = Predicted_Max_Price / 100,
    Modal_Price_Kg = Predicted_Modal_Price / 100
  )

cat("\nFINAL PREDICTIONS\n")
cat("State:", selected_state, "\n")
cat("Commodity:", selected_commodity, "\n")
cat("Variety:", selected_variety, "\n")
cat("Year:", prediction_year, "\n")
print(prediction_results)


# STEP 14: PLOT THE HISTORICAL MODAL PRICE

print(
  ggplot(selected_data, aes(Month, Modal_Price)) +
    geom_line() +
    labs(
      title = "Historical Modal Price",
      subtitle = paste(selected_state, selected_commodity, selected_variety),
      x = "Month",
      y = "Price (Rs per quintal)"
    ) +
    theme_minimal()
)


# STEP 15: PLOT THE PREDICTED MODAL PRICE

print(
  ggplot(prediction_results, aes(Month, Predicted_Modal_Price)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Predicted Monthly Modal Price",
      subtitle = paste("Prediction year:", prediction_year),
      x = "Month",
      y = "Predicted Price (Rs per quintal)"
    ) +
    theme_minimal()
)


# STEP 16: SAVE RESULTS

write.csv(
  evaluation_results,
  "model_evaluation_results.csv",
  row.names = FALSE
)

write.csv(
  prediction_results,
  "agricultural_price_predictions.csv",
  row.names = FALSE
)

cat("\nFiles saved in your current R working directory.\n")

saveRDS(
  monthly_data,
  "C:/v sem/IDS/monthly_data.rds"
)

file.exists("C:/v sem/IDS/monthly_data.rds")
