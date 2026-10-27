    # ============================================================
# AGRICULTURAL COMMODITY PRICE PREDICTION DASHBOARD (Rs/kg)
# Models: Baseline, Moving Average, SARIMA
# Historical analysis: summary statistics, heatmap, yearly averages
# ============================================================

packages <- c("shiny", "dplyr", "ggplot2", "forecast",
              "lubridate", "tidyr", "zoo")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(shiny)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(tidyr)

# ------------------------------------------------------------
# Load prepared data (source prices are Rs per quintal)
# ------------------------------------------------------------

data_file <- "C:/v sem/IDS/monthly_data.rds"

if (!file.exists(data_file)) {
  stop("Dataset file not found. Run saveRDS() on your monthly data first.")
}

monthly_data <- readRDS(data_file)

required_columns <- c("State", "Commodity", "Variety", "Month",
                      "Min_Price", "Max_Price", "Modal_Price")

if (!all(required_columns %in% names(monthly_data))) {
  stop("The saved dataset is missing required columns.")
}

monthly_data <- monthly_data %>%
  mutate(
    Month       = as.Date(Month),
    Min_Price   = Min_Price / 100,     # Rs/quintal -> Rs/kg
    Max_Price   = Max_Price / 100,
    Modal_Price = Modal_Price / 100
  )

# ------------------------------------------------------------
# Helper: build a continuous monthly series for one price column
# ------------------------------------------------------------

prepare_series <- function(data, price_column) {
  
  series_data <- data %>%
    group_by(Month) %>%
    summarise(Price = mean(.data[[price_column]], na.rm = TRUE),
              .groups = "drop") %>%
    filter(is.finite(Price), Price > 0) %>%
    arrange(Month)
  
  if (nrow(series_data) == 0) return(series_data)
  
  series_data <- series_data %>%
    complete(Month = seq(min(Month), max(Month), by = "month")) %>%
    arrange(Month)
  
  series_data$Price <- zoo::na.approx(series_data$Price, na.rm = FALSE)
  series_data$Price <- zoo::na.locf(series_data$Price, na.rm = FALSE)
  series_data$Price <- zoo::na.locf(series_data$Price,
                                    fromLast = TRUE, na.rm = FALSE)
  series_data
}

# ------------------------------------------------------------
# SARIMA helper: seasonal differencing forced, log-transformed
# ------------------------------------------------------------

fit_sarima <- function(values, start_month) {
  
  series <- ts(values,
               start = c(year(start_month), month(start_month)),
               frequency = 12)
  
  fit <- tryCatch(
    forecast::auto.arima(series, seasonal = TRUE, D = 1, lambda = 0),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    fit <- tryCatch(
      forecast::auto.arima(series, seasonal = TRUE, lambda = 0),
      error = function(e) NULL
    )
  }
  fit
}

# ------------------------------------------------------------
# Forecasting function
# ------------------------------------------------------------

forecast_series <- function(data, price_column, prediction_year) {
  
  series_data <- prepare_series(data, price_column)
  
  if (nrow(series_data) < 36) {
    stop("At least 36 months of valid data are required.")
  }
  
  values      <- series_data$Price
  start_month <- min(series_data$Month)
  last_month  <- max(series_data$Month)
  
  horizon <- (prediction_year - year(last_month)) * 12 +
    (12 - month(last_month))
  
  if (horizon < 1) {
    stop("Choose a forecast year after the last observed month.")
  }
  
  # Hold out the last 12 months to compare models
  test_n <- 12
  train  <- head(values, length(values) - test_n)
  test   <- tail(values, test_n)
  
  baseline_test <- rep(tail(train, 1), test_n)
  
  ma_test    <- numeric(test_n)
  ma_history <- train
  for (i in seq_len(test_n)) {
    ma_test[i] <- mean(tail(ma_history, 3))
    ma_history <- c(ma_history, ma_test[i])
  }
  
  sarima_fit <- fit_sarima(train, start_month)
  
  if (is.null(sarima_fit)) {
    sarima_test <- rep(NA_real_, test_n)
  } else {
    sarima_test <- as.numeric(forecast(sarima_fit, h = test_n)$mean)
  }
  
  metric_row <- function(actual, predicted, model_name) {
    error <- actual - predicted
    ok    <- actual != 0 & is.finite(actual) & is.finite(predicted)
    data.frame(
      Model = model_name,
      MAE   = mean(abs(error)),
      RMSE  = sqrt(mean(error^2)),
      MAPE  = if (any(ok)) mean(abs(error[ok] / actual[ok])) * 100 else NA_real_
    )
  }
  
  metrics <- bind_rows(
    metric_row(test, baseline_test, "Baseline"),
    metric_row(test, ma_test,       "Moving Average"),
    metric_row(test, sarima_test,   "SARIMA")
  )
  
  valid_metrics <- metrics[is.finite(metrics$RMSE), ]
  best_model    <- valid_metrics$Model[which.min(valid_metrics$RMSE)]
  
  holdout <- data.frame(
    Month            = tail(series_data$Month, test_n),
    Actual           = test,
    Baseline         = baseline_test,
    `Moving Average` = ma_test,
    SARIMA           = sarima_test,
    check.names      = FALSE
  )
  
  # Refit chosen model on all data and forecast
  if (best_model == "Baseline") {
    
    predictions <- rep(tail(values, 1), horizon)
    
  } else if (best_model == "Moving Average") {
    
    history     <- values
    predictions <- numeric(horizon)
    for (i in seq_len(horizon)) {
      predictions[i] <- mean(tail(history, 3))
      history <- c(history, predictions[i])
    }
    
  } else {
    
    final_fit <- fit_sarima(values, start_month)
    
    if (is.null(final_fit)) {
      predictions <- rep(tail(values, 1), horizon)
      best_model  <- "Baseline (SARIMA failed)"
    } else {
      predictions <- as.numeric(forecast(final_fit, h = horizon)$mean)
    }
  }
  
  future_months <- seq(from = last_month %m+% months(1),
                       by = "month", length.out = horizon)
  
  forecast_all <- data.frame(
    Month = future_months,
    Predicted_Price = pmax(predictions, 0.01)
  )
  
  forecast_year <- forecast_all %>%
    filter(year(Month) == prediction_year)
  
  list(
    forecast   = forecast_year,
    metrics    = metrics,
    best_model = best_model,
    history    = series_data,
    holdout    = holdout
  )
}

# ------------------------------------------------------------
# User interface
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Agricultural Commodity Price Prediction in India (Rs/kg)"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      selectInput("state", "Select State",
                  choices = sort(unique(monthly_data$State))),
      selectInput("commodity", "Select Commodity", choices = NULL),
      selectInput("variety", "Select Variety", choices = NULL),
      selectInput("year", "Prediction Year",
                  choices = 2026:2030, selected = 2026),
      actionButton("predict", "Predict Prices", class = "btn-primary"),
      br(), br(),
      helpText("The Historical Analysis tab updates as soon as you choose a
                State, Commodity and Variety. Press 'Predict Prices' for the
                Forecast and Model Evaluation tabs. Accuracy drops the
                further the year is from the last observed data.")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        # ---------------- TAB 1: HISTORICAL ANALYSIS ----------------
        tabPanel(
          "Historical Analysis",
          br(),
          h4("1. Summary Statistics"),
          textOutput("data_period"),
          br(),
          tableOutput("summary_table"),
          
          h4("2. Heatmap: Modal Price by Month and Year"),
          plotOutput("heatmap_plot", height = "400px"),
          
          h4("3. Average Modal Price by Year"),
          plotOutput("yearly_plot", height = "320px")
        ),
        
        # ---------------- TAB 2: FORECAST ----------------
        tabPanel(
          "Forecast",
          br(),
          h3("Selected Model"),
          textOutput("best_model"),
          
          h3("Price Highlights"),
          textOutput("peak_text"),
          textOutput("low_text"),
          textOutput("flat_warning"),
          
          h3("Predicted Monthly Prices (Rs/kg)"),
          tableOutput("forecast_table"),
          
          h3("Predicted Modal Price by Month"),
          plotOutput("forecast_plot"),
          
          h3("Historical Modal Price and Forecast"),
          plotOutput("history_plot")
        ),
        
        # ---------------- TAB 3: MODEL EVALUATION ----------------
        tabPanel(
          "Model Evaluation",
          br(),
          h3("Model Performance Comparison (errors in Rs/kg)"),
          tableOutput("metrics_table"),
          
          h3("Holdout Test: Actual vs Predicted (last 12 months, Modal Price)"),
          plotOutput("holdout_plot", height = "380px")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ---- dropdown updates ----
  observeEvent(input$state, {
    commodities <- monthly_data %>%
      filter(State == input$state) %>%
      pull(Commodity) %>% unique() %>% sort()
    
    updateSelectInput(
      session, "commodity",
      choices  = commodities,
      selected = if (length(commodities)) commodities[1] else character(0)
    )
  })
  
  observeEvent(list(input$state, input$commodity), {
    req(input$state, input$commodity)
    
    varieties <- monthly_data %>%
      filter(State == input$state, Commodity == input$commodity) %>%
      pull(Variety) %>% unique() %>% sort()
    
    updateSelectInput(
      session, "variety",
      choices  = varieties,
      selected = if (length(varieties)) varieties[1] else character(0)
    )
  })
  
  # ============================================================
  # HISTORICAL ANALYSIS (does not need the Predict button)
  # ============================================================
  
  eda_data <- reactive({
    req(input$state, input$commodity, input$variety)
    
    selected <- monthly_data %>%
      filter(State == input$state,
             Commodity == input$commodity,
             Variety == input$variety)
    
    validate(need(nrow(selected) > 0, "No matching data found."))
    
    mi <- prepare_series(selected, "Min_Price")   %>% rename(Minimum = Price)
    mo <- prepare_series(selected, "Modal_Price") %>% rename(Modal   = Price)
    ma <- prepare_series(selected, "Max_Price")   %>% rename(Maximum = Price)
    
    df <- mi %>%
      inner_join(mo, by = "Month") %>%
      inner_join(ma, by = "Month") %>%
      arrange(Month)
    
    validate(need(nrow(df) >= 12,
                  "At least 12 months of data are needed for the analysis."))
    
    df$Year     <- year(df$Month)
    df$MonthNum <- month(df$Month)
    df$MonthLab <- factor(month.abb[df$MonthNum], levels = month.abb)
    df
  })
  
  output$data_period <- renderText({
    df <- eda_data()
    paste0("Data available: ", format(min(df$Month), "%b %Y"), " to ",
           format(max(df$Month), "%b %Y"), " (", nrow(df), " months)")
  })
  
  output$summary_table <- renderTable({
    df <- eda_data()
    f <- function(x) round(c(mean(x), median(x), sd(x), min(x), max(x),
                             sd(x) / mean(x) * 100), 2)
    data.frame(
      Statistic = c("Mean", "Median", "Std deviation", "Lowest",
                    "Highest", "Variation (CV %)"),
      `Minimum (Rs/kg)` = f(df$Minimum),
      `Modal (Rs/kg)`   = f(df$Modal),
      `Maximum (Rs/kg)` = f(df$Maximum),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)
  
  output$heatmap_plot <- renderPlot({
    df <- eda_data()
    ggplot(df, aes(x = MonthLab, y = factor(Year), fill = Modal)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = round(Modal, 0)), size = 3) +
      scale_fill_gradient(low = "#fff5eb", high = "#d94801") +
      scale_y_discrete(limits = rev) +
      labs(title = "Modal Price Heatmap (Rs/kg)",
           x = "Month", y = "Year", fill = "Rs/kg") +
      theme_minimal()
  })
  
  output$yearly_plot <- renderPlot({
    df <- eda_data()
    yearly <- df %>%
      group_by(Year) %>%
      summarise(Average = mean(Modal), .groups = "drop")
    
    ggplot(yearly, aes(x = factor(Year), y = Average)) +
      geom_col(fill = "#5b9bd5") +
      geom_text(aes(label = round(Average, 1)), vjust = -0.4) +
      labs(title = "Average Modal Price per Year",
           x = "Year", y = "Average Price (Rs/kg)") +
      theme_minimal()
  })
  
  # ============================================================
  # FORECAST (needs the Predict button)
  # ============================================================
  
  result <- eventReactive(input$predict, {
    
    req(input$state, input$commodity, input$variety, input$year)
    
    selected_data <- monthly_data %>%
      filter(State == input$state,
             Commodity == input$commodity,
             Variety == input$variety)
    
    validate(need(nrow(selected_data) > 0, "No matching data found."))
    
    prediction_year <- as.integer(input$year)
    
    validate(need(
      prediction_year > year(max(selected_data$Month)),
      "Select a year after the last available observation."
    ))
    
    withProgress(message = "Fitting models...", value = 0.3, {
      tryCatch({
        targets <- c("Min_Price", "Max_Price", "Modal_Price")
        results <- lapply(targets, function(target) {
          out <- forecast_series(selected_data, target, prediction_year)
          incProgress(0.3)
          out
        })
        names(results) <- targets
        results
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      })
    })
  }, ignoreInit = TRUE)
  
  forecast_combined <- reactive({
    req(result())
    
    min_d   <- result()[["Min_Price"]]$forecast
    max_d   <- result()[["Max_Price"]]$forecast
    modal_d <- result()[["Modal_Price"]]$forecast
    
    data.frame(
      Month   = modal_d$Month,
      Minimum = pmin(min_d$Predicted_Price, modal_d$Predicted_Price),
      Modal   = modal_d$Predicted_Price,
      Maximum = pmax(max_d$Predicted_Price, modal_d$Predicted_Price)
    )
  })
  
  output$best_model <- renderText({
    req(result())
    paste("Minimum:", result()[["Min_Price"]]$best_model,
          "| Maximum:", result()[["Max_Price"]]$best_model,
          "| Modal:",   result()[["Modal_Price"]]$best_model)
  })
  
  output$peak_text <- renderText({
    df   <- forecast_combined()
    peak <- df[which.max(df$Modal), ]
    paste0("Highest predicted modal price: ",
           format(peak$Month, "%B %Y"), " at Rs ",
           round(peak$Modal, 2), "/kg")
  })
  
  output$low_text <- renderText({
    df  <- forecast_combined()
    low <- df[which.min(df$Modal), ]
    paste0("Lowest predicted modal price: ",
           format(low$Month, "%B %Y"), " at Rs ",
           round(low$Modal, 2), "/kg")
  })
  
  output$flat_warning <- renderText({
    df <- forecast_combined()
    variation <- (max(df$Modal) - min(df$Modal)) / mean(df$Modal)
    if (variation < 0.02) {
      "Warning: the forecast is almost flat, so the chosen model found no clear monthly pattern. Treat the month-wise values with caution."
    } else {
      ""
    }
  })
  
  output$forecast_table <- renderTable({
    df <- forecast_combined()
    data.frame(
      Month = format(df$Month, "%b %Y"),
      `Minimum (Rs/kg)` = round(df$Minimum, 2),
      `Modal (Rs/kg)`   = round(df$Modal, 2),
      `Maximum (Rs/kg)` = round(df$Maximum, 2),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)
  
  output$forecast_plot <- renderPlot({
    df <- forecast_combined()
    df$Label  <- factor(format(df$Month, "%b"), levels = format(df$Month, "%b"))
    df$Status <- ifelse(df$Modal == max(df$Modal), "Highest",
                        ifelse(df$Modal == min(df$Modal), "Lowest", "Other"))
    
    ggplot(df, aes(x = Label, y = Modal, fill = Status)) +
      geom_col() +
      geom_text(aes(label = round(Modal, 1)), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = c(Highest = "#d9534f",
                                   Lowest  = "#5cb85c",
                                   Other   = "#5b9bd5")) +
      labs(title = paste("Predicted Modal Price by Month -", input$year),
           x = "Month", y = "Price (Rs/kg)", fill = NULL) +
      theme_minimal()
  })
  
  output$history_plot <- renderPlot({
    req(result())
    history <- result()[["Modal_Price"]]$history
    df      <- forecast_combined()
    
    ggplot() +
      geom_line(data = history, aes(Month, Price, colour = "Historical")) +
      geom_line(data = df, aes(Month, Modal, colour = "Forecast"),
                linewidth = 1) +
      geom_point(data = df, aes(Month, Modal, colour = "Forecast")) +
      scale_colour_manual(values = c(Historical = "grey30",
                                     Forecast   = "#d9534f")) +
      labs(title = "Historical Monthly Modal Price and Forecast",
           x = "Month", y = "Price (Rs/kg)", colour = NULL) +
      theme_minimal()
  })
  
  # ============================================================
  # MODEL EVALUATION
  # ============================================================
  
  output$metrics_table <- renderTable({
    req(result())
    
    bind_rows(lapply(names(result()), function(target) {
      m <- result()[[target]]$metrics
      m$Price_Type <- target
      m
    })) %>%
      select(Price_Type, Model, MAE, RMSE, MAPE) %>%
      mutate(across(c(MAE, RMSE, MAPE), ~ round(.x, 2)))
  }, striped = TRUE, bordered = TRUE)
  
  output$holdout_plot <- renderPlot({
    req(result())
    hold <- result()[["Modal_Price"]]$holdout
    
    long <- hold %>%
      pivot_longer(-Month, names_to = "Series", values_to = "Price")
    
    ggplot(long, aes(x = Month, y = Price, colour = Series,
                     linewidth = Series == "Actual")) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      scale_linewidth_manual(values = c(`TRUE` = 1.6, `FALSE` = 0.8),
                             guide = "none") +
      labs(title = "Last 12 Months: Actual vs Model Predictions (Modal Price)",
           x = "Month", y = "Price (Rs/kg)", colour = NULL) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)