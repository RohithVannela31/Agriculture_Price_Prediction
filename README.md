# Agricultural Commodity Price Prediction in India Using R

**Student:** Rohith Vannela  
**Roll Number:** 061

## 1. Project Overview

This project analyses historical agricultural commodity prices in India and predicts monthly minimum, maximum, and modal prices for a selected state, commodity, variety, and future year using R.

## 2. Objectives

- Analyse historical agricultural market prices.
- Prepare and aggregate price data into monthly observations.
- Visualise historical price trends.
- Compare Baseline, Moving Average, and SARIMA forecasting models.
- Evaluate models using MAE, RMSE, and MAPE.
- Generate monthly price predictions in rupees per quintal and rupees per kilogram.

## 3. Technologies Used

- R
- RStudio
- R Markdown
- dplyr
- ggplot2
- lubridate
- forecast
- zoo
- tidyr

## 4. Project Files

- `agricultural_price_prediction.R` — R code for data preparation and prediction.
- `Agriculture_commodity_price_predictions.R` — additional R prediction code, if used in the final implementation.
- `061_Agricultural_Price_Prediction.Rmd` — R Markdown report source.
- `061_Agricultural_Price_Prediction.html` — rendered report.
- `Agricultural_Commodity_Price_Analysis.pptx` — project presentation.
- `agricultural_price_predictions.csv` — prediction results.
- `model_evaluation_results.csv` — model evaluation results.
- `Agriculture_Price_Prediction.Rproj` — RStudio project file.

## 5. Dataset

The project uses historical agricultural market price data containing state, commodity, variety, arrival date, and minimum, maximum, and modal prices.

**Original dataset source URL:**1. Kaggle. *Daily Commodity Prices India* (AGMARKNET data).
   <https://www.kaggle.com/datasets/khandelwalmanas/daily-commodity-prices-india>.

## 6. Forecasting Models

1. **Baseline:** Uses the last observed training value as the forecast.
2. **Moving Average:** Uses the previous three observations to forecast prices.
3. **SARIMA:** Models time-series trends and potential seasonal patterns.

The models are evaluated using Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and Mean Absolute Percentage Error (MAPE).

## 7. How to Run

1. Install R and RStudio.
2. Open `Agriculture_Price_Prediction.Rproj`.
3. Install the required R packages if they are not already installed.
4. Ensure the required dataset files are available at the paths specified in the code.
5. Open and run the relevant R script or knit the R Markdown report.

## 8. Limitations

Predictions depend on historical data quality, missing-value handling, and the stability of historical price patterns. Forecasts are estimates and are not guaranteed future market prices.

## 9. Conclusion

This project demonstrates exploratory data analysis and time-series forecasting of Indian agricultural commodity prices using R. It compares three forecasting approaches and generates monthly price estimates to support analysis of historical and potential future price trends.
