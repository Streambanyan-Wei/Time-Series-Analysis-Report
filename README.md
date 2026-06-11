# Time Series Analysis Final Report

This repository contains the data processing, model fitting, diagnostic checking, forecasting, and report files for the Time Series Analysis Final Report. The project focuses on Singapore's annual Total Fertility Rate (TFR) and Total Live Births (TLB) from 1960 to 2025.

## Project Overview

Low fertility has become an important demographic issue in Singapore. This project investigates whether annual time series models can provide reliable short-term forecasts of Singapore's Total Fertility Rate and Total Live Births.

The main research question is:

**Can annual time series models provide reliable short-term forecasts of Singapore's Total Fertility Rate and Total Live Births, and what do these forecasts suggest about Singapore's future fertility trend?**

To answer this question, ARIMA and SARIMA models were fitted and compared using residual diagnostics and forecast accuracy measures.

## Data

The dataset used in this project is from the Singapore Department of Statistics, SingStat Table Builder:

**Births and Fertility Rates, Annual: Table M810091**

The two variables used in the analysis are:

* `TFR`: Total Fertility Rate
* `TLB`: Total Live Births

The full dataset covers the period from 1960 to 2025. The data were divided into:

* Training period: 1960 to 2012
* Testing period: 2013 to 2025

The training data were used to fit ARIMA and SARIMA models, while the testing data were used to evaluate forecast accuracy.

## Methods

The analysis included the following steps:

1. Data cleaning and preparation
2. Time series plotting
3. Stationarity checking using KPSS tests
4. First differencing and seasonal differencing
5. ACF and PACF analysis
6. ARIMA and SARIMA model fitting
7. Residual diagnostic checking
8. Forecasting for the testing period
9. Model comparison using AIC, MSE, RMSE and MAE

Both non-seasonal ARIMA models and seasonal SARIMA models were considered. Since the data are annual, the seasonal period of 12 was interpreted as a possible 12-year cyclical structure rather than ordinary monthly seasonality.

## Model Evaluation

The candidate models were compared using:

* AIC: to evaluate in-sample model fit and model complexity
* MSE: mean squared forecast error
* RMSE: root mean squared forecast error
* MAE: mean absolute forecast error

The forecast errors were calculated by comparing the predicted values from 2013 to 2025 with the actual observations in the testing data.

## Main Findings

The results show that annual ARIMA and SARIMA models can provide some useful short-term forecasting information, but their reliability is limited.

For TFR, the non-seasonal ARIMA models generally performed better than the seasonal models. For TLB, the seasonal SARIMA models performed better than the non-seasonal models. However, the forecast plots showed that the models did not fully capture the actual testing trends, especially after 2015.

One important feature was that several models predicted a peak around 2024. This may be related to the Dragon year effect in the Chinese zodiac. However, the actual data did not increase as strongly as the models predicted, suggesting that recent social and economic factors may have weakened this effect.

Overall, the findings suggest that Singapore's low fertility pattern is likely to continue in the short term, but exact future values should be interpreted with caution.

## Software

The analysis was conducted in R. The main R packages used were:

* `tidyverse`
* `tsibble`
* `feasts`
* `fable`
* `fabletools`

## How to Run the Code

1. Download or clone this repository.
2. Open the R script in the `code` folder.
3. Make sure the dataset `BirthsAndFertilityRatesAnnual.csv` is in the working directory.
4. Install the required R packages if they are not already installed.
5. Run the R script to reproduce the data processing, model fitting, diagnostics, and forecast plots.

## Author

Xirong Wei
Adelaide University
June 2026
