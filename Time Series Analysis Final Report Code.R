library(tidyverse)
library(fabletools)
library(feasts)
library(tsibble)
library(fable)

#Read file####
dat<-read.csv("BirthsAndFertilityRatesAnnual.csv")

fertility<-dat %>% select(DataSeries,Total.Fertility.Rate)
births<-dat %>% select(DataSeries,Total.Live.Births)

#Split training and testing data####
fertility_train<- fertility %>% filter(DataSeries<=2012) %>% 
  rename(years=DataSeries,TFR=Total.Fertility.Rate) %>% 
  arrange(years)

births_train<- births %>% filter(DataSeries<=2012) %>% 
  rename(years=DataSeries,TLB=Total.Live.Births) %>% 
  arrange(years)

fertility_test<- fertility %>% filter(DataSeries>2012) %>% 
  rename(years=DataSeries,TFR=Total.Fertility.Rate) %>% 
  arrange(years)

births_test<- births %>% filter(DataSeries>2012) %>% 
  rename(years=DataSeries,TLB=Total.Live.Births) %>% 
  arrange(years)

#View the total data####
plot(births_train$TLB)
plot(fertility_train$TFR)

TLBa<- births_train$TLB %>% ts(start = 1960)

plot(TLBa)

TFRa<-fertility_train$TFR %>% ts(start = 1960)

plot(TFRa)

# Convert test data to ts format
TFRb <- fertility_test$TFR %>% ts(start = 2013)
TLBb <- births_test$TLB %>% ts(start = 2013)

#Modelling####
## Model TFR ####
#Check the p value of TFR
unitroot_kpss(TFRa)
#Since the p value is less than 0.05, reject the hypothesis

#Check the first difference of the TFR which aims to remove trend
plot(diff(TFRa))
unitroot_kpss(diff(TFRa))
#Obviously diff(TFRa) isn't stationary, because the p value is still less than 0.05

#Try to check the ACF of the TFR
acf(diff(TFRa),lag=45)
#ACF has a spike on lag8
#Double check PACF
pacf(diff(TFRa),lag=45)
#PACF has a spike on lag16, so try to model with ARIMA(16,1,0)
acf(arima(TFRa,order=c(16,1,0))$resid,lag=45)
pacf(arima(TFRa,order=c(16,1,0))$resid,lag=45)
arima(TFRa,order=c(16,1,0))
#Valid

#The residual is the white noise
#Try another model
#ARIMA(16,1,1)
acf(arima(TFRa,order=c(16,1,1))$resid,lag=45)
pacf(arima(TFRa,order=c(16,1,1))$resid,lag=45)
arima(TFRa,order=c(16,1,1))
#Valid

acf(arima(TFRa,order=c(15,1,1))$resid,lag=45)
pacf(arima(TFRa,order=c(15,1,1))$resid,lag=45)
arima(TFRa,order=c(15,1,1))
#Valid

acf(arima(TFRa,order=c(15,1,0))$resid,lag=45)
pacf(arima(TFRa,order=c(15,1,0))$resid,lag=45)
#arima(TFRa,order=c(16,1,1))
#Have significent spike on lag16,reject

acf(arima(TFRa,order=c(14,1,1))$resid,lag=45)
pacf(arima(TFRa,order=c(14,1,1))$resid,lag=45)
#Invalid: Significant spike on lag 15

acf(arima(TFRa,order=c(14,1,2))$resid,lag=45)
pacf(arima(TFRa,order=c(14,1,2))$resid,lag=45)
#Invalid: Significant spike on lag 15

acf(arima(TFRa,order=c(14,1,3))$resid,lag=45)
pacf(arima(TFRa,order=c(14,1,3))$resid,lag=45)
arima(TFRa,order=c(14,1,3))
#Valid

acf(arima(TFRa,order=c(13,1,3))$resid,lag=45)
pacf(arima(TFRa,order=c(13,1,3))$resid,lag=45)
arima(TFRa,order=c(13,1,3))
#Valid

## Model TLB ####
plot(TLBa)
unitroot_kpss(TLBa)
#Have a obvious trend

plot(diff(TLBa))
unitroot_kpss(diff(TLBa))
#The first difference of TLB is stationary
acf(diff(TLBa),lag=45)
pacf(diff(TLBa),lag=45)
#Have significant spike on lag13, so initial model is ARIMA(13,1,0)

acf(arima(TLBa,order = c(13,1,0))$resid,lag=45)
pacf(arima(TLBa,order = c(13,1,0))$resid,lag=45)
#Invalid: Have a marginally significant spike on lag 15

acf(arima(TLBa,order = c(13,1,1))$resid,lag=45)
pacf(arima(TLBa,order = c(13,1,1))$resid,lag=45)
arima(TLBa,order = c(13,1,1))
#Valid

acf(arima(TLBa,order = c(12,1,1))$resid,lag=45)
pacf(arima(TLBa,order = c(12,1,1))$resid,lag=45)
#Invalid: Have a significant spike on lag14

acf(arima(TLBa,order = c(12,1,2))$resid,lag=45)
pacf(arima(TLBa,order = c(12,1,2))$resid,lag=45)
#Invalid: Have a significant spike on lag14

acf(arima(TLBa,order = c(12,1,3))$resid,lag=45)
pacf(arima(TLBa,order = c(12,1,3))$resid,lag=45)
#Invalid: Error in ARIMA(12,1,3) has not stationary component

acf(arima(TLBa,order = c(13,1,2))$resid,lag=45)
pacf(arima(TLBa,order = c(13,1,2))$resid,lag=45)
arima(TLBa,order = c(13,1,2))
#Valid

acf(arima(TLBa,order = c(13,1,3))$resid,lag=45)
pacf(arima(TLBa,order = c(13,1,3))$resid,lag=45)
arima(TLBa,order = c(13,1,3))
#Valid

acf(arima(TLBa,order = c(13,1,4))$resid,lag=45)
pacf(arima(TLBa,order = c(13,1,4))$resid,lag=45)
arima(TLBa,order = c(13,1,4))
#Valid

# Try Seasonal Model ####
## TFR ####
acf(diff(diff(TFRa),12),lag=45)
pacf(diff(diff(TFRa),12),lag=45)

#marginally spike on lag 5
acf(arima(TFRa,order = c(5,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(5,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TFRa,order = c(5,1,0),seasonal = list(order=c(1,1,0),period = 12))
#Valid      

acf(arima(TFRa,order = c(4,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(4,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 4.

acf(arima(TFRa,order = c(4,1,1),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(4,1,1),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 15.

acf(arima(TFRa,order = c(4,1,2),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(4,1,2),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 7

acf(arima(TFRa,order = c(4,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(4,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 15

# acf(arima(TFRa,order = c(4,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# pacf(arima(TFRa,order = c(4,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# arima(TFRa,order = c(4,1,4),seasonal = list(order=c(1,1,0),period = 12))
# #Valid
# 
# acf(arima(TFRa,order = c(3,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# pacf(arima(TFRa,order = c(3,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# arima(TFRa,order = c(3,1,4),seasonal = list(order=c(1,1,0),period = 12))
# #Valid
# 
# acf(arima(TFRa,order = c(2,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# pacf(arima(TFRa,order = c(2,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# #Invalid: Signigicant spike on lag 15
# 
# acf(arima(TFRa,order = c(2,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# pacf(arima(TFRa,order = c(2,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
# #Invalid: Significant spike on lag 15

acf(arima(TFRa,order = c(3,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(3,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TFRa,order = c(3,1,3),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TFRa,order = c(2,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(2,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TFRa,order = c(2,1,3),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TFRa,order = c(1,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(1,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TFRa,order = c(1,1,3),seasonal = list(order=c(1,1,0),period = 12))

acf(arima(TFRa,order = c(0,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(0,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 8

acf(arima(TFRa,order = c(0,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TFRa,order = c(0,1,4),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Valid


## TLB ####
acf(diff(diff(TLBa),12),lag=45)
pacf(diff(diff(TLBa),12),lag=45)


acf(arima(TLBa,order = c(4,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(4,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TLBa,order = c(4,1,0),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TLBa,order = c(3,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(3,1,0),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 4

acf(arima(TLBa,order = c(3,1,1),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(3,1,1),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TLBa,order = c(3,1,1),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TLBa,order = c(2,1,1),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(2,1,1),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 4

acf(arima(TLBa,order = c(2,1,2),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(2,1,2),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 4

acf(arima(TLBa,order = c(2,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(2,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TLBa,order = c(2,1,3),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TLBa,order = c(1,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(1,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TLBa,order = c(1,1,3),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TLBa,order = c(0,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(0,1,3),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
arima(TLBa,order = c(0,1,3),seasonal = list(order=c(1,1,0),period = 12))
#Valid

acf(arima(TLBa,order = c(0,1,2),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
pacf(arima(TLBa,order = c(0,1,2),seasonal = list(order=c(1,1,0),period = 12))$resid,lag=45)
#Invalid: Significant spike on lag 4

# Forecasting ####

## Plot ####
### Helper function: fit models and get forecast data####
get_forecast_df <- function(train_ts, test_ts, model_specs) {
  years <- as.numeric(time(test_ts))
  
  out <- data.frame(Year = years, Actual = as.numeric(test_ts))
  
  for (model_name in names(model_specs)) {
    spec <- model_specs[[model_name]]
    
    if (is.null(spec$seasonal)) {
      fit <- arima(train_ts, order = spec$order)
    } else {
      fit <- arima(train_ts, order = spec$order, seasonal = spec$seasonal)
    }
    
    pred <- predict(fit, n.ahead = length(test_ts))$pred
    out[[model_name]] <- as.numeric(pred)
  }
  
  out
}

# Helper function: plot actual vs forecasts
plot_forecast_compare <- function(df, title_text, ylab_text) {
  df_long <- df %>%
    pivot_longer(
      cols = -Year,
      names_to = "Series",
      values_to = "Value"
    )
  
  ggplot(df_long, aes(x = Year, y = Value, colour = Series, linetype = Series)) +
    geom_line(linewidth = 1) +
    labs(
      title = title_text,
      x = "Year",
      y = ylab_text,
      colour = "",
      linetype = ""
    ) +
    theme_minimal()
}

### TFR non-seasonal models ####
TFR_nonseasonal_models <- list(
  "Actual" = NULL,   # placeholder, not used in fitting
  "ARIMA(16,1,0)" = list(order = c(16, 1, 0), seasonal = NULL),
  "ARIMA(16,1,1)" = list(order = c(16, 1, 1), seasonal = NULL),
  "ARIMA(15,1,1)" = list(order = c(15, 1, 1), seasonal = NULL),
  "ARIMA(14,1,3)" = list(order = c(14, 1, 3), seasonal = NULL),
  "ARIMA(13,1,3)" = list(order = c(13, 1, 3), seasonal = NULL)
)

# remove placeholder before fitting
TFR_nonseasonal_models_fit <- TFR_nonseasonal_models[names(TFR_nonseasonal_models) != "Actual"]

TFR_nonseasonal_df <- get_forecast_df(TFRa, TFRb, TFR_nonseasonal_models_fit)

plot_forecast_compare(
  TFR_nonseasonal_df,
  title_text = "TFR Non-seasonal Models: Forecast vs Actual (2013-2025)",
  ylab_text = "Total Fertility Rate"
)

### TLB non-seasonal models ####
TLB_nonseasonal_models <- list(
  "ARIMA(13,1,1)" = list(order = c(13, 1, 1), seasonal = NULL),
  "ARIMA(13,1,2)" = list(order = c(13, 1, 2), seasonal = NULL),
  "ARIMA(13,1,3)" = list(order = c(13, 1, 3), seasonal = NULL),
  "ARIMA(13,1,4)" = list(order = c(13, 1, 4), seasonal = NULL)
)

TLB_nonseasonal_df <- get_forecast_df(TLBa, TLBb, TLB_nonseasonal_models)

plot_forecast_compare(
  TLB_nonseasonal_df,
  title_text = "TLB Non-seasonal Models: Forecast vs Actual (2013-2025)",
  ylab_text = "Total Live Births"
)


### TFR seasonal models ####
TFR_seasonal_models <- list(
  "ARIMA(5,1,0)(1,1,0)[12]" = list(
    order = c(5, 1, 0),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(3,1,3)(1,1,0)[12]" = list(
    order = c(3, 1, 3),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(2,1,3)(1,1,0)[12]" = list(
    order = c(2, 1, 3),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(0,1,4)(1,1,0)[12]" = list(
    order = c(0, 1, 4),
    seasonal = list(order = c(1, 1, 0), period = 12)
  )
)

TFR_seasonal_df <- get_forecast_df(TFRa, TFRb, TFR_seasonal_models)

plot_forecast_compare(
  TFR_seasonal_df,
  title_text = "TFR Seasonal Models: Forecast vs Actual (2013-2025)",
  ylab_text = "Total Fertility Rate"
)


### TLB seasonal models ####
TLB_seasonal_models <- list(
  "ARIMA(4,1,0)(1,1,0)[12]" = list(
    order = c(4, 1, 0),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(3,1,1)(1,1,0)[12]" = list(
    order = c(3, 1, 1),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(2,1,3)(1,1,0)[12]" = list(
    order = c(2, 1, 3),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(1,1,3)(1,1,0)[12]" = list(
    order = c(1, 1, 3),
    seasonal = list(order = c(1, 1, 0), period = 12)
  ),
  "ARIMA(0,1,3)(1,1,0)[12]" = list(
    order = c(0, 1, 3),
    seasonal = list(order = c(1, 1, 0), period = 12)
  )
)

TLB_seasonal_df <- get_forecast_df(TLBa, TLBb, TLB_seasonal_models)

plot_forecast_compare(
  TLB_seasonal_df,
  title_text = "TLB Seasonal Models: Forecast vs Actual (2013-2025)",
  ylab_text = "Total Live Births"
)

## MSE and MAE ####

### TFR non-seasonal models ####

fit_TFR_1610 <- arima(TFRa, order = c(16, 1, 0))
pred_TFR_1610 <- predict(fit_TFR_1610, n.ahead = length(TFRb))$pred
fit_TFR_1610$aic
mean((pred_TFR_1610 - TFRb)^2)
sqrt(mean((pred_TFR_1610 - TFRb)^2))
mean(abs(pred_TFR_1610 - TFRb))


fit_TFR_1611 <- arima(TFRa, order = c(16, 1, 1))
pred_TFR_1611 <- predict(fit_TFR_1611, n.ahead = length(TFRb))$pred
fit_TFR_1611$aic
mean((pred_TFR_1611 - TFRb)^2)
sqrt(mean((pred_TFR_1611 - TFRb)^2))
mean(abs(pred_TFR_1611 - TFRb))


fit_TFR_1511 <- arima(TFRa, order = c(15, 1, 1))
pred_TFR_1511 <- predict(fit_TFR_1511, n.ahead = length(TFRb))$pred
fit_TFR_1511$aic
mean((pred_TFR_1511 - TFRb)^2)
sqrt(mean((pred_TFR_1511 - TFRb)^2))
mean(abs(pred_TFR_1511 - TFRb))


fit_TFR_1413 <- arima(TFRa, order = c(14, 1, 3))
pred_TFR_1413 <- predict(fit_TFR_1413, n.ahead = length(TFRb))$pred
fit_TFR_1413$aic
mean((pred_TFR_1413 - TFRb)^2)
sqrt(mean((pred_TFR_1413 - TFRb)^2))
mean(abs(pred_TFR_1413 - TFRb))


fit_TFR_1313 <- arima(TFRa, order = c(13, 1, 3))
pred_TFR_1313 <- predict(fit_TFR_1313, n.ahead = length(TFRb))$pred
fit_TFR_1313$aic
mean((pred_TFR_1313 - TFRb)^2)
sqrt(mean((pred_TFR_1313 - TFRb)^2))
mean(abs(pred_TFR_1313 - TFRb))

### TLB non-seasonal models ####

fit_TLB_1311 <- arima(TLBa, order = c(13, 1, 1))
pred_TLB_1311 <- predict(fit_TLB_1311, n.ahead = length(TLBb))$pred
fit_TLB_1311$aic
mean((pred_TLB_1311 - TFRb)^2)
sqrt(mean((pred_TLB_1311 - TLBb)^2))
mean(abs(pred_TLB_1311 - TLBb))


fit_TLB_1312 <- arima(TLBa, order = c(13, 1, 2))
pred_TLB_1312 <- predict(fit_TLB_1312, n.ahead = length(TLBb))$pred
fit_TLB_1312$aic
mean((pred_TLB_1312 - TLBb)^2)
sqrt(mean((pred_TLB_1312 - TLBb)^2))
mean(abs(pred_TLB_1312 - TLBb))


fit_TLB_1313 <- arima(TLBa, order = c(13, 1, 3))
pred_TLB_1313 <- predict(fit_TLB_1313, n.ahead = length(TLBb))$pred
fit_TLB_1313$aic
mean((pred_TLB_1313 - TLBb)^2)
sqrt(mean((pred_TLB_1313 - TLBb)^2))
mean(abs(pred_TLB_1313 - TLBb))


fit_TLB_1314 <- arima(TLBa, order = c(13, 1, 4))
pred_TLB_1314 <- predict(fit_TLB_1314, n.ahead = length(TLBb))$pred
fit_TLB_1314$aic
mean((pred_TLB_1314 - TLBb)^2)
sqrt(mean((pred_TLB_1314 - TLBb)^2))
mean(abs(pred_TLB_1314 - TLBb))

### TFR seasonal models ####

fit_TFR_s_510 <- arima(
  TFRa,
  order = c(5, 1, 0),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TFR_s_510 <- predict(fit_TFR_s_510, n.ahead = length(TFRb))$pred
fit_TFR_s_510$aic
mean((pred_TFR_s_510 - TFRb)^2)
sqrt(mean((pred_TFR_s_510 - TFRb)^2))
mean(abs(pred_TFR_s_510 - TFRb))


fit_TFR_s_313 <- arima(
  TFRa,
  order = c(3, 1, 3),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TFR_s_313 <- predict(fit_TFR_s_313, n.ahead = length(TFRb))$pred
fit_TFR_s_313$aic
mean((pred_TFR_s_313 - TFRb)^2)
sqrt(mean((pred_TFR_s_313 - TFRb)^2))
mean(abs(pred_TFR_s_313 - TFRb))


fit_TFR_s_213 <- arima(
  TFRa,
  order = c(2, 1, 3),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TFR_s_213 <- predict(fit_TFR_s_213, n.ahead = length(TFRb))$pred
fit_TFR_s_213$aic
mean((pred_TFR_s_213 - TFRb)^2)
sqrt(mean((pred_TFR_s_213 - TFRb)^2))
mean(abs(pred_TFR_s_213 - TFRb))


fit_TFR_s_014 <- arima(
  TFRa,
  order = c(0, 1, 4),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TFR_s_014 <- predict(fit_TFR_s_014, n.ahead = length(TFRb))$pred
fit_TFR_s_014$aic
mean((pred_TFR_s_014 - TFRb)^2)
sqrt(mean((pred_TFR_s_014 - TFRb)^2))
mean(abs(pred_TFR_s_014 - TFRb))

### TLB seasonal models ####

fit_TLB_s_410 <- arima(
  TLBa,
  order = c(4, 1, 0),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TLB_s_410 <- predict(fit_TLB_s_410, n.ahead = length(TLBb))$pred
fit_TLB_s_410$aic
mean((pred_TLB_s_410 - TLBb)^2)
sqrt(mean((pred_TLB_s_410 - TLBb)^2))
mean(abs(pred_TLB_s_410 - TLBb))


fit_TLB_s_311 <- arima(
  TLBa,
  order = c(3, 1, 1),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TLB_s_311 <- predict(fit_TLB_s_311, n.ahead = length(TLBb))$pred
fit_TLB_s_311$aic
mean((pred_TLB_s_311 - TLBb)^2)
sqrt(mean((pred_TLB_s_311 - TLBb)^2))
mean(abs(pred_TLB_s_311 - TLBb))


fit_TLB_s_213 <- arima(
  TLBa,
  order = c(2, 1, 3),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TLB_s_213 <- predict(fit_TLB_s_213, n.ahead = length(TLBb))$pred
fit_TLB_s_213$aic
mean((pred_TLB_s_213 - TLBb)^2)
sqrt(mean((pred_TLB_s_213 - TLBb)^2))
mean(abs(pred_TLB_s_213 - TLBb))


fit_TLB_s_113 <- arima(
  TLBa,
  order = c(1, 1, 3),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TLB_s_113 <- predict(fit_TLB_s_113, n.ahead = length(TLBb))$pred
fit_TLB_s_113$aic
mean((pred_TLB_s_113 - TLBb)^2)
sqrt(mean((pred_TLB_s_113 - TLBb)^2))
mean(abs(pred_TLB_s_113 - TLBb))


fit_TLB_s_013 <- arima(
  TLBa,
  order = c(0, 1, 3),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
pred_TLB_s_013 <- predict(fit_TLB_s_013, n.ahead = length(TLBb))$pred
fit_TLB_s_013$aic
mean((pred_TLB_s_013 - TLBb)^2)
sqrt(mean((pred_TLB_s_013 - TLBb)^2))
mean(abs(pred_TLB_s_013 - TLBb))
