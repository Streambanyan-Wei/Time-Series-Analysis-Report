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


# TFR model fitted on training data


