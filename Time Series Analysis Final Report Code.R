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
plot(arima(TFRa,order=c(16,1,0))$resid,lag=45) 
plot(acf(arima(TFRa,order=c(16,1,0))$resid,lag=45))
plot(pacf(arima(TFRa,order=c(16,1,0))$resid,lag=45))
arima(TFRa,order=c(16,1,0))

#The residual is the white noise
#Try another model
#ARIMA(16,1,1)
plot(acf(arima(TFRa,order=c(16,1,1))$resid,lag=45))
plot(pacf(arima(TFRa,order=c(16,1,1))$resid,lag=45))
arima(TFRa,order=c(16,1,1))

plot(acf(arima(TFRa,order=c(15,1,1))$resid,lag=45))
plot(pacf(arima(TFRa,order=c(15,1,1))$resid,lag=45))
arima(TFRa,order=c(15,1,1))

plot(acf(arima(TFRa,order=c(15,1,0))$resid,lag=45))
plot(pacf(arima(TFRa,order=c(15,1,0))$resid,lag=45))
arima(TFRa,order=c(16,1,1))
#Have significent spike on lag16,reject


