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


