# Forecast of energy production by a private
# solar panel installation in eastern Poland
# 
# Kuba T³uczek, 2022
# Implementation of knowledge acquired from
# https://otexts.com/fpp3/

library(forecast)
library(dplyr)
library(tsibble)
library(imputeTS)
library(Cairo)
library(fable)
library(ggplot2)
library(feasts)
library(fpp2)
library(tidyr)

# Reading data and converting to tsibble
energia <- read.table("energia.csv", sep=";", header=T)
energia <- energia %>%
  mutate(month = yearmonth(Data)) %>%
  select(-Data)%>%
  as_tsibble(index = month)

# Imputing missing data with seasonal split
energia$Produkcja1 <- na_seasplit(energia2$Produkcja1, find_frequency = T)
energia$Produkcja2 <- na_seasplit(energia2$Produkcja2, find_frequency = T)
energia$Zakup <- na_seasplit(energia2$Zakup, find_frequency = T)
energia$Sprzedaz <- na_seasplit(energia2$Sprzedaz, find_frequency = T)

# Exploratory plots
CairoWin()
autoplot(energia, Produkcja1) +
  labs(y="KWh", title = "Produkcja w inwerterze nr 1",
       x = "Miesi¹c")

autoplot(energia, Produkcja2) +
  labs(y="KWh", title = "Produkcja w inwerterze nr 2",
       x = "Miesi¹c")

autoplot(energia, Zakup) +
  labs(y="KWh", x = "Miesi¹c",
       title = "Iloœæ zakupionej energii elektrycznej")

autoplot(energia, Sprzedaz) +
  labs(y="KWh", x = "Miesi¹c",
       title = "Iloœæ sprzedanej energii elektrycznej")

# Calculated variables
energia %>%
  mutate(ProdukcjaRazem = Produkcja1 + Produkcja2, 
         Bilans = Sprzedaz - Zakup) -> energia

autoplot(energia, Bilans) +
  labs(y="KWh", x = "Miesi¹c",
       title = "Bilans sprzedanej i zakupionej energii")

# Seasonal plot and subseries
energia %>%
  gg_season(Produkcja1, labels = "both")

energia %>%
  gg_subseries(Produkcja1)

# STL decomposition
energia %>%
  select(month, ProdukcjaRazem) %>%
  as_tsibble(index = month) -> energia_produkcja

energia_produkcja %>%
  stl(s.window = "periodic", robust = T) %>%
  autoplot() + labs(title = "STL decomposition of total production")

# Automatic ARIMA model
energia %>%
  model(auto = ARIMA(ProdukcjaRazem, stepwise = F, approximation = F)) -> fit_ari

forecast(fit_ari, h=12) %>%
  filter(.model=='auto') %>%
  hilo() %>%
  select(-ProdukcjaRazem,-.model)

forecast(fit_ari, h=12) %>%
  filter(.model=='auto') %>%
  autoplot(energia)

# ETS
prodr <- ts(energia$ProdukcjaRazem, start = c(2016,3), frequency = 12)
ets_add <- hw(prodr, seasonal = "additive")
ets_mul <- hw(prodr, seasonal = "multiplicative")
ets_dmp <- hw(prodr, damped = T, seasonal = "multiplicative")

autoplot(prodr) +
  autolayer(ets_add, series = "HW Additive", PI=F) +
  autolayer(ets_mul, series = "HW Multi", PI=F) +
  autolayer(ets_dmp, series = "HW Multi-D", PI=F) + 
  xlab("Month") + 
  ylab("KWh") + 
  ggtitle("Total Energy Production") +
  guides(colour=guide_legend(title = "ETS method"))


ets_dmp %>% forecast(h=12) %>% autoplot()


# NNAR

# Without an external regressor
fitnn <- nnetar(prodr)
nnetforecast <- forecast(fitnn, h=12, PI = T)
nnetforecast
autoplot(nnetforecast)

# With an external regressor
forecast(energia$Bilans, h=12)
fitnn2 <- nnetar(prodr, xreg = energia$Bilans)
balance_forecast <- c(-964.29,-588.84,213.32,638.36,923.74,789.57,698.11,361.37,-363.04,-827.66,-1507.65,-1375.54)
nnetforecast2 <- forecast(fitnn2, PI=T, xreg = balance_forecast)
nnetforecast2
autoplot(nnetforecast2)