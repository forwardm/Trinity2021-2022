#https://rpubs.com/alfado/msts
setwd("/Users/jingyiliao/Desktop/Business Forecasting/Business Forecasting")
library(forecast)
library(zoo)
library(TSA)
library(stats)
library(tseries)
library(timeDate)
library(fUnitRoots)


eur_usd.df <- read.csv("EURUSD_Others.csv")
eurusd.new.df <- data.frame(eur_usd.df$Date, eur_usd.df$EUR_USDClose)
colnames(eurusd.new.df)<-c("Date","EURUSD_Close")
head(eurusd.new.df)

#dealing with NULL value, and set NULL value to mean value (before+after)/2
for(i in 1:length(eurusd.new.df[,1])){
  if(eurusd.new.df[i,2] == "null"){
    k<-(as.numeric(eurusd.new.df[i-1,2])+as.numeric(eurusd.new.df[i+1,2]))/2
    eurusd.new.df[i,2]<-as.character(k)
  }
}
eurusd.new.df[,2] <- as.numeric(eurusd.new.df[,2])
colSums(is.na(eurusd.new.df))

eur_usd.ts <- ts(eurusd.new.df$EURUSD_Close,
                 start = c(2011, 10), end = c(2021, 10), frequency = 365)
eur_usd.ts %>% 
  autoplot(xlab = "Year", ylab = "Exchange Rate", main = "EUR to USD")+
  theme_minimal()

forex_kalman <- na_kalman(eur_usd.ts, model = "auto.arima", smooth = T)
forex_kalman %>%
  autoplot()+
  theme_minimal()

#Decomposing
dec <- decompose(x = forex_kalman, type = "additive")
dec %>% 
  autoplot()+
  theme_minimal()

#forex ts object to msts
eurusd_msts<- msts(eurusd.new.df$EURUSD_Close, seasonal.periods = c(7,365), 
                   start = c(2011,10), end = c(2021,10))
plot(eurusd_msts, main="EUR To USD", xlab="Year", ylab="Exchange Rate")

msts_kalman <- na_kalman(eurusd_msts, model = "auto.arima", smooth = T)
msts_kalman %>%
  autoplot()+
  theme_minimal()

msts_kalman %>% mstl() %>% autoplot() + theme_minimal()

msts_test <- tail(msts_kalman, 365) 
msts_train <- head(msts_kalman, length(msts_kalman)- length(msts_test))


##Forecasting For Validation

eurusd_sma.train <- SMA(x = msts_train, n = 5)
eurusd_sma.train <- msts (eurusd_sma.train, start = c(2011,10), seasonal.periods = c(7,365))

eurusd_sma.test <- SMA(x = msts_test, n = 5)
eurusd_sma.test <- msts (eurusd_sma.test, start = c(2020,10), seasonal.periods = c(7,365))

#Visualize the model to do comparison
msts_train %>% 
  autoplot(series = "Actual") +
  autolayer(eurusd_sma.train, series ="SMA")+
  scale_color_manual(values = c("Black", "Red"))+
  theme_minimal()

#Do the forecasting
eurusd_forecast_sma <- forecast(object = eurusd_sma.train, h = 365)
#Visualize the forecast model
msts_kalman %>% 
  autoplot(series = "train") +
  autolayer(msts_test, series = "test") +
  autolayer(eurusd_forecast_sma$fitted, series = "forecast train") +
  autolayer(eurusd_forecast_sma$mean, series = "forecast test") +
  theme_minimal()
#Check for the accuracy
sma_acc.train<- accuracy(object = eurusd_forecast_sma$fitted, msts_train)
sma_acc.train

sma_acc.test<-accuracy(object = eurusd_forecast_sma$mean, msts_test)
sma_acc.test


##Forecasting For Incoming year 2022
#Build the SMA model
eurusd_sma_2022 <- SMA(x = msts_kalman, n = 5)
eurusd_sma_2022 <- msts(data = eurusd_sma_2022, start = c(2011,10),
                        end = c(2021,10), seasonal.periods = c(7,365))
#Forecasting
eurusd_forecast_sma_2022 <- forecast(object = eurusd_sma_2022, h = 365)
#Visualization
ggplotly(msts_kalman %>% 
           autoplot(series = "Actual") +
           autolayer(eurusd_forecast_sma_2022$mean, series = "Forecast 2022") +
           labs(title = "EUR to USD Forecast for 2022 using SMA", y = "EUR to USD")+
           theme_minimal()
)

