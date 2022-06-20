library(dplyr)
library(vars)
install.packages("zoo")               # Install zoo package
library("zoo")         


library(forecast)
install.packages("tsfgrnn")
library(tsfgrnn)
install.packages("pracma")
library(pracma)
?movavg
setwd("C:/Users/Tejendra/Desktop/FoldersOnDesktop/UdemyCourse/Section7")
require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(tsibble)  #tsibble for time series based on tidy principles
require(fable)  #for forecasting based on tidy principles
require(ggfortify)  #for plotting timeseries
require(forecast)  #for forecast function
require(tseries)
require(chron)
require(lubridate)
require(directlabels)
require(zoo)
require(lmtest)
require(TTR)  #for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)









#creating data, assiging nas to previous values and making all variables numeric
EuroForecast.df <- read.csv("LaggedData.csv")
head(EuroForecast.df)
str(EuroForecast.df)
EuroForecast.df$Today_EUR_USDClose <- as.numeric(EuroForecast.df$Today_EUR_USDClose)
EuroForecast.df$Prev_European_Basket_Close <- as.numeric(EuroForecast.df$Prev_European_Basket_Close)
EuroForecast.df$Prev_Commodities.Basket.Close <- as.numeric(EuroForecast.df$Prev_Commodities.Basket.Close)
EuroForecast.df$Prev_S.P_500_Close <- as.numeric(EuroForecast.df$Prev_S.P_500_Close)
EuroForecast.df$Prev_Brent_Crude_Spot <- as.numeric(EuroForecast.df$Prev_Brent_Crude_Spot)
EuroForecast.df <- na.locf(EuroForecast.df)  

#plotting data to see if any relationships
#plot(EuroForecast.df)
#plot(EuroForecast.df$Prev_Brent_Crude_Spot,EuroForecast.df$Today_EUR_USDClose)
#plot(EuroForecast.df$Prev_Commodities.Basket.Close, EuroForecast.df$Today_EUR_USDClose)
#plot(EuroForecast.df$Prev_European_Basket_Close, EuroForecast.df$Today_EUR_USDClose)
#plot(EuroForecast.df$Prev_S.P_500_Close, EuroForecast.df$Today_EUR_USDClose)


#creating trends of each variable
?rollmean()
n_variable <- 1
trend_EUR <- data.frame("trend_Euro"=rollmean(EuroForecast.df$Today_EUR_USDClose, k=n_variable, align = "left"))
length(trend_EUR$trend_Euro)
trend_EUR<-data.frame("trend_Euro"=trend_EUR[1:(length(trend_EUR$trend_Euro)-1),])
length(trend_EUR$trend_Euro)
na_valid_6 <- data.frame("trend_Euro"=matrix(NA, nrow = n_variable, ncol = 1)) #have to be previous rolling average
trend_EURO_2 <- rbind(na_valid_6,trend_EUR)


trend_SP <- data.frame("trend_SP"=rollmean(EuroForecast.df$Prev_S.P_500_Close, k=n_variable,align = "left"))
na_valid_6 <- data.frame("trend_SP"=matrix(NA, nrow = n_variable-1, ncol = 1))
trend_SP_2 <- rbind(na_valid_6,trend_SP)
head(trend_SP_2, n= 13)


trend_Oil <- data.frame("trend_Oil"=rollmean(EuroForecast.df$Prev_Brent_Crude_Spot, k=n_variable, align = "left"))
na_valid_6 <- data.frame("trend_Oil"=matrix(NA, nrow = n_variable-1, ncol = 1))
trend_Oil_2 <- rbind(na_valid_6,trend_Oil)

trend_EuroBasket<- data.frame("trend_EuroBasket"=rollmean(EuroForecast.df$Prev_European_Basket_Close, k=n_variable, align = "left"))
na_valid_6 <- data.frame("trend_EuroBasket"=matrix(NA, nrow = n_variable-1, ncol = 1))
trend_EuroBasket_2 <- rbind(na_valid_6,trend_EuroBasket)
str(trend_EuroBasket_2)

trend_Com<- data.frame("trend_Com"=rollmean(EuroForecast.df$Prev_Commodities.Basket.Close, k=n_variable, align = "left"))
na_valid_6 <- data.frame("trend_Com"=matrix(NA, nrow = n_variable-1, ncol = 1))
trend_Com_2 <- rbind(na_valid_6,trend_Com)

#puts them into the original dataset
EuroForecast.df$trend_Com <- as.vector(t(trend_Com_2))
EuroForecast.df$trend_Oil <- as.vector(t(trend_Oil_2))
EuroForecast.df$trend_SP <- as.vector(t(trend_SP_2))
EuroForecast.df$trend_EuroBasket <- as.vector(t(trend_EuroBasket_2))
EuroForecast.df$trend_EURUSD <- as.vector(t(trend_EURO_2))

head(EuroForecast.df)

#these paramaters are modifiable and get
offset <- 30
startRow <- length(EuroForecast.df$TimeSeries)-90
endRow <- length(EuroForecast.df$TimeSeries)



#do analysis on forecast
Forecast.df <- EuroForecast.df[c(startRow:endRow),c(1:12)]
head(EuroForecast.df)

library(forecast)


#splitting the data
max_length <- length(Forecast.df$TimeSeries)
train.ts <- Forecast.df
train.ts <- train.ts %>% slice(1:(max_length-offset))
valid.ts <- Forecast.df 
valid.ts <- valid.ts %>% slice((max_length-offset+1):max_length)

str(train.ts)


#run regression model with all variables including trend
my_reg_forecast <-  glm(Today_EUR_USDClose ~ Prev_Commodities.Basket.Close + Prev_Brent_Crude_Spot+
                         Prev_European_Basket_Close+ Prev_S.P_500_Close
                       +trend_EURUSD 
                       + trend_Com + trend_Oil
                       +trend_SP+ trend_EuroBasket, data = train.ts)

summary(my_reg_forecast)








#training set
pred.train <-  predict(my_reg_forecast, train.ts)
pred.train.df <- tibble(pred.train)
pred.train.df$TimeSeries <- seq.int(nrow(pred.train.df))
pred.train.df$TimeSeries <- pred.train.df$TimeSeries
pred.train.df$residuals <-   train.ts$Today_EUR_USDClose - pred.train.df$pred.train

accuracy(pred.train, train.ts$Today_EUR_USDClose)


#--------------new code Nov 25th 2021--------------------#
lag_train <- lag(train.ts, 1)

naive_tomorrow_today <-  train.ts$Today_EUR_USDClose - lag_train$Today_EUR_USDClose 
train_pred_tomorrow_today <-pred.train - lag_train$Today_EUR_USDClose 


sum(naive_tomorrow_today >=0 , na.rm=TRUE)
sum(naive_tomorrow_today <=0 , na.rm=TRUE)

sum(train_pred_tomorrow_today >=0 && naive_tomorrow_today >=0, na.rm=TRUE)
sum(train_pred_tomorrow_today <=0 , na.rm=TRUE)

new_var <- (train_pred_tomorrow_today >0)
new_naive <- (naive_tomorrow_today>0)

new_naive_lag <-lag(new_naive)

sum(new_var == new_naive, na.rm = TRUE)/length(new_var)

sum(new_naive_lag == new_naive, na.rm = TRUE)/length(new_var)


#--------------new code Nov 25th 2021--------------------#
view(train.ts)


#validation set
pred.valid <- predict(my_reg_forecast, valid.ts)
pred.valid.df <- tibble(pred.valid)
pred.valid.df$TimeSeries <- seq.int(nrow(pred.valid.df))
pred.valid.df$TimeSeries <- pred.valid.df$TimeSeries+(max_length-offset+1)
accuracy(pred.valid, valid.ts$Today_EUR_USDClose)
pred.valid.df$residuals <- valid.ts$Today_EUR_USDClose - pred.valid.df$pred.valid




#--------------new code Nov 25th 2021--------------------#
lag_train <- lag(valid.ts, 1)

naive_tomorrow_today <-  valid.ts$Today_EUR_USDClose - lag_train$Today_EUR_USDClose 
train_pred_tomorrow_today <-pred.valid - lag_train$Today_EUR_USDClose 


sum(naive_tomorrow_today >=0 , na.rm=TRUE)
sum(naive_tomorrow_today <=0 , na.rm=TRUE)

sum(train_pred_tomorrow_today >=0 && naive_tomorrow_today >=0, na.rm=TRUE)
sum(train_pred_tomorrow_today <=0 , na.rm=TRUE)

new_var <- (train_pred_tomorrow_today >0)
new_naive <- (naive_tomorrow_today>0)

new_naive_lag <-lag(new_naive)

sum(new_var == new_naive, na.rm = TRUE)/length(new_var)

sum(new_naive_lag == new_naive, na.rm = TRUE)/length(new_var)


#--------------new code Nov 25th 2021--------------------#










pacf(Forecast.df$Today_EUR_USDClose)





#transforms train
train_12<- data.frame(pred.train.df$pred.train)
names(train_12)[names(train_12) == 'pred.train.df.pred.train'] <- 'Train_1'
na_1 <- data.frame(matrix(NA, nrow = offset, ncol = 1))
names(na_1)[names(na_1) == 'matrix.NA..nrow...offset..ncol...1.'] <- 'Train_1'
pred.train2 <- rbind(train_12,na_1)
length(pred.train2$Train_1)

#transforms residuals
residuals_12<- data.frame(pred.train.df$residuals)
names(residuals_12)[names(residuals_12) == 'pred.train.df.residuals'] <- 'Residuals_1'
na_res <- data.frame(matrix(NA, nrow = offset, ncol = 1))
names(na_res)[names(na_res) == 'matrix.NA..nrow...offset..ncol...1.'] <- 'Residuals_1'
residuals.train2 <- rbind(residuals_12,na_res)

#transform valid
valid_12<- data.frame(pred.valid.df$pred.valid)
names(valid_12)[names(valid_12) == 'pred.valid.df.pred.valid'] <- 'Valid_1'
na_valid <- data.frame(matrix(NA, nrow = endRow-startRow-offset+1, ncol = 1))
names(na_valid)[names(na_valid) == 'matrix.NA..nrow...endRow...startRow...offset...1..ncol...1.'] <- 'Valid_1'
pred.valid2 <- rbind(na_valid,valid_12)
str(na_valid)
str(valid_12)

#transforms valid residuals
valid_res<- data.frame(pred.valid.df$residuals)
names(valid_res)[names(valid_res) == 'pred.valid.df.residuals'] <- 'Valid_1'
na_valid_res <- data.frame(matrix(NA, nrow = endRow-startRow-offset+1, ncol = 1))
names(na_valid_res)[names(na_valid_res) == 'matrix.NA..nrow...endRow...startRow...offset...1..ncol...1.'] <- 'Valid_1'
res.valid2 <- rbind(na_valid_res,valid_res)
str(na_valid_res)
str(valid_res)



plot_all<- data.frame(pred.train2$Train_1,
                      residuals.train2$Residuals_1+1.16, 
                      EuroForecast.df$Today_EUR_USDClose[startRow:endRow],
                      pred.valid2$Valid_1,
                      res.valid2$Valid_1+1.16
)
str(plot_all)
plot_all$TimeSeries <- 1:nrow(plot_all)

dev.off()




head(plot_all)

#now plot everything
everything.plot <- ggplot(plot_all, aes(x=TimeSeries)) +
  geom_line( aes(y=pred.train2.Train_1 , color = "Train Prediction"), linetype = "dashed")+
  geom_line(aes(y=residuals.train2.Residuals_1...1.16 , color='Train Residuals'), linetype = "dotted") + 
  geom_line( aes(y=EuroForecast.df.Today_EUR_USDClose.startRow.endRow.,color = 'Actual'), size = 0.5)+
  geom_line(aes(y=pred.valid2.Valid_1, color = "Valid Prediction"), linetype = "dashed")+
  geom_line( aes(y=res.valid2.Valid_1...1.16, color='Valid Residuals'), linetype = "dotted") +
  scale_color_manual(name = "Y series", values = c("Train Prediction" = "green", "Train Residuals" = "red",
                                                   "Actual"="black", "Valid Prediction"= "blue",
                                                   "Valid Residuals" = "darkRed"))+
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the first axis
    name = "Euro/USD ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.-1.16, name="Residuals"))+
  labs(title="Moving Average Multi-Variable Training & Validation Predictions with Residuals")
everything.plot











#the simple naive hypothesis
lag_1 <- lead(Forecast.df)
lag_1$TimeSeries <- lag_1$TimeSeries -1

head(Forecast.df)
head(lag_1)
tail(Forecast.df)
tail(lag_1)

residual_2 = as_data_frame(lag_1$Today_EUR_USDClose-Forecast.df$Today_EUR_USDClose)



#look at residuals
residuals_naive <- as_data_frame(lag_1$Today_EUR_USDClose-Forecast.df$Today_EUR_USDClose)
residuals_naive$count <-  seq.int(nrow(residuals_naive))

plot(1:length(residual_2),residual_2)

accuracy( lag_1$Today_EUR_USDClose,Forecast.df$Today_EUR_USDClose)







#the simple naive hypothesis
lag_1_train <- lead(train.ts)
lag_1_train$TimeSeries <- lag_1_train$TimeSeries -1

lag_1_train <- na.locf(lag_1_train)




accuracy( lag_1_train$Today_EUR_USDClose,train.ts$Today_EUR_USDClose)


#the simple naive hypothesis
lag_1_valid <- lead(valid.ts)
lag_1_valid$TimeSeries <- lag_1_valid$TimeSeries -1

lag_1_valid <- na.locf(lag_1_valid)
?na.locf


accuracy( lag_1_valid$Today_EUR_USDClose,valid.ts$Today_EUR_USDClose)

