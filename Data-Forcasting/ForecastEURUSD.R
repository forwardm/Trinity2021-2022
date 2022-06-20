rm(list=ls()) 
library(dplyr)
library(corrplot)



EuroForecast.df <- read.csv("LaggedData.csv")
view(EuroForecast.df)
head(EuroForecast.df)
str(EuroForecast.df)
EuroForecast.df$Today_EUR_USDClose <- as.numeric(EuroForecast.df$Today_EUR_USDClose)
EuroForecast.df$Prev_European_Basket_Close <- as.numeric(EuroForecast.df$Prev_European_Basket_Close)
EuroForecast.df$Prev_Commodities.Basket.Close <- as.numeric(EuroForecast.df$Prev_Commodities.Basket.Close)
EuroForecast.df$Prev_S.P_500_Close <- as.numeric(EuroForecast.df$Prev_S.P_500_Close)
EuroForecast.df$Prev_Brent_Crude_Spot <- as.numeric(EuroForecast.df$Prev_Brent_Crude_Spot)
str(EuroForecast.df)
EuroForecast.df <- na.locf(EuroForecast.df) 





justEuro <-  EuroForecast.df[,c("Date","Today_EUR_USDClose")]
justEuro <- ts(justEuro, frequency = 365, start= c(2012,1))

?ggAcf
ggAcf(justone)
?Pacf
lag_euro<- lag(EuroForecast.df$Today_EUR_USDClose)

head(EuroForecast.df$Today_EUR_USDClose)
head(lag_euro)

ggPacf(justone,lag.max= 50)
head(difference_euro)
plot(1:length(difference_euro), difference_euro)

difference_euro <- data.frame(EuroForecast.df$Today_EUR_USDClose-lag_euro)
ggPacf(difference_euro,lag.max=150)

ggPacf(EuroForecast.df$Today_EUR_USDClose,lag.max= 50)
justone <- justEuro[,c("Today_EUR_USDClose")]





head(subset_personal)
view(subset_personal)
#correlations for personal characteristics
subset_personal <- EuroForecast.df[,c("Today_EUR_USDClose",
                                      "Prev_European_Basket_Close","Prev_Commodities.Basket.Close",
                                      "Prev_S.P_500_Close", "Prev_Brent_Crude_Spot"
                                   
)]
plot(subset_personal)
correlations <- cor(subset_personal, use="complete.obs")
corrplot(correlations, method="circle")


#scale the data so can view on graph
PlotForecast <- EuroForecast.df
PlotForecast$Today_EUR_USDClose<- PlotForecast$Today_EUR_USDClose*3000
PlotForecast$Prev_Commodities.Basket.Close<- PlotForecast$Prev_Commodities.Basket.Close*80
PlotForecast$Prev_Brent_Crude_Spot<- PlotForecast$Prev_Brent_Crude_Spot*60



#plots to view all and see the overall trends
summary(PlotForecast)
ggplot(data=PlotForecast, aes(x=TimeSeries))+
  geom_line(aes(y = Today_EUR_USDClose), color = "black") + 
  geom_line(aes(y = Prev_Commodities.Basket.Close), color="red") +
  geom_line(aes(y = Prev_Brent_Crude_Spot), color="green")+
  geom_line(aes(y = Prev_European_Basket_Close), color = "darkred") + 
  geom_line(aes(y = Prev_S.P_500_Close), color="steelblue")


offset <- 30
startRow <-length(EuroForecast.df$TimeSeries)-90
endRow <- length(EuroForecast.df$TimeSeries)



Forecast.df <- EuroForecast.df[c(startRow:endRow),c(1:7)]


library(forecast)


#splitting the data
max_length <- length(Forecast.df$TimeSeries)
train.ts <- Forecast.df
train.ts <- train.ts %>% slice(1:(max_length-offset))
valid.ts <- Forecast.df 
valid.ts <- valid.ts %>% slice((max_length-offset+1):max_length)




#run regression model?
my_reg_forecast <-  lm(Today_EUR_USDClose ~ Prev_Commodities.Basket.Close + Prev_Brent_Crude_Spot+
                         Prev_European_Basket_Close+ Prev_S.P_500_Close, data = train.ts)

summary(my_reg_forecast)



#run regression model wihtout stock index
my_reg_forecast <-  lm(Today_EUR_USDClose ~ Prev_Commodities.Basket.Close + Prev_Brent_Crude_Spot
                          , data = train.ts)

summary(my_reg_forecast)




#training set
pred.train <-  predict(my_reg_forecast, train.ts)
#residuals(pred.train.df)
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






(plot.train_residuals <- ggplot() + 
    geom_line(data = train.ts, aes(x=TimeSeries,y=Today_EUR_USDClose),
              color = 'black') + 
    geom_line(data = pred.train.df, aes(x=TimeSeries+startRow,y=pred.train),
              color='green')
)

#look at residuals
ggplot(pred.train.df)+geom_line(aes(x=TimeSeries,y=residuals))

#plots everything residuals, prediction actual
total.plot <- ggplot(pred.train.df, aes(x=TimeSeries)) +
  geom_line( aes(y=pred.train.df$pred.train ), color = "Green", linetype = "dashed")+
  geom_line( aes(y=pred.train.df$residuals+1.2), color='Red', linetype = "dotted") + 
  geom_line(data = train.ts, aes(x=TimeSeries-startRow,y=Today_EUR_USDClose),
            color = 'Black', size = 0.5)+
   # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the first axis
    name = "Euro/USD ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.-1.2, name="Residuals")
    
  )

total.plot

  










#validation set
pred.valid <- predict(my_reg_forecast, valid.ts)
pred.valid.df <- tibble(pred.valid)
pred.valid.df$TimeSeries <- seq.int(nrow(pred.valid.df))
pred.valid.df$TimeSeries <- pred.valid.df$TimeSeries+(max_length-offset+1)
accuracy(pred.valid, valid.ts$Today_EUR_USDClose)
pred.valid.df$residuals <-   valid.ts$Today_EUR_USDClose - pred.valid.df$pred.valid




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








#look at residuals
residuals_valid <- as_data_frame(pred.valid - valid.ts$Today_EUR_USDClose)
residuals_valid$count <-  seq.int(nrow(residuals_valid))

ggplot(residuals_valid)+geom_line(aes(x=count,y=value))

#graph of valid vs real
(plot.valid_residuals <- ggplot() + 
    geom_line(data = valid.ts, aes(x=TimeSeries-startRow,y=Today_EUR_USDClose),
              color = 'darkred') + 
    geom_line(data = pred.valid.df, aes(x=TimeSeries,y=pred.valid),
              color='darkblue')
)
plot.valid_residuals

#combined
ggplot(pred.valid.df, aes(x=TimeSeries)) +
  geom_line(data= pred.valid.df, aes(y=pred.valid ), color = "Green", linetype = "dashed")+
  geom_line(data= pred.valid.df, aes(y=residuals+1.18), color='Red', linetype = "dotted") + 
  geom_line(data = valid.ts, aes(x=TimeSeries-startRow,y=Today_EUR_USDClose),
            color = 'Black', size = 0.5)+
  # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the first axis
    name = "Euro/USD ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.-1.18, name="Residuals")
    
  )








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
residuals_naive <- as_data_frame(Forecast.df$Today_EUR_USDClose - lag_1$Today_EUR_USDClose)
residuals_naive$count <-  seq.int(nrow(residuals_naive))

plot(residuals_naive)

plot_naive <- data.frame(lag_1$Today_EUR_USDClose,
                                  Forecast.df$Today_EUR_USDClose, residuals_naive$value)
plot_naive$TimeSeries <- 1:nrow(lag_1)


(plot.valid_residuals <- ggplot(plot_naive, aes(x=TimeSeries)) + 
    geom_line( aes(y=lag_1.Today_EUR_USDClose,color="Actual"),)+
    geom_line( aes(y=Forecast.df.Today_EUR_USDClose, color = "Naive Forecast"), ) + 
    geom_line( aes(y=residuals_naive.value+1.16, color = "Residuals"),
            , size = 0.5, linetype = "dotted")+
     
    scale_color_manual(name = "Y series", values = c("Naive Forecast" = "blue", "Actual" = "black",
                                                     "Residuals"="red"))+
    geom_vline(xintercept = (max_length-offset), color = 'orange')+
    # Divide by 10 to get the same range than the temperature
    scale_y_continuous(
      # Features of the first axis
      name = "Euro/USD ",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.-1.16, name="Residuals")
      
    )+ 
    labs(title="Naive Forecast with Residuals")

)  
  

plot.valid_residuals


ggplot(residuals_naive)+
  geom_point(aes(x=count,y=value))












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
                      residuals.train2$Residuals_1+1.15, 
                      EuroForecast.df$Today_EUR_USDClose[startRow:endRow],
                      pred.valid2$Valid_1,
                      res.valid2$Valid_1+1.15
                      )
str(plot_all)
plot_all$TimeSeries <- 1:nrow(plot_all)






head(plot_all)

#now plot everything
everything.plot <- ggplot(plot_all, aes(x=TimeSeries)) +
  geom_line( aes(y=pred.train2.Train_1 , color = "Train Prediction"), linetype = "dashed")+
  geom_line(aes(y=residuals.train2.Residuals_1...1.15 , color='Train Residuals'), linetype = "dotted") + 
  geom_line( aes(y=EuroForecast.df.Today_EUR_USDClose.startRow.endRow.,color = 'Actual'), size = 0.5)+
  geom_line(aes(y=pred.valid2.Valid_1, color = "Valid Prediction"), linetype = "dashed")+
  geom_line( aes(y=res.valid2.Valid_1...1.15, color='Valid Residuals'), linetype = "dotted") +
  scale_color_manual(name = "Y series", values = c("Train Prediction" = "green", "Train Residuals" = "red",
                                                   "Actual"="black", "Valid Prediction"= "blue",
                                                   "Valid Residuals" = "purple"))+
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the first axis
    name = "Euro/USD ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.-1.15, name="Residuals"))+
  labs(title="Simple Multi-Variable Training & Validation Predictions with Residuals")
everything.plot






#this next section looks at plotting individually

#plots everything residuals, prediction actual for entire series
everything.plot <- ggplot(pred.train.df, aes(x=TimeSeries)) +
  geom_line(data=pred.train.df, aes(y=pred.train ), color = "Green", linetype = "dashed")+
  geom_line( data=pred.train.df,aes(y=residuals+1.2), color='Red', linetype = "dotted") + 
  geom_line(data = Forecast.df, aes(x=TimeSeries-startRow,y=Today_EUR_USDClose),
            color = 'Black', size = 0.5)+
  geom_line(data = pred.valid.df ,aes(y=pred.valid), color = "Blue", linetype = "dashed")+
  geom_line( data =pred.valid.df, aes(y=residuals+1.2), color='Red', linetype = "dotted") +
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the first axis
    name = "Euro/USD ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.-1.2, name="Residuals")
    
  )
everything.plot



#plot residuals against oil
ggplot(Forecast.df, aes(x=TimeSeries))+
  geom_line( data=pred.train.df,aes(y=residuals*600), color='DarkRed') +
  geom_line(data = Forecast.df ,aes(y=Prev_Brent_Crude_Spot), color = "Blue", linetype = "dashed")+
  geom_line( data =pred.valid.df, aes(y=residuals*600), color='DarkRed') +
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  scale_y_continuous(
  # Features of the first axis
  name = "Previous Day Brent Crude Spt ",
  # Add a second axis and specify its features
  sec.axis = sec_axis(~./600, name="Residuals")
  
)

#plot residuals against commodities
ggplot(Forecast.df, aes(x=TimeSeries))+
  geom_line( data=pred.train.df,aes(y=residuals*100), color='DarkRed') +
  geom_line(data = Forecast.df ,aes(y=Prev_Commodities.Basket.Close), color = "Blue", linetype = "dashed")+
  geom_line( data =pred.valid.df, aes(y=residuals*100), color='DarkRed') +
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  scale_y_continuous(
    # Features of the first axis
    name = "Previous Day Brent Crude Spt ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./100, name="Residuals")
    
  )

#plot residuals against S&P
ggplot(Forecast.df, aes(x=TimeSeries))+
  geom_line( data=pred.train.df,aes(y=residuals*20000), color='DarkRed') +
  geom_line(data = Forecast.df ,aes(y=Prev_S.P_500_Close), color = "Blue", linetype = "dashed")+
  geom_line( data =pred.valid.df, aes(y=residuals*20000), color='DarkRed') +
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  scale_y_continuous(
    # Features of the first axis
    name = "Previous Day Brent Crude Spt ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./20000, name="Residuals")
    
  )

?geom_line
#plot residuals against S&P
ggplot(Forecast.df, aes(x=TimeSeries))+
  geom_line( data=pred.train.df,aes(y=residuals*15000), color='DarkRed', linetype="dashed", show.legend =TRUE) +
  geom_line(data = Forecast.df ,aes(y=Prev_S.P_500_Close), color = "Blue", linetype = "dashed")+
  geom_line(data = Forecast.df ,aes(y=Prev_Commodities.Basket.Close*600), color = "Orange", linetype = "dashed")+
  geom_line(data = Forecast.df ,aes(y=Prev_Brent_Crude_Spot*100), color = "Green", linetype = "dashed")+
  geom_line(data = Forecast.df ,aes(y=Prev_European_Basket_Close), color = "DarkBlue", linetype = "dashed")+
  geom_line( data =pred.valid.df, aes(y=residuals*15000), color='DarkRed', linetype= "dashed") +
  geom_vline(xintercept = (max_length-offset), color = 'orange')+
  scale_y_continuous(
    # Features of the first axis
    name = "Previous Day Brent Crude Spt ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./15000, name="Residuals")
    
  )













            
