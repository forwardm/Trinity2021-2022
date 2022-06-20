library(dplyr)
library(sentimentr)
library(ggplot2)
install.packages("lubridate")
library(lubridate)

chipotle_price <- read.csv("CMG.csv")

chipotle_price

data <- read.csv("Combined_Tweets.csv")


chipotle_tweets <- data[!(data$Company.name=="Bookingcom" ),]

#------------------ create sentiment and make df--------------#

sentiment_analysis_chipotle <- sentiment(chipotle_tweets$full_text)

sentiment_analysis_chipotle <-as.data.frame(sentiment_analysis_chipotle)

#-------------sum by tweet multiple sentences-----------------#
sentiment_analysis_chipotle %>% 
  group_by(element_id) %>% 
  summarise(sentiment = sum(sentiment))

new_variable <- sentiment_analysis_chipotle %>% 
  group_by(element_id) %>% 
  summarise(sentiment = sum(sentiment))

chipotle_tweets$sentiment_sum <- new_variable$sentiment

#--------------sum by day--------------#
sum_by_day <- chipotle_tweets %>% 
  group_by(created_at) %>% 
  summarise(sentiment_sum = sum(sentiment_sum))

max(sum_by_day$sentiment_sum)

sum_by_day$created_at <- as.Date(sum_by_day$created_at, "%m/%d/%Y")


#----------------find weekends-------------#
day_of_week <- wday(sum_by_day$created_at, label = FALSE)

sum_by_day$dow <-day_of_week


#------------- add weekend sums to monday sums-------------#
i<-1
for ( i in 1:nrow(sum_by_day)){
  
  if(sum_by_day$dow[i] == 2){
    sum_by_day$sentiment_sum[i] <- sum_by_day$sentiment_sum[i]+
      sum_by_day$sentiment_sum[i-1]+
      sum_by_day$sentiment_sum[i-2]
  }
  
  i<-i+1
}

#----------- delete weekends dates -------------------#
sum_by_day<-sum_by_day[!(sum_by_day$dow==7 | sum_by_day$dow==1),]




#---------------- run regression compared -------------#
nrow(sum_by_day)
nrow(chipotle_price)

regression_df_chipotle <- as.data.frame(sum_by_day$created_at)
regression_df_chipotle$sentiment_score <-sum_by_day$sentiment_sum
regression_df_chipotle$stock_date <- chipotle_price$Date
regression_df_chipotle$close_price <- chipotle_price$Close

plot(close_price ~ sentiment_score, data = regression_df_chipotle)
cor(regression_df_chipotle$close_price, regression_df_chipotle$sentiment_score)

chipotle_regression <- lm(close_price ~ sentiment_score, data = regression_df_chipotle)

summary(chipotle_regression)

















#-------------------- do the same for bookingCom----------------#



chipotle_price <- read.csv("BKNG.csv")

chipotle_price

data <- read.csv("Combined_Tweets.csv")


chipotle_tweets <- data[!(data$Company.name=="Chipotle" ),]

#------------------ create sentiment and make df--------------#

sentiment_analysis_chipotle <- sentiment(chipotle_tweets$full_text)

sentiment_analysis_chipotle <-as.data.frame(sentiment_analysis_chipotle)

#-------------sum by tweet multiple sentences-----------------#
sentiment_analysis_chipotle %>% 
  group_by(element_id) %>% 
  summarise(sentiment = sum(sentiment))

new_variable <- sentiment_analysis_chipotle %>% 
  group_by(element_id) %>% 
  summarise(sentiment = sum(sentiment))

chipotle_tweets$sentiment_sum <- new_variable$sentiment

#--------------sum by day--------------#
sum_by_day <- chipotle_tweets %>% 
  group_by(created_at) %>% 
  summarise(sentiment_sum = sum(sentiment_sum))

max(sum_by_day$sentiment_sum)

sum_by_day$created_at <- as.dates(sum_by_day$created_at)


#----------------find weekends-------------#
day_of_week <- wday(sum_by_day$created_at, label = FALSE)

sum_by_day$dow <-day_of_week


#------------- add weekend sums to monday sums-------------#
i<-1
for ( i in 1:nrow(sum_by_day)){
  
  if(sum_by_day$dow[i] == 2){
    sum_by_day$sentiment_sum[i] <- sum_by_day$sentiment_sum[i]+
      sum_by_day$sentiment_sum[i-1]+
      sum_by_day$sentiment_sum[i-2]
  }
  
  i<-i+1
}

#----------- delete weekends dates -------------------#
sum_by_day<-sum_by_day[!(sum_by_day$dow==7 | sum_by_day$dow==1),]




#---------------- run regression compared -------------#
nrow(sum_by_day)
nrow(chipotle_price)

regression_df_chipotle <- as.data.frame(sum_by_day$created_at)
regression_df_chipotle$sentiment_score <-sum_by_day$sentiment_sum
regression_df_chipotle$stock_date <- chipotle_price$Date
regression_df_chipotle$close_price <- chipotle_price$Close

plot(close_price ~ sentiment_score, data = regression_df_chipotle)
cor(regression_df_chipotle$close_price, regression_df_chipotle$sentiment_score)

chipotle_regression <- lm(close_price ~ sentiment_score, data = regression_df_chipotle)

summary(chipotle_regression)







