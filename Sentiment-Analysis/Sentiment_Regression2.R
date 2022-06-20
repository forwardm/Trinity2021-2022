library(dplyr)
library(lubridate)
library(corrplot)

data <- read.csv("Final_bc_1.csv")
head(data)


chipotle_price <- read.csv("CMG.csv")

chipotle_price

chipotle_tweets <- data[!(data$company=="bookingcom" ),]

nrow(chipotle_price)
nrow(chipotle_tweets)




#----------------find weekends-------------#
day_of_week <- wday(chipotle_tweets$created_at, label = FALSE)

chipotle_tweets$dow <-day_of_week

#----------------delete weekends-------------#

chipotle_tweets<-subset(chipotle_tweets, dow != 1)
chipotle_tweets<-subset(chipotle_tweets, dow != 7)

chipotle_tweets$price <- chipotle_price$Close


data_regression <- subset(chipotle_tweets, select = -c(1,2,26) )


#-----------make a lag------------#
regression_1 <- lead(data_regression)

dim(data_regression)
nrow(data_regression)

data_regression$price <- regression_1$price
#data_regression <- data_regression[-1,]
data_regression <- data_regression[-nrow(data_regression),]

#------------regression------------------#
length(data_regression)
nrow(data_regression)

plot(data_regression$Mean_trust, data_regression$price)
data_regression$price

M= cor(data_regression)
M
z<-data_frame(y[,13])
corrplot(M, method = 'circle') # colorful number



chipotle_regression <- lm(price ~Mean_surprise+Mean_trust+Mean_joy+Mean_sadness+
                             Mean_anger+Sum_anger+Sum_fear+Mean_negative+Mean_anticipation
                           , data = data_regression)
summary(chipotle_regression)




#-------------causality--------------#
library(lmtest)
?grangertest
output <- grangertest( price~Mean_trust , data=data_regression)
output #trust
output <- grangertest( price~Mean_surprise , data=data_regression)
output #surprise
output <- grangertest( price~Mean_sadness, data=data_regression)
output #sadness
output <- grangertest( price~Mean_joy , data=data_regression)
output #joy
output <- grangertest( price~Mean_anger , data=data_regression)
output #anger
output <- grangertest( price~Sum_anger , data=data_regression)
output #sumanger
output <- grangertest( price~Sum_fear, data=data_regression)
output #sumfear
output <- grangertest( price~Mean_negative , data=data_regression)
output #mean_negative
output <- grangertest( price~Mean_anticipation , data=data_regression)
output #mean_anticipation
grangertest()



library(Hmisc)
coeffs <- rcorr(as.matrix(data_regression))
coeffs$r

new<- coeffs$r
new[,nrow(new)]






#---------------price fluctuations-----------#
data_regression <-chipotle_tweets
data_regression <- subset(chipotle_tweets, select = -c(1,26) )

data_regression$price <- lead(regression_1$price)-regression_1$price



chipotle_fluc_regression <- data_regression

chipotle_fluc_regression <- chipotle_fluc_regression[-1,]
chipotle_fluc_regression <- chipotle_fluc_regression[-nrow
                                                     (chipotle_fluc_regression),]
?cov
nrow(chipotle_fluc_regression)
cov(chipotle_fluc_regression$price, chipotle_fluc_regression$Sum_fear)
cov(chipotle_fluc_regression$price, chipotle_fluc_regression$Sum_fear)

Mone<- cor(pricefluctations)
corrplot(Mone)

regerssion_price <- lm(price~Mean_anger+Mean_surprise+Sum_positive+Mean_fear,data=chipotle_fluc_regression)
summary(regerssion_price)


regerssion_price <- lm(price~Mean_anger+Mean_surprise+Sum_positive+Mean_fear+Sum_disgust,data=chipotle_fluc_regression)
summary(regerssion_price)



data_regression <-chipotle_tweets
data_regression <- subset(chipotle_tweets, select = -c(1,26) )

data_regression$price <- lead(regression_1$price)-regression_1$price

output <- grangertest( price~Mean_anticipation
                       , data=chipotle_fluc_regression, order=1)
output
?grangertest
