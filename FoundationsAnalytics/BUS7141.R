library(corrplot)
library(Amelia)
library(purrr)
library(caret)
library(MASS)
library(leaps)

social_data <- read.csv("Project_data_Twitter_facebook_raw.csv")

summary(social_data)



#---------------------- Data Preprocessing-----------------------#
#need to turn our data into numeric.
social_data$NUM_OF_EMPLOYEES <- as.numeric(social_data$NUM_OF_EMPLOYEES)
social_data$HISTORICAL_MARKET_CAP <- as.numeric(social_data$HISTORICAL_MARKET_CAP)
social_data$ESG_DISCLOSURE_SCORE <- as.numeric(social_data$ESG_DISCLOSURE_SCORE)
social_data$GOVNCE_DISCLOSURE_SCORE <- as.numeric(social_data$GOVNCE_DISCLOSURE_SCORE)
social_data$ENVIRON_DISCLOSURE_SCORE <- as.numeric(social_data$ENVIRON_DISCLOSURE_SCORE)
social_data$SOCIAL_DISCLOSURE_SCORE <- as.numeric(social_data$SOCIAL_DISCLOSURE_SCORE)
social_data$TOT_GHG_CO2_EM_INTENS_PER_SALES <- as.numeric(social_data$TOT_GHG_CO2_EM_INTENS_PER_SALES)
social_data$PCT_WOMEN_EMPLOYEES <- as.numeric(social_data$PCT_WOMEN_EMPLOYEES)
social_data$PCT_WOMEN_ON_BOARD <- as.numeric(social_data$PCT_WOMEN_ON_BOARD)
social_data$ROBECOSAM_TOTAL_STBLY_RANK <- as.numeric(social_data$ROBECOSAM_TOTAL_STBLY_RANK)
social_data$ROBECOSAM_ENV_DIMENSION_RANK <- as.numeric(social_data$ROBECOSAM_ENV_DIMENSION_RANK)
social_data$ROBECOSAM_ECON_DIMENSION_RANK <- as.numeric(social_data$ROBECOSAM_ECON_DIMENSION_RANK)
social_data$ROBECOSAM_SOCIAL_DIMENSION_RANK <- as.numeric(social_data$ROBECOSAM_SOCIAL_DIMENSION_RANK)


# -------------------------- NA Imputation with Amelia------------#
#make boundaries for data for amelia, ex. a rank cant be below 0 or above 100
# the column is (column number of variable, min bound, max bound) or just go ?amelia -> bounds
num_emp_bounds <- c(4,0,1000000) 
mkt_cap_bounds <- c(8,0,1000000)
esg_scr_bounds <- c(9,0,100)
gov_scr_bounds <- c(10,0,100)
env_scr_bounds <- c(11,0,100)
soc_scr_bounds <- c(12,0,100)
c02_emit_bounds <- c(13,0,3000)
num_wom_emp_bounds <- c(14,0,100)
pct_wmn_brd_bounds <- c(15,0,100)
env_bounds<- c(18,0,100)
tot_bounds<- c(16,0,100)
econ_bounds <- c(19,0,100)
soc_bounds <- c(17,0,100)
amelia_bounds <- rbind(num_emp_bounds,mkt_cap_bounds,esg_scr_bounds,gov_scr_bounds,
                       env_scr_bounds,soc_scr_bounds,c02_emit_bounds,num_wom_emp_bounds,
                       pct_wmn_brd_bounds,env_bounds,tot_bounds,econ_bounds,soc_bounds)

#run the amelia model , get the summary and compare what the distributions are like to original, red=predict, blue = real
#idvars gets rid of ID variables for imputation, bounds sets the boundaries of certain variables
a.out <- amelia(x = social_data, idvars = c("Twitter_Username","ID","Company"), bounds =amelia_bounds  )
summary(a.out) 
plot(a.out)

#the 5 different imputations due to amelia
a.out_1 <- data.frame(a.out$imputations[1])
a.out_2 <-data.frame(a.out$imputations[2])
a.out_3 <-data.frame(a.out$imputations[3])
a.out_4 <-data.frame(a.out$imputations[4])
a.out_5 <-data.frame(a.out$imputations[5])

summary(a.out$imputations[1])



#now lets look at correlations on our data:
#choose whicever imputation you want, in this scenario chose 5th
social_data_new <- data.frame(a.out_5)
summary(social_data_new)
str(social_data_new)
correlation_matrix <- data.frame(social_data_new[,-c(1,2,20)])

plot.new()
corrplot(cor(correlation_matrix),is.corr = TRUE,method=c("circle"),tl.cex=0.4)
?cor







#---------------------------CLustering--------------------------#
#now lets run clustering to see what our unsupervised models do and classify x number of groups
# two scenarios to look into:
#    1. lets look at mkt cap & ESG rank
#    2. lets look at ESG rank and ESG disclosure
# have to edit some inputs in the three lines of code below to get it right but
# can adjust and increase number of variables.
str(correlation_matrix)
maxs <- apply(correlation_matrix[,c(7,14)], 2, max) 
mins <- apply(correlation_matrix[,c(7,14)], 2, min)
scaled_data<- as.data.frame(scale(correlation_matrix[,c(7,14)], center = mins, 
                                      scale = maxs - mins))
summary(scaled_data)


wss <- function(k) {
  kmeans(scaled_data, k )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Determining Clusters for Total ESG Rank & Market Cap")


# we select whatever 'elbow' says this could be changed to whatever the graph says
n_clusters <- 3
km<- kmeans(scaled_data,n_clusters)
km


#if doing a 2d cluster then we can plot data points
# if not doing a 2d cluster ignore this part.
clustered_data <-  data.frame(cbind(correlation_matrix$imp5.ESG_DISCLOSURE_SCORE,
                                    correlation_matrix$imp5.ROBECOSAM_TOTAL_STBLY_RANK, km$cluster))
clustered_data$X3 <- factor(clustered_data$X3)
ggplot(clustered_data, aes(x=X1, y=X2,  color=X3)) +
  xlab("ESG Disclosure Score")+
  ylab("Total ESG Rank")+
  labs(color = "Clusters")+
  ggtitle("ESG Disclosure Score vs ESG Rank")+
  geom_point()


#this section just shows the differences in the groups based off of which variable
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(1, 2))
# label x-axes
axis(1, at = c(1:2), labels = names(scaled_ESG_Finances))


# plot centroids
for (i in c(1:2))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 2, 3),
                                                       "black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:2)))

clusters <- km$cluster

#combining cluster results with original dataset ----
data_new <- cbind(correlation_matrix,clusters)
data_new$clusters <- factor(data_new$clusters)



cluster1_data <- subset(data_new,data_new$clusters==1)
cluster2_data <- subset(data_new,data_new$clusters==2)
cluster3_data <- subset(data_new,data_new$clusters==3)

summary(cluster1_data)
summary(cluster2_data)
summary(cluster3_data)

data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.ROBECOSAM_TOTAL_STBLY_RANK))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.HISTORICAL_MARKET_CAP))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.FB.shares.per.relevant))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.Retweets))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.Likes))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.NET_INCOME))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.ROBECOSAM_TOTAL_STBLY_RANK))
data_new1
data_new1 <- data_new %>% group_by(clusters) %>% summarise(me = mean(imp5.ROBECOSAM_TOTAL_STBLY_RANK))
data_new1



ggplot(aes(x = clusters , y = imp5.HISTORICAL_MARKET_CAP), data = data_new) +
  stat_summary(fun = "mean", geom = "bar") 
ggplot(aes(x = clusters , y = imp5.FB.shares.per.relevant), data = data_new) +
  stat_summary(fun = "mean", geom = "bar")
ggplot(aes(x = clusters , y = imp5.Retweets), data = data_new) +
  stat_summary(fun = "mean", geom = "bar")
ggplot(aes(x = clusters , y = imp5.Likes), data = data_new) +
  stat_summary(fun = "mean", geom = "bar")

ggplot(aes(x = clusters , y = imp5.NET_INCOME), data = data_new) +
  stat_summary(fun = "mean", geom = "bar")

summary(cluster3_data)
#-------------------- Regression Models ---------------------#
#now lets run regressions for total rank


#   1. lets first look at regression and impact of social media on 
#      ESG total ranking

str(correlation_matrix)

# Look at total rank based off of social media interaction
esg_social_media_data <- correlation_matrix[,c(14,18:ncol(correlation_matrix))]
model_esg_SM <- lm(imp5.ROBECOSAM_TOTAL_STBLY_RANK~., data= esg_social_media_data) 
summary(model_esg_SM)

#look at social based off of social media interaction
esg_social_media_data <- correlation_matrix[,c(15,18:ncol(correlation_matrix))]
model_esg_SM <- lm(imp5.ROBECOSAM_SOCIAL_DIMENSION_RANK~., data= esg_social_media_data) 
summary(model_esg_SM)

#look at env based off of social media interaction
esg_social_media_data <- correlation_matrix[,c(16,18:ncol(correlation_matrix))]
model_esg_SM <- lm(imp5.ROBECOSAM_ENV_DIMENSION_RANK~., data= esg_social_media_data) 
summary(model_esg_SM)

#look at econ based off of social media interaction
esg_social_media_data <- correlation_matrix[,c(17,18:ncol(correlation_matrix))]
model_esg_SM <- lm(imp5.ROBECOSAM_ECON_DIMENSION_RANK~., data= esg_social_media_data) 
summary(model_esg_SM)

#look at econ based off of social media interaction

esg_social_media_data<- correlation_matrix
model_esg_SM <- lm(imp5.HISTORICAL_MARKET_CAP~., data= esg_social_media_data) 
summary(model_esg_SM)


esg_social_media_data<- correlation_matrix
model_esg_SM <- lm(imp5.SALES_REV_TURN~., data= esg_social_media_data) 
summary(model_esg_SM)




#run stepwise on total based off of social media interaction
esg_social_media_data <- correlation_matrix[,c(14,18:ncol(correlation_matrix))]
# Train the model

                    
step.model <- train(imp5.ROBECOSAM_TOTAL_STBLY_RANK~.,
                    data= esg_social_media_data,
                    method = "lmStepAIC", 
                    #trControl = train.control,
                    trace = FALSE
)
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)


models <- regsubsets(imp5.ROBECOSAM_TOTAL_STBLY_RANK~.,
                     data= esg_social_media_data,
                    
                     method = "forward")
summary(models)
models$rss
plot( 1:length(models$rss),models$rss)






#    2. Now lets look at the impact of variables on economics and impacts from social.


#---------------trying AMELIA shit--------------#
b.out <- NULL
se.out <- NULL
for(i in seq_len(a.out$m)) {
  ols.out <- lm(ROBECOSAM_TOTAL_STBLY_RANK ~ ., data = a.out$imputations[[i]])
  summary(ols.out)
  b.out <- rbind(b.out, ols.out$coef)
  se.out <- rbind(se.out, coef(summary(ols.out))[, 2])
}

combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
#---------------trying AMELIA shit--------------#




























































#-----------------------unsure if want to use didnt want to delete------------ #

library(ggplot2)

d <- density(na.omit(rank_without_na$NUM_OF_EMPLOYEES)) # returns the density data
plot(d)
ggplot(social_data, aes(x=NET_ASSETS, y=NET_INCOME))+geom_point()
plot(social_data$NET_ASSETS, social_data$NET_INCOME)

str(social_data)
a.out$overvalues
summary(a.out$overvalues)
length(imputed_data)
imputed_data <- a.out$imputations
imputed_data <- data.frame(imputed_data[,c(137:170)])
summary(imputed_data)



#look into and subset data based on wheter or not nas have data
na_rank <- social_data[social_data$ROBECOSAM_ENV_DIMENSION_RANK%in%NA,]
rank_without_na <- social_data[!(social_data$ROBECOSAM_ENV_DIMENSION_RANK%in%NA),]
summary(na_rank)
summary(rank_without_na)


#column 1 is just IDS, column 16 is tot, 17=social,18=env,19=econ, 20 twitter name)
#create models for each 
model_social_rank <- lm( ROBECOSAM_SOCIAL_DIMENSION_RANK~., data=rank_without_na[,-c(1,2,16,18,19,20)])
model_env_rank <- lm( ROBECOSAM_ENV_DIMENSION_RANK~., data=rank_without_na[,-c(1,2,16,17,19,20)])
model_econ_rank <- lm( ROBECOSAM_ECON_DIMENSION_RANK~., data=rank_without_na[,-c(1,2,16,17,18,20)])
model_tot_rank <- lm( ROBECOSAM_TOTAL_STBLY_RANK~., data=rank_without_na[,-c(1,2,17,18,19,20)])


imputate_soc <- predict(model_social_rank,na_rank[,-c(1,2,16,18,19,20)])
?predict
summary(model_social_rank)
summary(model_env_rank)
summary(model_econ_rank)
summary(model_tot_rank)

summary(social_data_numeric)
head(social_data)
str(social_data)
corrplot(na.omit(social_data_numeric),method= c("circle"), is.corr = FALSE)


#checking for missing values ----
social_data[rowSums(is.na(social_data)) > 0,] #no NA values
social_data[,c(20)]
social_data_numeric <- data.frame(social_data[,-c(1,2,20)])
str(social_data_numeric)

corrplot(social_data_numeric,method= c("circle"), is.corr = FALSE,)
round(cor(na.omit((social_data_numeric))),2)
?round
    
