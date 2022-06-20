library(aod)
library(ggplot2)
library('fastDummies')
library(dplyr)
install.packages("tidyverse")

library(tidyverse)

bankData.df <- read.csv("BankMarketingData.csv")

Data2.df <- bankData.df

#reorder months so show sequentially
bankData.df$month <-factor(bankData.df$month,levels=c("jan","feb","mar", "apr","may","jun","jul",
                              "aug","sep", "oct","nov",
                              "dec"),ordered=TRUE)



# change the followinng columns to 1/0 from yes/no
Data2.df$default<-ifelse(Data2.df$default=="yes",1,0)
Data2.df$housing<-ifelse(Data2.df$housing=="yes",1,0)
Data2.df$loan<-ifelse(Data2.df$loan=="yes",1,0)
Data2.df$y<-ifelse(Data2.df$y=="yes",1,0)


#create dummy variables for the following
Data2.df$poutcome <- factor(Data2.df$poutcome)
Data2.df$month <- factor(Data2.df$month)
Data2.df$contact <- factor(Data2.df$contact)
Data2.df$education <- factor(Data2.df$education)
Data2.df$marital <- factor(Data2.df$marital)
Data2.df$job <- factor(Data2.df$job)

Data2.df

#trying to normalize data first.
#library(normalr)
#normalize(Data2.df, method = "standardize", range = c(0, 1))

#create logistic regression model of eeverything
mylogit <- glm(formula = y ~ age + job + marital + education + default + balance + housing
               + loan + contact + day + month + duration + campaign
               + pdays + previous + poutcome,
               data = Data2.df, family = "binomial")
#get logistic regression summary
summary(mylogit)

#get confidence intervals
confint(mylogit)


#create logistic regression model without day, contact
mylogit2 <- glm(formula = y ~ age + job + marital + education + default + balance + housing
               + loan  + month + duration + campaign
               + pdays + previous + poutcome,
               data = Data2.df, family = "binomial")
#get logistic regression summary
summary(mylogit2)

#get confidence intervals
confint(mylogit2)






#create logistic regression model without day, contact
#we only want 25-60 for significance
freq_month_grp_no_dmso <- bankData.df[!(bankData.df$month=="mar" | bankData.df$month=="dec" | bankData.df$month=="oct" | bankData.df$month=="sep"),]

  #table(subset(bankData.df, month != 'mar' & month != 'dec' 
    #                                   & month != 'oct' & month != 'sep',
    #                                   select=c(y,month )))
# change the followinng columns to 1/0 from yes/no
freq_month_grp_no_dmso$default<-ifelse(freq_month_grp_no_dmso$default=="yes",1,0)
freq_month_grp_no_dmso$housing<-ifelse(freq_month_grp_no_dmso$housing=="yes",1,0)
freq_month_grp_no_dmso$loan<-ifelse(freq_month_grp_no_dmso$loan=="yes",1,0)
freq_month_grp_no_dmso$y<-ifelse(freq_month_grp_no_dmso$y=="yes",1,0)


mylogitMonth <- glm(formula = y ~ age + job + marital + education + default + balance + housing
                    + loan  + month + duration + campaign
                    + pdays + previous + poutcome,
                    data = freq_month_grp_no_dmso, family = "binomial")

summary(mylogitMonth)

mylogitMonth <- glm(formula = y ~ month,
                data = Data2.df, family = "binomial")
#get logistic regression summary for month
summary(mylogitMonth)

mylogitall <- glm(formula = y ~ .,
                  data = Data2.df, family = "binomial")

summary(mylogitall)





#create logistic regression model just about personal traits
mylogit3 <- glm(formula = y ~ age + job + marital + education 
             
                ,data = Data2.df, family = "binomial")
#get logistic regression summary
summary(mylogit3)

#get confidence intervals
confint(mylogit3)

install.packages('corrplot')
library(corrplot)
subset_personal <- bankData.df[,c('y','age', 'job','marital','education')]
factor(subset_personal)
correlations <- cor(subset_personal)
corrplot(correlations, method="circle")






# data analysis for job group to y
freq_job_grp <- table(bankData.df$y,bankData.df$job)
freq_job_grp
prop.table(freq_job_grp, 2)
prop.table(freq_job_grp, 1)

#shows counts
((ggplot(bankData.df, aes(job,fill=y)) +   geom_bar(stat = "count",   alpha = 0.75) +
    labs(x = "Job Type ", title = 'Y Outcome by Job Type') + 
    geom_text(stat='count', aes(label=..count..), vjust=-1, check_overlap = TRUE)  
))
  #+ facet_wrap('y'))

#plots proportion
tab_job = prop.table(freq_job_grp, margin=2)
tab_job = as.data.frame(tab_job)
ggplot(tab_job,aes(x=Var2,y=Freq,fill=Var1)) + geom_col() +
  labs(x = "Job Type ", title = 'Proportion Y Outcome by Job Type') +




barplot(freq_job_grp, main = "Count of Succesful Term Deposit by Job Status",
        xlab= ("Job Status"),
        ylab= ("Count"),
        legend = rownames(freq_job_grp))

#chi square test of y and job group:
chisq.test(Data2.df$y, Data2.df$job)








# data analysis for education group to y
freq_education_grp <- table(bankData.df$y, bankData.df$education)
freq_education_grp
prop.table(freq_education_grp, 2)
prop.table(freq_education_grp, 1)

#shows counts
((ggplot(bankData.df, aes(education,fill=y)) +   geom_bar(stat = "count",   alpha = 0.75) +
    labs(x = "Education Level ", title = 'Y Outcome by Education Level ') + 
    geom_text(stat='count', aes(label=..count..), vjust=-1, check_overlap = TRUE)  
)
  + facet_wrap('y'))

#plots proportion
tab_education = prop.table(freq_education_grp, margin=2)
tab_education = as.data.frame(tab_education)
ggplot(tab_education,aes(x=Var2,y=Freq,fill=Var1)) + geom_col()+ 
  labs(x = "Education Level ", title = 'YProportio Y Outcome by Education Level ') 

#plots counts yes/no
barplot(freq_education_grp, main = "Count of Succesful Term Deposit by Education Level",
        xlab= ("Education Level"),
        ylab= ("Count"),
        legend = rownames(freq_education_grp))











# data analysis for marital group to y
marital_grp <- as_tibble(bankData.df$y,bankData.df$marital)
freq_marital_grp <- table( bankData.df$y,bankData.df$marital)
freq_marital_grp
prop.table(freq_marital_grp, 2)
prop.table(freq_marital_grp, 1)

#shows counts
((ggplot(bankData.df, aes(marital,fill=y)) +   geom_bar(stat = "count",   alpha = 0.75) +
    labs(x = "Marital Status", title = 'Y Outcome by Marital Status') + 
    geom_text(stat='count', aes(label=..count..), vjust=-1, check_overlap = TRUE)  
)
  + facet_wrap('y'))

#plots proportions
tab_marital = prop.table(freq_marital_grp, margin=2)
tab_marital = as.data.frame(tab_marital)
ggplot(tab_marital,aes(x=Var2,y=Freq,fill=Var1)) + geom_col() + 
  labs(x = "Marital Status", title = 'Proportion Y Outcome by Marital Status')


#plots counts
barplot(freq_marital_grp, main = "Count of Succesful Term Deposit by Marital Status",
        xlab= ("Marital Status"),
        ylab = ("Count"),
        legend = rownames(freq_marital_grp))







#This code shows statistics and graphs for age

#summarize
summary(bankData.df$age)
#group by y and get summary for age
group_by(bankData.df, y) %>% 
  summarize(mean = mean(age),median= median(age), var = var(age), stdev = sqrt(var(age)) )

# data analysis for age group to y
freq_age_grp <- table( bankData.df$y,bankData.df$age)
prop.table(freq_age_grp, 2)
prop.table(freq_age_grp, 1)

#shows counts
((ggplot(bankData.df, aes(age,fill=y)) +   geom_bar(stat = "count",   alpha = 0.75) +
    labs(x = "Age", title = 'Y Outcome by Age ') + 
    geom_text(stat='count', aes(label=..count..), vjust=-1, check_overlap = TRUE)  
)
  )#+ facet_wrap('y'))


#plots proportion
tab_age = prop.table(freq_age_grp_25_60, margin=2)
tab_age = as.data.frame(tab_age)
ggplot(tab_age,aes(x=age,y=Freq,fill=y)) + geom_col()





barplot(freq_age_grp, main = "Count of Succesful Term Deposit by Age",
        xlab= ("Age"),
        ylab = ("Count"),
        legend = rownames(freq_age_grp))



#creating a second age group for proportion table where
#we only want 25-60 for significance as the others do not have enough data points
freq_age_grp_25_60 <- table(subset(bankData.df, age >= 25 & age <=60, 
                         select=c(y, age)))
#plots proportion
tab_age = prop.table(freq_age_grp_25_60, margin=2)
tab_age = as.data.frame(tab_age)
ggplot(tab_age,aes(x=age,y=Freq,fill=y)) + geom_col() + 
  labs(title = 'Proportion of Succesful Candidates by Age')











#we only want 25-60 for significance
freq_month_grp_no_dmso <- table(subset(bankData.df, month != 'mar' & month != 'dec' 
                                       & month != 'oct' & month != 'sep',
                                       select=c(y,month)))
tab_month_excl = prop.table(freq_month_grp_no_dmso, margin=2)
tab_month_excl = as.data.frame(tab_month_excl)
ggplot(tab_month_excl,aes(x=month,y=Freq,fill=y)) + geom_col()






# going on a journey to look at what the affect is of day on a sale.
library(tidyverse)
library(lubridate)

Data2.df$date <- paste('2021',Data2.df$mon, Data2.df$day, sep="-") %>% ymd() %>% as.Date()

ggplot(Data2.df, aes(date,y )) + (stat="bin", aes(group=y)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%B") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

milk <-ggplot(Data2.df, aes(format(date, "%m-%d"))) +
  geom_bar(stat = "count") +
  labs(x = "Month")











# this section is for Q1
#first need to partition data into yes and no y outcomes then we can see frequency distribution
OnlyYes <- Data2.df %>% 
  filter(y %in% c(1)) 
count(OnlyYes)

                
OnlyNo <- Data2.df %>% 
  filter(y %in% c(0)) 
count(OnlyNo)



mean(OnlyYes$day)
mean(OnlyNo$day)

#this package allows for overlap
install.packages('ggrepel')
library(ggrepel)


#mostly just checking counts by type, individual and combined
ggplot(OnlyYes, aes(month), colour = 'red') +   geom_bar(stat = "count", fill= 'blue') +
  labs(x = "Month", y= "Count   'Yes'") + geom_text(stat='count', aes(label=..count..), vjust=-1, colour = 'red')
ggplot(OnlyNo, aes(month), ) +   geom_bar(stat = "count", fill= 'green') +
  labs(x = "Month",  y= "Count   'No'") + geom_text(stat='count', aes(label=..count..), vjust=-1, colour= 'red')
ggplot(bankData.df, aes(month,fill=y)) +   geom_bar(stat = "count",   alpha = 0.75) +
  labs(x = "Month", title = 'Y Outcome by Month') + geom_text(stat='count', aes(label=..count..), vjust=-1, check_overlap = TRUE)  
?geom_text


((ggplot(bankData.df, aes(month,fill=y)) +   geom_bar(stat = "count",   alpha = 0.75) +
    labs(x = "Month", title = 'Y Outcome by Month') + geom_text(stat='count', aes(label=..count..), vjust=-1, check_overlap = TRUE)  
  )
+ facet_wrap('y'))



# proportion analysis for month group compared y
freq_month_grp <- table(bankData.df$y,bankData.df$month)
#this checks how often the % of yes/no for each month
prop.table(freq_month_grp, 2)
#this checks how often the % of yes/no  over the whole year by month
prop.table(freq_month_grp, 1)


#plots proportion
tab_month = prop.table(freq_month_grp, margin=2)
tab_month = as.data.frame(tab_month)
ggplot(tab_month,aes(x=Var2,y=Freq,fill=Var1)) + geom_col() +
  labs(x='Month', Y='Proportion YesNo', title = "Proportion of Succesful Term Deposit by Month")

barplot(freq_month_grp, main = "Proportion of Succesful Term Deposit by Month",
        xlab= ("Month"),
        ylab = ("Count"),
        legend = rownames(freq_month_grp),
)
?barplot

#now want to run a logistic regression model, but first need to factor categorical
# and change y to 1/0
Data2.df$y<-ifelse(Data2.df$y=="yes",1,0)
Data2.df$month <- factor(Data2.df$month)
#run regression only on y = month + constant, binomial for logistic
mylogitall <- glm(formula = y ~ month,
                  data = Data2.df, family = "binomial")
summary(mylogitall)


#want to look into what percent of data exists.
install.packages("janitor")
library(janitor)
bankData.df %>%
  tabyl(month) %>%
  adorn_pct_formatting()


#getrid of march aprl/oct/dec/na
NotBadMonth <- Data2.df %>% 
  filter(!month %in% c("mar","oct","sep","dec")) 

#run regression only on y = month + constant, binomial for logistic
# exclude mar/oct/sep/dec
mylogitall <- glm(formula = y ~ month,
                  data = NotBadMonth, family = "binomial")
summary(mylogitall)


#hypothesis that the y and month are independent
chisq.test(Data2.df$month, Data2.df$y)











?cbind




cor(freq_month_grp)

full.model <- glm(y ~ month, data = Data2.df, family = "binomial")
# Stepwise regression model
library(MASS)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


library(tidyverse)
library(caret)
library(leaps)
models <- regsubsets(y~., data = Data2.df, nvmax = 5,
                     method = "seqrep")
summary(models)

  
  


