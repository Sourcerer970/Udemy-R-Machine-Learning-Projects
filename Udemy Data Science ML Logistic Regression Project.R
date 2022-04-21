setwd('C:/Users/Preetom/Desktop/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects')
adult <- read.csv('adult_sal.csv')
head(adult)

library(dplyr)
adult <- select(adult, - X)
head(adult)
str(adult)
summary(adult)

#Data Cleaning#
table(adult$type_employer)
#There are 1836 null values. The two smallest groups are never-worked and without-pay.

unemp <- function(job) {
  job <- as.character(job) 
  if(job == 'Never-worked' | job == 'Without-pay') {
    return('Unemployed')
  } else {
    return(job)
  }
}

adult$type_employer <-sapply(adult$type_employer, unemp)
table(adult$type_employer)

group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, group_emp)
table(adult$type_employer)

table(adult$marital)

group_marital <- function(mar) {
  mar <- as.character(mar)
  
  #Not Married
  if (mar == 'Divorced' | mar == 'Separated' | mar == 'Widowed') {
    return('Not Married')
    
    #Never-Married
  }else if (mar == 'Never-married') {
    return('Never-Married')
    
    #Married
  }else {
    return('Married')
  }
}

adult$marital <- sapply(adult$marital, group_marital)
table(adult$marital)

table(adult$country)
levels(adult$country)
install.packages('ggplot2')
install.packages('ggthemes')
library(ggplot2)
library(ggthemes)
install.packages('Amelia')
library(Amelia)
install.packages('caTools')
library(caTools)

Asian <- c('Laos', 'Philippines', 'Vietnam', 'Cambodia', 'Taiwan', 'Hong', 'Thailand', 'China', 'India', 'Japan', 'Iran')
North.America <- c('Canada', 'Puerto-Rico', 'United-States')
Europe <- c('France', 'Holand-Netherlands', 'Germany', 'Ireland', 'Poland', 'Yugoslavia', 'Greece', 'Italy', 'Portugal', 'Hungary', 'England', 'Scotland')
Latin.and.South.America <- c('Cuba', 'Dominican-Republic', 'Honduras', 'Mexico', 'Ecuador', 'Nicaragua', 'El-Salvador', 'Guatemala', 'Jamaica', 'Outlying-US(Guam-USVI-etc)', 'Trinadad&Tobago', 'Columbia', 'Haiti', 'Peru')
Other <- c('South')

group_country <- function(cntry) {
  cntry <- as.character(cntry)
  if(cntry %in% Asian) {
    return('Asian')
  } else if(cntry %in% North.America) {
    return('North.America')
  } else if(cntry %in% Europe) {
    return('Europe')
  } else if(cntry %in% Latin.and.South.America) {
    return('Latin.and.South.America')
  } else {
    return('Other')
  }
}
adult$country <- sapply(adult$country, group_country)
table(adult$country)
str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

str(adult)

adult[adult == '?'] <- NA
table(adult$type_employer)

missmap(adult, main = 'Missingness Map', col = c('yellow', 'red'), legend = TRUE)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

help("na.omit")
adult <- na.omit(adult)
str(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#Exploratory Data Analysis# 
str(adult)
ggplot(adult, aes(age)) + geom_histogram(aes(fill = factor(income)), binwidth = 1, color = 'black') + theme_bw()
ggplot(adult, aes(hr_per_week)) + geom_histogram(color = 'black') + theme_bw() 

names(adult)[names(adult) == 'country'] <- 'region'
str(adult)

ggplot(adult, aes(region)) + geom_bar(aes(fill=factor(income)), color= 'black') + theme_bw()

#Building a Model#
head(adult)

#Train Test Split
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.70)

#Training Data
train = subset(adult, sample == TRUE)

#Testing Data 
test = subset(adult, sample == FALSE)

#Training the Model#
model <- glm(as.factor(income) ~. ,family=binomial(logit),data = train)
summary(model)

help("step")
new.model <- step(model)
summary(new.model)

test$predicted.income <- predict(model, newdata = test, type = 'response' )
table(test$income, test$predicted.income > 0.5)

#accuracy
(6372+1423)/(6372+1423+548+872)

#recall
6732/(6372+548)

#precision
6732/(6372+872)
