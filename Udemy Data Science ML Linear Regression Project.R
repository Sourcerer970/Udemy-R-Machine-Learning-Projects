setwd("C:/Users/Preetom/Desktop/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects")
bike <- read.csv('bikeshare.csv')
head(bike)

#We're trying to predict the total count of bikes rented during each hour covered by the test set using only the information available priorto the rental period
library(ggplot2)
library(ggthemes)
library(dplyr)
ggplot(data = bike, aes(x = temp, y = count)) + geom_point(aes(color = temp), alpha = 0.2) + theme_bw()

bike$datetime <-as.POSIXct(bike$datetime)
head(bike)
ggplot(data = bike, aes(x = datetime, y = count)) + geom_point(aes(color = temp), alpha = 0.5) + scale_color_gradient(low = 'light green', high = 'orange') +theme_bw() 

cor(bike[, c('temp', 'count')])
ggplot(bike, aes(x = factor(season), y = count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()

bike$hour <- sapply(bike$datetime, function(x){format(x, "%H")})
head(bike)

pl <- ggplot(filter(bike, workingday == 1), aes(x = hour, y = count)) 
pl2 <- pl + geom_point(aes(color = temp), alpha = 0.5, position = position_jitter(w = 1, h = 0))
pl3 <- pl2 + scale_color_gradientn(colors = c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))
pl4 <- pl3 + theme_bw()
pl4

gl <- ggplot(filter(bike, workingday == 0), aes(x = hour, y = count))
gl2 <- gl + geom_point(aes(color = temp), alpha = 0.5, position = position_jitter(w = 1, h = 0))
gl3 <- gl2 + scale_color_gradientn(colors = c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))
gl4 <- gl3 + theme_bw()
gl4

temp.model <- lm(count ~ temp, bike)
summary(temp.model)

6.0462 + 9.1705*25
temp.test <- data.frame(temp = c(25))
predict(temp.model, temp.test)

bike$hour <- sapply(bike$hour, as.numeric)
head(bike)

model <- lm(count ~ . - atemp - casual - registered - datetime, bike)
summary(model)
