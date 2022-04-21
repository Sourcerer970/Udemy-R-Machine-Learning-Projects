library(ISLR)
str(iris)
head(iris)

#Standardizing the Data
stand.features <- scale(iris[1:4])
var(stand.features[,1])
var(stand.features[,2])

final.data <- cbind(stand.features, iris[5])
head(final.data)

#Train Test Split
set.seed(101)
library(caTools)
sample.data <- sample.split(final.data$Species, SplitRatio = 0.70)
train.data <- subset(final.data, sample.data == TRUE)
test.data <- subset(final.data, sample.data == FALSE)

#KNN Model
library(class)
set.seed(101)
predicted.Species <- knn(train.data[1:4], test.data[1:4], train.data$Species, k = 1)
head(predicted.Species)
predicted.Species

misclass.error <- mean(test.data$Species != predicted.Species)
misclass.error

#Choosing K Values
predicted.Species <- NULL
error.rate <- NULL

for (i in 1:10) {
  set.seed(101)
  predicted.Species <- knn(train.data[1:4], test.data[1:4], train.data$Species, k = i)
  error.rate[i] <- mean(test.data$Species != predicted.Species)
}
error.rate

library(ggplot2)
k.values<- 1:10
error.df <- data.frame(error.rate, k.values)
error.df
ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty = 'dotted', color = 'red')
