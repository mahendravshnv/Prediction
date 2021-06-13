#Prediction using Supervised ML
#By Mahendra Kumar Vaishnav


#Load the required libraries
library(ggplot2)
library(readr)
library(caTools)

#Load the dataset 

studentdata <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(studentdata)
attach(studentdata)
summary(studentdata)

#Plot the Scatter Diagram betweeen HOurs vs Scores
plot(Hours,Scores, xlab = 'Study Hours', ylab = 'Scores in %age',main = 'Scatter Plot - Hours v/s %age ',lwd=2)

#plot shows a Positive Linear Regression, Now we go for Correlation between both the variables.
cor(Hours,Scores)

#AS we know correlation r=0.9761 is very strong correlation
set.seed(2)
split <- sample.split(studentdata,SplitRatio=0.8)
split
train <- subset(studentdata,split='TRUE')
test <- subset(studentdata,split='FALSE')
train
test

#Create Model
Model <- lm(Scores~Hours,data = train)
summary(Model)
#Training complete

#Now let's plot the regression line on the Scatter Plot
plot<-plot(Hours,Scores, xlab = 'Study Hours', ylab = 'Scores in %age',main = 'Scatter Plot - Hours v/s %age', col="blue",lwd=2)
abline(Model,lwd =2,col = 'red')

#Let's Predict data for testing data
predict <- predict(Model,test)
predict

#Compare actual and predicted
df <-data.frame(Actual=Scores,predicted=predict)
DT::datatable(df)

#Now let's make prediction for given value of Hours=9.25
data <- data.frame(Hours=c(9.25))
predict(Model,newdata= data)
#Hence the predicted score is "92.90985"

