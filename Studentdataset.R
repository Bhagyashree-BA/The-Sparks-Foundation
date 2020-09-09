# Importing Student dataset into.csv file and Check the structure of the dataset
str(Student)
summary(Student)
# Creating a model for predicting the Students Scores w.r.t. No of Hours
# For Regression model importing CaTools library
library(caTools)
set.seed(2)
# Splitting data into Train and Test dataset
split=sample.split(Student,SplitRatio = 0.7)
train=subset(Student,split="True")
test=subset(Student,split="False")
# Creating Regression model (y=Scores,x=Hours)
model=lm(Scores~Hours,data = train)
#Checking Accuracy of the model
summary(model)
# Checking the predicted value
predict_result=predict(model,test)
#Checking Actual Score and predicted score W.r.t. Hours
result=cbind(test,predict_result)
#Plotting scatter data of Scores and Hours
library(ggplot2)
y=Student$Scores
x=Student$Hours
plot(x,y,ylab = "Scores",xlab = "No.of Hours",main = "Scores vs Hours")
# Plotting best fit line
abline(lm(y~x))
# Calculating Sum of Error of Square
SSE=sum((test$Scores-predict_result)^2)
SSE
#calculating Total sum of square of Error
SST=sum((test$Scores-mean(Student$Scores))^2)
SST
#calculating RSS value
r=1-(SSE/SST)
View(r)