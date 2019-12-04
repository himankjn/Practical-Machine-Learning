
#refer: https://www.coursera.org/learn/practical-machine-learning/lecture/9mGzA/boosting
##boosting
##use weak classifiers(models) and weigh them and add them up to get strong classifiers.
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage<-subset(Wage,select=-c(logwage))
partition<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[partition,]
testing<-Wage[-partition,]
#gbm uses boosting with trees
#can use gbm/mboost/ada/gamBoost
model<-train(wage~.,method="gbm",data=training,verbose=F)
model
qplot(predict(model,testing),wage,data=testing)
