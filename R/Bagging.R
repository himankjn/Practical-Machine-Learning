#refer: https://www.coursera.org/learn/practical-machine-learning/lecture/6V3fC/bagging
#Bagging or BootStrap aggregating.
##useful for non linear functions.
##resample cases and recalculate predictions and then avg or majority vote them .
##it reduces variance.
##removes overfitting of models

#Bagged Loess
install.packages("ElemStatLearn")
library(ElemStatLearn)
data("ozone")
ozone<-ozone[order(ozone$ozone),]
head(ozone)
ll<-matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
        ss<-sample(1:nrow(ozone),replace=T)
        ozone0<-ozone[ss,];
        ozone0<-ozone0[order(ozone$ozone),]
        loess0<-loess(temperature~ozone,data=ozone0,span=0.2)
        ll[i,]<-predict(loess0,newdata=data.frame(ozone=1:155))
}
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col='grey',lwd=2)}
lines(1:155,apply(ll,2,mean),col='red',lwd=2)



#bagging can be done in train function with method parameter or using bag function.
library(caret)
train<-(method=bagEarth/treebag/bagFDA)
##we can use bag function to manually bag any model.        


##tree bagging
data(iris)
library(ggplot2)
table(iris$Species)
partition<-createDataPartition(y=iris$Species,p=0.75,list=F)
training<-iris[partition,]
testing<-iris[-partition,]
dim(training)
qplot(data=training,Petal.Width,Petal.Length,col=Species)
qplot(data=training,Petal.Width,Sepal.Width,col=Species)
#model without bagging
model<-train(Species~.,method='rpart',data=training)
predict(model,newdata=testing)
confusionMatrix(predict(model,newdata=testing),testing$Species)
#model with bagging
model<-train(Species~.,method='treebag',data=training)
confusionMatrix(predict(model,newdata=testing),testing$Species)
##only first tree from bagged model.
fancyRpartPlot(model$finalModel$mtrees[[1]]$btree)
