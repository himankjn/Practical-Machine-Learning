library(caret)
set.seed(333)
data("faithful")
partition<-createDataPartition(y=faithful$waiting,p=0.5,list=F)
training<-faithful[partition,]
testing<-faithful[-partition,]
head(training)
plot(training$waiting,training$eruptions,pch=19,col='blue')
## almost linear trend
lm1<-lm(eruptions~waiting,data=training)
lm1
plot(training$waiting,training$eruptions,pch=19,col='blue')
lines(training$waiting,lm1$fitted,lwd=3)

coef(lm1)[1]+coef(lm1)[2]*80
predict(lm1,data.frame(waiting=80))


par(mfrow=c(1,2))
plot(training$waiting,training$eruptions,pch=19,col='blue')
lines(training$waiting,lm1$fitted,lwd=3)
plot(testing$waiting,testing$eruptions,pch=19,col='blue')
lines(testing$waiting,predict(lm1,newdata=testing),lwd=3)


##rmse of test set
RMSE(testing$eruptions,predict(lm1,testing))

###doing same linear model in caret.
model<-train(eruptions~waiting,data=training,method='lm')
summary(model$finalModel)


