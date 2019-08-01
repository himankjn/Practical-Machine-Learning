library(caret)
library(ISLR)
library(ggplot2)
data("Wage")
summary(Wage)
        
partition<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[partition,]
testing<-Wage[-partition,]
pairs(wage~age+education+jobclass,data=Wage)
qplot(age,wage,data=Wage,colour=jobclass)
qplot(age,wage,data=Wage,colour=education)

model<-train(wage~age+jobclass+education,method='lm',data=training)
mod<-model$finalModel
par(mfrow=c(1,1))
plot(mod,1)
qplot(mod$fitted,mod$residuals,color=race,data=training)

plot(mod$residuals)

pred<-predict(model,testing)
qplot(wage,pred,colour=year,data=testing)
