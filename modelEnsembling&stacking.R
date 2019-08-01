#REGULARIZED REGRESSION:
#The train error always reduces with increased number of predictors but the test error
#decreases first and then start increasing again. this is due to overfitting.
#we use methods like PCA , regularized regression to decrease dimensions by removing correlated and unnecessary predictors
#in regularized regression we penalize/shrink the coefficients which are large.
#lambda is tuning parameter proportional to penalization of coefficients.



#Ensembling is combining classifiers/algorithms to improve accuracy. it reduces interpretability.
#RF,BOOSTING,BAGGING are also themes of ensembling.


#Model stacking uses output of initial models as input to combined model
library(ISLR)
data(Wage)
library(caret)
partition<-createDataPartition(y=Wage$wage,p=0.7,list=F)
validation<-Wage[-partition,]
builddata<-Wage[partition,]
partition<-createDataPartition(y=builddata$wage,p=0.7,list=F)
training<-builddata[partition,]
testing<-builddata[-partition,]
model1<-train(wage~.,method='glm',data=training)
model2<-train(wage~.,method='rf',data=training,trControl=trainControl(method='cv'),number=3)
pred1<-predict(model1,testing)
pred2<-predict(model2,testing)
qplot(pred1,pred2,colour=wage,data=testing)
predF<-data.frame(pred1,pred2,wage=testing$wage)
        
combmodel<-train(wage~.,method='gam',data=predF)
combpred<-predict(combmodel,predF)
RMSE(pred1,testing$wage)
RMSE(pred2,testing$wage)
RMSE(combpred,testing$wage)
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combpred-testing$wage)^2))
#combined predictors model reduces sum of squared errors by alot
pred1v<-predict(model1,validation)
pred2v<-predict(model2,validation)
predvdf<-data.frame(pred1=pred1v,pred2=pred2v)
combpredv<-predict(combmodel,predvdf)

