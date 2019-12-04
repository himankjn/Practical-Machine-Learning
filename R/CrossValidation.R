
#cross validation is of 4 types. bootstrapping,kfold,repeated kfold,leave one out
library(caret)
data(iris)
partition<-createDataPartition(y=iris$Species,p=0.75,list=F)
train<-iris[partition,]
test<-iris[-partition,]

model1<-train(data=train,Species~.,method='rpart')
confusionMatrix(predict(model1,test),test$Species)
model1


##train control is used for crossvalidation setup.
#train control:
#method= boot,boot632,cv,repeatedcv,LOOCV
#number: no. of subsamples.
#repeats: number of times to repeat subsampling


##default method is boot with 25 reps.
#bootstrapping resampling
train_control<-trainControl(method='boot',number=100)
model2<-train(Species~.,data=iris,trControl=train_control,method='rpart')
model2

#kfold cross validation
train_control<-trainControl(method='cv',number=10)
# train the model
model3<-train(Species~.,data=iris,trControl=train_control,method='rpart')
model3


#repeated kfold cross validation
train_control<-trainControl(method='repeatedcv',number=10,repeats=3)
model4<-train(Species~.,data=iris,trControl=train_control,method='rpart')
model4

#leave one out cross validation
train_control<-trainControl(method='LOOCV')
model5<-train(Species~.,data=iris,trControl=train_control,method='rpart')
model5


