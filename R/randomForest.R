#refer: https://www.coursera.org/learn/practical-machine-learning/lecture/XKsl6/random-forests
#create multiple trees with bootstraped resamples and at every split resample the variables.
#highly accurate for competetions.


library(caret)
library(ggplot2)
partition<-createDataPartition(y=iris$Species,p=0.7,list=F)
training<-iris[partition,]
testing<-iris[-partition,]
model<-train(Species~.,data=training,method='rf',prox=T)
model
library(randomForest)
#get second tree of model.
getTree(model$finalModel,k=2)
pred<-predict(model,testing)
confusionMatrix(testing$Species,pred)

irisP<-classCenter(training[,c(3,4)],training$Species,model$finalModel$prox)
irisP
irisP<-as.data.frame(irisP);irisP$Species<-unique(training$Species)
p<-qplot(Petal.Width,Petal.Length,col=Species,data=training)
p+geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

             

##plotting missed values
predright<-pred==testing$Species
qplot(Petal.Width,Petal.Length,colour=predright,data=testing)
