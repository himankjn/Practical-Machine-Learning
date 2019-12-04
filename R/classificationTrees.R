data(iris)
library(ggplot2)
table(iris$Species)
partition<-createDataPartition(y=iris$Species,p=0.75,list=F)
training<-iris[partition,]
testing<-iris[-partition,]
dim(training)
qplot(data=training,Petal.Width,Petal.Length,col=Species)
   qplot(data=training,Petal.Width,Sepal.Width,col=Species)

library(caret)
   #modelling with classification tree
model<-train(Species~.,method='rpart',data=training)
model$finalModel
#plotting classification tree dendogram
plot(model$finalModel,uniform=T)
text(model$finalModel,use.n=T,all=T,cex=.8)
#prettier dendogram with rattle package fancyRpartplot function
install.packages("rattle")
library(rattle)
fancyRpartPlot(model$finalModel)
#prediction
predict(model,newdata=testing)
confusionMatrix(predict(model,newdata=testing),testing$Species)


#note : classificatoin trees work well with non linear settings.exceptionally for categorizing problems where outcome is a facotr of multiple levls.
#they might overfit training model due to interaction in variables.
#transformations are not much important.
#easy to interpret & visualize and implicit feature selection
#non linearity isnt a problem and transformations are not needes


data(mtcars)
model1<-train(data=mtcars,mpg~.,method='rpart')

model1
fancyRpartPlot(model1$finalModel)
mean(mtcars$mpg)
qplot(data=mtcars,cyl,mpg)
mean(mtcars$mpg[])
mean(mtcars[mtcars$cyl>=5,]$mpg)
