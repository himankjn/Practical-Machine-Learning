#model based prediction
#Linear discriminant analysis, Quadratic discriminant analysis,Naive Bayes
data(iris)
library(ggplot2)
partition<-createDataPartition(y=iris$Species,p=0.7,list=F)
training<-iris[partition,]
testing<-iris[-partition,]
modelLDA<-train(Species~.,data=training,method='lda')
modelNB<-train(Species~.,data=training,method='nb')
confusionMatrix(testing$Species,predict(modelNB,testing))
confusionMatrix(testing$Species,predict(modelLDA,testing))
equality1<-predict(modelLDA,testing)==testing$Species
equality2<-predict(modelNB,testing)==testing$Species
qplot(Petal.Width,Petal.Length,color=equality1,data=testing)
qplot(Petal.Width,Petal.Length,color=equality2,data=testing)
