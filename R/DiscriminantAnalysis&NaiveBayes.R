#Model Based Prediction:
#Discriminant analysis used with categorical outcome and only continuous predictors.
#It is used to separate categories.
library(caret)
partition<-createDataPartition(y=iris$Species,p=0.7,list=F)
training<-iris[partition,]
testing<-iris[-partition,]
#linear discriminant analysis
modelLDA<-train(Species~.,data=training,method='lda')
#quadratic discriminant analysis
modelQDA<-train(Species~.,data=training,method='qda')
#naive bayes
modelNB<-train(Species~.,data=training,method='nb')
pQDA<-predict(model2,testing)
pLDA<-predict(model,testing)
pNB<-predict(modelb,testing)
table(pQDA,pLDA)
