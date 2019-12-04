library(kernlab)
library(caret)
data(spam)
partition<-createDataPartition(y=spam$type,p=0.75,list=F)
training<-spam[partition,]
testing<-spam[-partition,]
##pca combines dataframe variables into principal componenets that reduces variables and expalins as much variabtion as possilbe by using combinations of those variables.
typecolor=((spam$type=='spam')*1+1)
preproc<-preProcess(log10(training[,-58]+1),method='pca',pcaComp = 2)
trainpc<-predict(preproc,log10(training[,-58]+1))
plot(trainpc[,1],trainpc[,2],col=typecolor)
#creating model with just principal components.
model<-train(y=training$type,x=trainpc,method="glm")
model

testpc<-predict(preproc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(model,testpc))
##pca usually used in linear predictions like regression.
##transforms before pca is necessary for outliers.
