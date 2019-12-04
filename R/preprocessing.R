library(caret)
library(kernlab)
library(RANN)

data(spam)
set.seed(123)
partition<-createDataPartition(y=spam$type,p=0.75,list=F)
training<-spam[partition,]
testing<-spam[-partition,]
dim(training)

args(trainControl)
model<-train(type~.,data=training,method='glm')
model$finalModel
predictions<-predict(model,newdata=testing)
confusionMatrix(predictions,testing$type)




##cross validataion using k folds
folds<-createFolds(y=spam$type,k=10,list=TRUE,returnTrain = T)
#folds returns indices of training set.
sapply(folds,length)
folds[[1]]

##cross validation using resampling
folds<-createResample(y=spam$type,list=TRUE,times=10)
sapply(folds,length)

##cross validation using time slicing
tme<-1:1000
folds<-createTimeSlices(y=tme,initialWindow = 20,horizon=10)
folds$train[[1]]

#EXPLORATORY TRAINING ANALYSIS
##it's helpful to plot predictors.example wage dataset.
install.packages("ISLR")
library(ISLR)
data(Wage)
summary(Wage)

partition<-createDataPartition(y=Wage$wage,p=0.75,list=F)
training<-Wage[partition,]
testing<-Wage[-partition,]
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot='pairs')
qplot(age,wage,data=training)
qplot(age,wage,data=training,colour=jobclass)
qq<-qplot(age,wage,colour=education,data=training)
qq+geom_smooth(method="lm")

## group values into groups using cut2 function in hmisc package.
library(Hmisc)
cutwage<-cut2(training$wage,g=3)
table(cutwage)

p1<-qplot(cutwage,age,data=training,fill=cutwage,geom=c("boxplot"))
p1+geom_jitter()

t1<-table(cutwage,training$jobclass)
t1
prop.table(t1,1)      

qplot(wage,colour=education,data=training,geom="density")
        

#Pre processing 
## scaling and centering is done to reduce large variability in predictors.
partition<-createDataPartition(y=spam$type,p=0.75,list=F)
training<-spam[partition,]
testing<-spam[-partition,]
dim(training)
hist(training$capitalAve)
### we note that almost all mails have less captial letters. except a few which have high.
mean(training$capitalAve)
sd(training$capitalAve)
### We observe huge standard deviation. this may trick algorithms. so preprocess by standardize.
traincapave<-training$capitalAve
traincapaveS<-(traincapave-mean(traincapave))/sd(traincapave)
mean(traincapaveS)              
sd(traincapaveS)
##this decrease sd to 1 and mean to 0
## but for testing set its not exacatly 1 and 0
testcapave<-testing$capitalAve
testcapaveS<-(testcapave-mean(traincapave))/sd(traincapave)
mean(testcapaveS)
sd(testcapaveS)

##we can use preprocess function:
preobj<-preProcess(training[,-58],method=c("center","scale"))
preobj
traincapaveS<-predict(preobj,training[,-58])$capitalAve
mean(traincapaveS )
sd(traincapaveS)
##does same standardizing.

## using same preprocess object for test set.
testcapaveS<-predict(preobj,testing[,-58])$capitalAve
mean(testcapaveS)
sd(testcapaveS)
## we can also directly send preprocess to train command:
model<-train(type~.,data=training,preProcess=c('center','scale'),method='glm')
model


##box-cox transformation is done to transform continuous variables from skewed to normal distribution
preobj<-preProcess(training[,-58],method=c("BoxCox"))
traincapaveS<-predict(preobj,training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(training$capitalAve)
hist(traincapaveS)
#we see its not normal and not unimodal like before

##k nearest neighbour imputation using preprocess

###make some values NA
training$capave<-training$capitalAve
selectNA<-rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capave[selectNA]<-NA

#impute and standardize
preobj<-preProcess(training[,-58],method="knnImpute")
capave<-predict(preobj,training[,-58])$capave
#standardize capave values
capavetruth<-training$capitalAve
capavetruth<-(capavetruth-mean(capavetruth))/sd(capavetruth)
capavetruth
quantile((capave-capavetruth)[selectNA])
#we see not much differece between true values and Imputed values
