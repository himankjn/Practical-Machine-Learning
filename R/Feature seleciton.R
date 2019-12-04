#Featureselection or Co-variate creation. it includes turning raw data to useful predictor.

library(ISLR)
library(caret)
data(Wage)
partition<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[partition,]
testing<-Wage[-partition,]

##1) Converting Factor variables to dummy variables.
table(Wage$jobclass)
dummies<-dummyVars(wage~jobclass,data=training)
head(predict(dummies,training))

##2) Removing zero covariates. i.e. variables that provide almost no imput. 
nsv<-nearZeroVar(training,saveMetrics = T)
nsv
#remove nzv true variables.

##3) splines for non linear model e.g quadratic or cubic variable for curvature.
##use  gam functino in caret package. Refer regression models > nonLinear
https://github.com/himank369123/Regression-Models/blob/master/nonlinear.R
