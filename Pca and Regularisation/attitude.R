rm(list=ls(all=TRUE))
attach(attitude)
data=attitude
summary(data)

#Check names and structure of data
names(data)
str(attitude)
data$rating<-rating
data$rating=NULL
#Split the Data into train and test
library(caret)
set.seed(123)
train_rows<-sample(x=1:nrow(data),size=0.7*nrow(data))
train<-data[train_rows,]
test<-data[-train_rows,]

#Standardize the Data by using the function "PreProcess" from caret package
pre1<-preProcess(train[,setdiff(colnames(train),"rating")])
train_scale<-predict(pre1,train[,setdiff(colnames(train),"rating")])
test_scale<-predict(pre1,test[,setdiff(colnames(test),"rating")])

#Apply pca on train_scale using princomp
prcomp_train <- princomp(train_scale)
plot(prcomp_train)
train_data<-prcomp_train$scores
traindata=cbind(rating,train_data)

prcomp_test <- princomp(test_scale)
plot(prcomp_test)
test_data<-prcomp_test$scores
testdata=cbind(rating,test_data)
test_data1=predict(prcomp_train,test_scale)
train_data1=predict(prcomp_train,train_scale)

library(DMwR)

#preds_model<-predict(model1,train_data[,!(names(train_data)%in%c("rating"))])
regr.eval(rating,test_data1) 
regr.eval(rating,train_data1) 
