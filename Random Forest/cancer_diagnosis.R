rm(list=ls(all=TRUE))
#Import the cancer_diagnosis.csv data into R
setwd("")
data=read.csv("cancer_diagnosis.csv",header=TRUE,sep=",")
#Study dataset
str(data)
#Convert all features into appropriate data types
data$Cancer=as.factor(data$Cancer)
table(data$Cancer)

#Split dataset into train and test
set.seed(123)
ind=sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]

#Build the classification model using randomForest
library(randomForest)
set.seed(222)
rf=randomForest(Cancer~.,data=train, ntree=450,mtry=5,importance=T,proximity=T)
attributes(rf)
plot(rf)

#View results and understand important attributes
print(rf)
rf $predicted
rf $importance

#View results and understand important attributes
varImpPlot(rf)

#Predict on Train and Test datasets
library(caret)
p1=predict(rf,train)
p2=predict(rf,test)

#Calculate precision, recall and accuracy
confusionMatrix(p1,train$Cancer) # accu=1
confusionMatrix(p2,test$Cancer) #acc=0.939

#Stacking Technique: Cancer Dataset
# Use pre-processed data that is applied from step1 - step5 for random forest.
# Building different Machine Learning algorithms
#(1) Build rpart model on the training dataset
library(rpart)
model_dt <- rpart(Cancer ~ . , train)

# Prediction on the train data
preds_train_dt <- predict(model_dt)
preds_train_tree <- ifelse(preds_train_dt[, 1] > preds_train_dt[, 2], 0, 1)
# Prediction on the test data
preds_dt <- predict(model_dt, test)
preds_tree <- ifelse(preds_dt[, 1] > preds_dt[, 2], 0, 1)
preds_tree<-as.factor(preds_tree)
confusionMatrix(preds_tree, test$Cancer) #acc=0.926

# (2) Build KNN model on the training dataset
Library(caret)
# We'll build our KNN model, using the knn3() function from the caret package
model_knn <- knn3(Cancer ~ . , train, k = 5)
# Store the predictions on the train data
preds_train_k <- predict(model_knn, train)
preds_train_knn <- ifelse(preds_train_k[, 1] > preds_train_k[, 2], 0, 1)
# Prediction on the test data
preds_k <- predict(model_knn, test)
preds_knn <- ifelse(preds_k[, 1] > preds_k[, 2], 0, 1)
preds_knn=as.factor(preds_knn)
confusionMatrix(preds_knn, test$Cancer) #acc=0.76

# (3) Build bagging rpart model on the training dataset
library(ipred)
set.seed(1234)
model_tree_bag <- bagging(Cancer ~ . , data=train,nbagg = 10,control =
                            rpart.control(cp = 0.01, xval = 10))
# Prediction on the train data
preds_train_tree_bag <- predict(model_tree_bag)
# Prediction on the test data
preds_tree_bag <- predict(model_tree_bag, test)
confusionMatrix(preds_tree_bag, test$Cancer) #accu=0.945

# (4) Preparing the train data for stacking model by combining training predictions
# of Random Forest, KNN, rpart & bagging models
train_preds_df <- data.frame(rf = preds_train_rf, knn = preds_train_knn,
                             tree = preds_train_tree, tree_bag = preds_train_tree_bag,
                             Cancer = train_data$Cancer)
# convert the target variable into a factor
train_preds_df$Cancer <- as.factor(as.character(train_preds_df$Cancer))

# (5) Check if there are any correlations in the data
# Use the sapply() function to convert all the variables other than the target
#variable into a numeric type
library(DMwR)
library(caret)
numeric_st_df <- sapply(train_preds_df[, !(names(train_preds_df) %in%
                                             "Cancer")], function(x) as.numeric(as.character(x)))
cor(numeric_st_df)
