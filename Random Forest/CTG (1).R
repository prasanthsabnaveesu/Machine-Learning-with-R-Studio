rm(list=ls(all=TRUE))
setwd("")
data=read.csv("CTG (1).csv",header=TRUE,sep=",")
str(data)
data$NSP=as.factor(data$NSP)
table(data$NSP)

#data partition
set.seed(123)
ind=sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]

#random forest
library(randomForest)
set.seed(222)
rf=randomForest(NSP~.,data=train, ntree=300,mtry=8,importance=T,proximity=T)
print(rf)
attributes(rf)

#prediction & confusion matrix - train data
library(caret)
p1=predict(rf,train)
confusionMatrix(p1,train$NSP)

#error rate of random forest
plot(rf)

#tune mtry
t=tuneRF(train[,-22],train[,22],
         stepFactor = 0.5,
         plot = TRUE,
         ntreeTry = 300,
         trace = TRUE,
         improve = 0.05)

#no. of nodes for the trees
hist(treesize(rf),
     main = "no. of nodes for the trees",
     col="Green")

#variable importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main="Top 10- Variable Importance")
importance(rf)
varUsed(rf)


