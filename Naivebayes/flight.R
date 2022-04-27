rm(list=ls(all=TRUE))
flight<-read.csv("FlightDelays.csv",header=T,sep=",")
str(flight)
summary(flight)
flight$Weather<-as.factor(flight$Weather)
flight$DAY_WEEK<-as.factor(flight$DAY_WEEK)
flight$Flight.Status<-as.factor(flight$Flight.Status)
flight$levels<-ifelse(flight$DEP_TIME>=600 & flight$DEP_TIME<=1200, "level1",
                      ifelse (flight$DEP_TIME>1200 & flight$DEP_TIME<=1800, "level2",
                      ifelse(flight$DEP_TIME>1800 & flight$DEP_TIME<=2100, "level3", "level4")))
table(flight$levels)
flight$DEP_TIME<-NULL
set.seed(007)
trainRows=sample(x=1:nrow(flight),size=0.70*nrow(flight))
train_data = flight[trainRows,] 
test_data = flight[-trainRows,]
test<-length(which(test_data$Flight.Status==1)/nrow(test_data))
test
train<-length(which(train_data$Flight.Status==1)/nrow(train_data))
train
install.packages("e1071")
library(e1071)
model=naiveBayes(train_data$Flight.Status~.,train_data)
prob_test=predict(model,test_data)
library(caret)
confusionMatrix(prob_test,test_data$Flight.Status)

