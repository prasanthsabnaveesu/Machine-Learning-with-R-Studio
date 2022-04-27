# Cleaning the work space
rm(list = ls(all=TRUE))

# Setting the working directory
setwd("")

## Read the Data


# Make sure the dataset is located in your current working directory, 
# else you can change your working directory using the "setwd()" function.

# 1. Read the "bank.txt" data into R

bank=read.table("bank.txt",sep=";",header=TRUE)




## Understand the data

# 2. Use the str() function to get the dimensions and types of attributes in the dataset

str(bank)




# 3. Use the summary() function to understand the distribution of variables in the dataset


summary(bank)
table(bank$campaign)
table(bank$previous)
bank$campaign=as.factor(bank$campaign)
bank$previous=as.factor(bank$previous)
# Use the head() and tail() functions to get a look at the data

head(bank)
tail(bank)


# Data Pre-processing

## Missing Values

# 3. Check the number of missing values in the data frame

sum(is.na(bank))





## 4. Divide the data into Train/Test Split

# * Split the data 70/30 into train and test sets by using __Stratified Sampling

# Split the data using stratified sampling, 
# we can do that by using the createDataPartition() function from the caret package also set the seed

install.packages("caret")
library(caret)
trainindex=createDataPartition(bank$y, p=0.75,list=FALSE)
train_data=bank[trainindex,]
test_data=bank[-trainindex,]







# Build a model

## Basic Logistic Regression Model

# Use the glm() function to build a basic model

# Build a model using all the variables, excluding the response variable, in the dataset

log_reg <- glm(y ~ ., data = train_data, family = "binomial")

 # Get the summary of the model and understand the output
summary(log_reg)


## 5. Run the stepAIC model and identify the important features

 # "stepAIC()" is a function in the MASS package
# * stepAIC uses AIC (Akaike information criterion) to either drop variables ("backward" direction) or add variables ("forward" direction) from the model



library(MASS)
step3=stepAIC(log_reg,direction="both")
model1=glm(y ~ marital + default + housing + loan + contact + day + month + 
             duration + poutcome, family="binomial",data=train_data)




## 6. Check whether there is multicolinearity in the data
# Modifying the Model with the VIF - **Variance Inflation Factor :**
  
# We use the "vif()" function from the car package. 
# Every explanatory variable would have a VIF score
install.packages("DmWR")
library(DMwR)
library(car)
vif(model1)
regr.eval(train_data$MEDV, model1$fitted.values)
Pred<-predict(model1,test_data)
regr.eval(test_data$MEDV, Pred) 
par(mfrow=c(2,2))
plot(model1)





# ROC

## Predicted Values are between 0 and 1

# The predict() function on the "glm" object of "binomial" family gives a probability score between 0 and 1, NOT the original levels (0 and 1) of the response variable 

# Hence we must first choose a cutoff point for getting to the original levels of the response variables

# To choose the cutoff point we will use the train data, as test data should not be used to make any decisions regarding the model

## Creating an ROC plot

# __Steps to create an ROC plot :__

# Get a list of predictions (probability scores) using the predict() function

# Use the argument 'type = "response"' in the predict function to get a list of predictions between 0 and 1

# By default if no dataset is mentioned, training data is used

prob_train <- predict(log_reg, type = "response")


# Using the ROCR package create a "prediction()" object

library(ROCR)

# The prediction object takes the probability scores and the original levels for theses data as input

pred <- prediction(prob_train, train_data$y)

# The prediction object contains a list of predictions (probability scores), original class labels, cutoffs, false positives, true positives, true negatives, false negatives, No. of positive predictions and No. of negative predictions corresponding to these cutoffs. Class distribution in the dataset.


# Extract performance measures (True Positive Rate and False Positive Rate) using the "performance()" function from the ROCR package

# The performance() function from the ROCR package helps us extract metrics such as True positive rate, False positive rate etc. from the prediction object, we created above.

# Two measures (y-axis = tpr, x-axis = fpr) are extracted

perf <- performance(pred, measure="tpr", x.measure="fpr")


# Plot the ROC curve using the extracted performance measures (TPR and FPR)

plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))


# Extract the AUC score of the ROC curve and store it in a variable named "auc"

 # Use the performance() function on the prediction object created above using the ROCR package, to extract the AUC score

perf_auc <- performance(pred, measure="auc")

# Access the auc score from the performance object

auc <- perf_auc@y.values[[1]]

print(auc) #0.9096

# For different threshold values identifying the tpr and fpr

# For different threshold values identifying the tpr and fpr
cutoffs <- data.frame(cut= perf@alpha.values[[1]], fpr= perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])

# Sorting the data frame in the decreasing order based on tpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]

# Plotting the true positive rate and false negative rate based based on the cutoff       
# increasing from 0.1-1
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))



## Choose a Cutoff Value

# Based on the trade off between TPR and FPR depending on the business domain, a call on the cutoff has to be made.

# A cutoff of 0.1 can be chosen

# Predictions on train -->
pred_class <- ifelse(prob_train> 0.1, "yes", "no")
table(train_data$y,pred_class)
  
  
## Predictions on test data
# After choosing a cutoff value of 0.1, let's predict the class labels on the test data using our model

prob_test <- predict(log_reg, test_data, type = "response")

preds_test <- ifelse(prob_test > 0.1, "yes", "no")


# Evaluation Metrics for classification

## Manual Computation

### Confusion Matrix

 # Create a confusion matrix using the table() function

test_data_labs <- test_data$y

conf_matrix <- table(test_data_labs, preds_test)

print(conf_matrix)

### Specificity

 # The Proportion of correctly identified negatives by the test/model.

specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])

print(specificity)

### Sensitivity

 # The Proportion of correctly identified positives by the test/model.

sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])

print(sensitivity)

### Accuracy

 # The Proportion of correctly identified psotivies/negatives in the entire population by the test/model

accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)

print(accuracy)


## Automated Computation through Caret

 # Evaluation metrics for classification can be accessed through the "confusionMatrix()" function from the caret package

library(caret)

# Using the argument "Positive", we can get the evaluation metrics according to our positive referene level

confusionMatrix(preds_test, test_data$y, positive = "yes")

