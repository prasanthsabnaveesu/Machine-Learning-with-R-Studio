# # Installation. 
# # Note: Windows user will need to install Rtools first
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

rm(list=ls(all=TRUE))
# install.packages("dummies")
setwd("E:/INSOFE/Batch 34/20171203_Batch34_CSE7305c_Boosting_Lab05/")

# Load required libraries
library(vegan)
library(dummies)
library(xgboost) 

# Feature names
attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data using csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

# Removing the id, zip and experience. 
drop_Attr = c("id", "zip", "exp")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
rm(drop_Attr)

# Convert attribute to appropriate type  
cat_Attr = c("family", "edu", "securities", 
             "cd", "online", "cc", "loan")
num_Attr = setdiff(attr, cat_Attr)
rm(attr)

cat_Data <- data.frame(sapply(data[,cat_Attr], as.factor))
num_Data <- data.frame(sapply(data[,num_Attr], as.numeric))

data = cbind(num_Data, cat_Data)
rm(cat_Data, num_Data)

# Do the summary statistics and check for missing values and outliers.
summary(data)
#------------------------------------------------------

# Build the xgboost classification model.

# Standardizing the numeric data
# final_Data = decostand(data[,num_Attr], "range") 
final_Data = data[,num_Attr]
rm(num_Attr)

# Convert all categorical attributes to numeric 
# 1. Using dummy function, convert education and family categorical attributes into numeric attributes 
edu = dummy(data$edu)
family = dummy(data$family)
final_Data = cbind(final_Data, edu, family)
cat_Attr = setdiff(cat_Attr, c("edu", "family"))
rm(edu, family)

# 2. Using as.numeric function, convert remaining categorical attributes into numeric attributes 
final_Data = cbind(final_Data, sapply(data[,cat_Attr], 
                                      function(x){as.numeric(as.character(x))}))
rm(cat_Attr)

ind_Attr = setdiff(names(final_Data), "loan")

str(final_Data)
summary(final_Data)

#############################################################
library(caret)
set.seed(1234)

index_train <- createDataPartition(final_Data$loan, p = 0.7, list = F)

pre_train <- final_Data[index_train, ]
pre_test <- final_Data[-index_train, ]

# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables

# Let's use the preProcess() function from the caret package to standardize the variables, using just the data points in the training data

std_method <- preProcess(pre_train[,ind_Attr], method = c("center", "scale"))
train_Data <- predict(std_method, pre_train)
test_Data <- predict(std_method, pre_test)
#############################################################################

# Check how records are split with respect to target attribute.
table(final_Data$loan)
table(train_Data$loan)
table(test_Data$loan)
rm(final_Data)

# Constructing the Dense matrix on the train and test data
dtrain = xgb.DMatrix(data = as.matrix(train_Data[,ind_Attr]),
                     label = train_Data$loan)

dtest = xgb.DMatrix(data = as.matrix(test_Data[,ind_Attr]),
                    label = test_Data$loan)

# fit the model
model = xgboost(data = dtrain, max.depth = 4, 
                eta = 0.4, nthread = 2, nround = 40, 
                objective = "binary:logistic", verbose = 1)

# objective = "binary:logistic": we will train a binary classification model ;
# max.deph = 4: the trees won't be deep, because our case is very simple ;
# nthread = 2: the number of cpu threads we are going to use;
# nround : max number of boosting iterations.
# eta : It controls the learning rate
# verbose = 1: print evaluation metric

# Both xgboost (simple) and xgb.train (advanced) functions train models.

# Because of the way boosting works, there is a time when having too many rounds lead to an overfitting. One way to measure progress in learning of a model is to provide to XGBoost a second dataset already classified. 
#Therefore it can learn on the first dataset and test its model on the second one.
#Some metrics are measured after each round during the learning.

#Use watchlist parameter. It is a list of xgb.DMatrix, 
#each of them tagged with a name.

watchlist = list(train=dtrain, test=dtest)

model = xgb.train(data=dtrain, max.depth=4,
                  eta=0.3, nthread = 2, nround=20, 
                  watchlist=watchlist,
                  eval.metric = "error", 
                  objective = "binary:logistic", verbose = 1)
# eval.metric allows us to monitor two new metrics for each round, logloss and error.

importance <- xgb.importance(feature_names = ind_Attr, model = model)
print(importance)
xgb.plot.importance(importance_matrix = importance)

# Gain is the improvement in accuracy brought by a feature to the branches it is on. 
# Cover measures the relative quantity of observations concerned by a feature.
# Frequency is the number of times a feature is used in all generated trees. 

# save model to binary local file
xgb.save(model, "xgboost.model")
rm(model)

# load binary model to R
model <- xgb.load("xgboost.model")

# prediction on test data
pred <- predict(model, as.matrix(test_Data[,ind_Attr]))

# size of the prediction vector
print(length(pred))

# limit display of predictions to the first 10
print(head(pred))

# The numbers we get are probabilities that a datum will be classified as 1. 
# Therefore, we will set the rule that if this probability for a 
# specific datum is > 0.5 then the observation is classified as 1 (or 0 otherwise).

prediction <- as.numeric(pred > 0.5,1,0)
print(head(prediction))

prediction <- as.factor(as.character(prediction))

confusionMatrix(test_Data$loan, prediction,positive = "1")
