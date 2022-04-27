###Clear the Global Environment
rm(list=ls(all=TRUE))

### Library Call
library(forecast)
library(plyr)
library(dplyr)
library(DMwR)

# Set the working directory
setwd("")

### Read Data that is stored in RData format
data = read.csv("BSE-BOM532309.csv",header=TRUE,sep=",")

### Observe the data
# * What are all the columns given in the data
names(data)

##Summary
summary(data)
## Dimensions
dim(data)

### Observation & Analysis 
RtData2Day <- data %>% group_by(Date) %>% summarise("MIN_PRICE" = mean(Close))
RtData2Day <- data.frame(RtData2Day)
str(RtData2Day)

# Converting the date column to appropriate date format
RtData2Day$Date = as.Date(RtData2Day$Date,format="%Y-%m-%d")

### Missing values in time series
# * Some times there will be missing entries in dates which will create a missing day in the data or if it is quarter,month or annual .
# * Observe the data to find if any
head(RtData2Day)
str(RtData2Day)

sum(is.na(RtData2Day))

### Detection of the missing values
# * Create a date field which consists of continuous sequence of dates 
# * Check against this with the current price data and find out the missing dates.
# * Join this variable to the current data to see the missing dates
minDate = min(as.Date(RtData2Day$Date,format="%Y-%m-%d"))
maxDate = max(as.Date(RtData2Day$Date,format="%Y-%m-%d"))
# Generating the sequence of dates from start date to end date
seq <- data.frame("dateRange"=seq(minDate,maxDate,by="days"))

# Mering the data frames
RtData2Day2 <- seq %>% full_join(RtData2Day,c("dateRange" = "Date"))

# Coverting to data frame
RtData2Day <- data.frame(RtData2Day)
RtData2Day2 <- RtData2Day2
head(RtData2Day2)

### Impuation of Missing Values
RtData2Day2$MIN_PRICE <- na.locf(RtData2Day2$MIN_PRICE)
head(RtData2Day2)

# Getting the year from the date column and creating a new column YEAR
RtData2Day2$YEAR <-as.numeric(format(RtData2Day2$dateRange,format="%Y"))
# Getting the week from the date column and creating a new column WEEK
RtData2Day2$WEEK <-as.numeric(format(RtData2Day2$dateRange,format="%W"))
# Sorting the data in ascending order based on Year and Week
RtData2Day2 <- RtData2Day2[order(RtData2Day2$YEAR,RtData2Day2$WEEK),]
# Getting the avreage price of the book at week level
RtData2Week <- RtData2Day2 %>% group_by(YEAR,WEEK) %>% summarise("MIN_PRICE" = mean(MIN_PRICE))
# Coverting to data frame
RtData2Week <- data.frame(RtData2Week)

### Splitting of the Data into train and test
# * splitting is done by sequential splitting
Train <- RtData2Week[1:(nrow(RtData2Week) - 4),]
Test <- RtData2Week[(nrow(RtData2Week) - 3):nrow(RtData2Week),]

### converting into time series 
# our target variable is price and each week has a price aggregated
Price <- ts(Train$MIN_PRICE, frequency =52)

### Vizualize the time series Data
plot(Price,type="l",lwd=3,col="red",xlab="week",ylab="Price",main="Time series plot for Book-xyzabc")

# * Decompose will provide us with the info on seasonality,trend and randomness
Pricedecomposed = decompose(Price)
plot(Pricedecomposed,col="blue")

## ACF and PACF plots
par(mfrow=c(2,2))
acf(Price,lag=30)
pacf(Price,lag=30)

# Looking at the acf anf pacf plots for 30 lags
acf(Train$MIN_PRICE,lag=30)
pacf(Train$MIN_PRICE,lag=30)

### Stationarize by differencing
par(mfrow=c(2,3))
plot(diff(Price,lag = 1),type="l");
acf(diff(Price,lag = 1),lag=30) ;
pacf(diff(Price,lag = 1),lag=30)
plot(diff(Price,lag=2),type="l");  acf(diff(Price,lag = 2),lag=30); pacf(diff(Price,lag = 2),lag=30)

# one lag has stationarize the data we can use ndiffs of forecast package to check no of differences required to      stationarize the data
ndiffs(Price)

### Modelling  the time series using simple moving averages
fitsma <- SMA(Price,n=2)

# Forecasting for book price the next 4 weeks
predsma <- forecast(fitsma[!is.na(fitsma)],h=4)
plot(predsma)

### Define the metric MAPE 
smaTrainMape <- regr.eval(Price[2:length(Price)],fitsma[2:length(Price)])
smaTestMape <- regr.eval(Test$MIN_PRICE,predsma$mean)
smaTrainMape #rmse=10.55
smaTestMape #rmse=17.74

### Weighted Moving Averages
fitwma<- WMA(Price,n=2,1:2)
# Forecasting for book price the next 4 weeks
predwma <- forecast(fitwma[!is.na(fitwma)],h=4)
plot(predwma)

### Define the metric MAPE 
wmaTrainMape <- regr.eval(Price[2:length(Price)],fitwma[2:length(Price)])
wmaTestMape <- regr.eval(Test$MIN_PRICE,predwma$mean)
wmaTrainMape #rmse=7.03
wmaTestMape #rmse=14.98

### Exponential Moving Averages
fitEma <- EMA(Price, n = 2)
# Forecasting for book price the next 4 weeks
predema <- forecast(fitEma[!is.na(fitEma)],h=4)
plot(predema)

### Define the metric MAPE 
emaTrainMape <- regr.eval(Price[2:length(Price)],fitEma[2:length(Price)])
emaTestMape <- regr.eval(Test$MIN_PRICE,predema$mean)
emaTrainMape #rmse=8.158
emaTestMape #rmse=9.60

## Build a HoltWinters model  with trend 
holtpriceforecast <- HoltWinters(Price,gamma=FALSE)
head(holtpriceforecast$fitted)

## HoltWinters model  with trend  and Seasonality
priceholtforecast <- HoltWinters(Price, beta=TRUE, gamma=TRUE, seasonal="additive")
head(priceholtforecast$fitted)

### Prediction on the Train
holtforecastTrain <- data.frame(priceholtforecast$fitted)
holtforecastTrainpredictions <- holtforecastTrain$xhat
head(holtforecastTrainpredictions)

### Prediction on test data
holtpriceforecast<-  forecast(priceholtforecast,h = 4)
plot(holtpriceforecast,ylim = c(-100,1100))

### Define the metric hw 
hwTestMape <- regr.eval(Test$MIN_PRICE,holtpriceforecast$mean)
hwTestMape #rmse=17.41

### Arima Models
model1 <- arima(Price,c(0,0,0))
model1
acf(Price) 
pacf(Price)
plot(Price)

## Considering the difference from the graph as d=1 to stationarize
model2 <- arima(Price,c(0,1,0))
model2
acf(diff(Price,lag = 1))
pacf(diff(Price,lag = 1))
plot(diff(Price))

# plot has still non stationary behaviour another difference can stationarize it 
model3 <- arima(Price,c(0,2,0))
model3
plot(diff(Price,differences = 2))
acf(diff(Price,differences = 2))
pacf(diff(Price,differences = 2))

# Observing the acf and pacf there is significant lag in acf and also in pacf that has to be taken care 
model4 <- arima(Price,c(1,1,1))
model4

###  Auto Arima
MODEL_ARIMA <- auto.arima(Price, ic='aic')
summary(MODEL_ARIMA)

### Forecast on the models 
pricearimaforecasts1 <- forecast(model1, h=4)
plot(pricearimaforecasts1)
pricearimaforecasts2 <- forecast(model2, h=4)
plot(pricearimaforecasts2)
pricearimaforecast3 <- forecast(model3, h=4)
plot(pricearimaforecast3)
pricearimaforecasts4 <- forecast(model4, h=4)
plot(pricearimaforecasts4)
pricearimaforecasts_autArima<- forecast(MODEL_ARIMA,h=4)
plot(pricearimaforecasts_autArima,flwd = 2)

### Define the metric ARIMA 
arimaModel1TestMape <- regr.eval(Test$MIN_PRICE,pricearimaforecasts1$mean)
arimaModel1TestMape #rmse=2.566e+02

arimaModel2TestMape <- regr.eval(Test$MIN_PRICE,pricearimaforecasts2$mean)
arimaModel2TestMape #rmse=5.11

arimaModel3TestMape <- regr.eval(Test$MIN_PRICE,pricearimaforecast3$mean)
arimaModel3TestMape #rmse=14.988

arimaModel4TestMape <- regr.eval(Test$MIN_PRICE,pricearimaforecasts4$mean)
arimaModel4TestMape #rmse=5.09

### Define the metric AUTO ARIMA 

autoarimaTestMape <- regr.eval(Test$MIN_PRICE,pricearimaforecasts_autArima$mean)
autoarimaTestMape #rmse=5.17

#model (1,1,1) has less "rmse" value when compared to other models