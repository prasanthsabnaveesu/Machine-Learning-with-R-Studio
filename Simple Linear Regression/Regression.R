setwd("C:/Users/Classroom 4/Desktop/Batch 34/CSE7302c/Day01")

# Regression

bigmac <- read.csv("BigMac-NetHourlyWage.csv", header = T, sep = ",")
bigmac
lm(bigmac$Net.Hourly.Wage....~bigmac$Big.Mac.Price....,bigmac)
bigmaclm <- lm(bigmac$Net.Hourly.Wage....~bigmac$Big.Mac.Price....,bigmac)
summary(bigmaclm)
par(mfrow=c(1,1))
plot(bigmac$Big.Mac.Price....,bigmac$Net.Hourly.Wage....)
abline(bigmaclm)
par(mfrow=c(2,2))
plot(bigmaclm)
