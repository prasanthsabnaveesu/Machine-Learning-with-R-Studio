rm(list=ls(all=TRUE))

setwd("")
library("arules")

# read data
FlightDelay = read.csv("FlightDelays.csv", header=T)
str(FlightDelay)

# subset "departure time" and convert rest of the
#attributes into factors
cat.data <- subset(FlightDelay,select=-c(1))
cat.data2 <- data.frame(apply(cat.data,2,function(x){as.factor(x)}))
str(cat.data2)

# bin "departure time" attribute

time.bins <- function(x){
  if(x>=600 & x<1200)
    dep.time <- '1'    
  else if(x>=1200 & x<1800)
    dep.time <- '2'    
  else if(x>=1800 & x<2100)
    dep.time <- '3'    
  else 
    dep.time <- '4'    
  
  return(dep.time)
}

time.zone <- data.frame("Time.Bins"=sapply(FlightDelay$CRS_DEP_TIME,time.bins))
data <- cbind(time.zone,cat.data2)
str(data)
summary(data)

flight <- as(data, "transactions")

itemFrequency(flight)
itemFrequencyPlot(flight)

# Experiment with the support value from 0.5 to 0.8 to see bar chart of variable
itemFrequencyPlot(flight, support = 0.5, cex.names=0.8)

rules <- apriori(flight,
                 parameter = list(support = 0.06, confidence = 0.6))

#Extract/Subset rules based on RHS or LHS 
rules2 <- apriori(flight,
                 parameter = list(support = 0.06, confidence = 0.6),appearance = list(rhs=c("Flight.Status=0")))

rules.classfilter1 <- as(subset(rules, subset = rhs %in% 
                                  "Flight.Status=0"),"data.frame")
rules.classfilter2 <- as(subset(rules, subset = rhs %in% "Flight.Status=0" & support > 0.8),"data.frame")
write.csv(rules.classfilter1,"finalrules.csv")
                   
rules_Lift <- subset(rules, subset = rhs %in% "Flight.Status=0" & lift > 1.01)
inspect(head(sort(rules_Lift, by = "confidence"), n = 3))

rules.sorted <- rules.classfilter1[order(rules.classfilter1$lift,decreasing = T),]
  
rulesImp <- rules.sorted[1:20,]
head(rulesImp)

## Finding redundant rules
#is.redundant(rules)
inspect(rules[is.redundant(rules)])
inspect(rules[!is.redundant(rules)])

# rearranging the rules as lhs an rhs as separate columns by splitting these rules at =>


require(stringr)
rules5=as(rules,"data.frame")
m=str_split(rules5$rules,"=>")
Class = data.frame(Class = unlist(lapply(m,function(x){str_trim(x[2])})))

Rule = data.frame(Rule = unlist(lapply(m,function(x){str_trim(x[1])})))
rules6= data.frame(rules5,Rule,Class)
rules6 = rules6[,-(which(colnames(rules6)=="rules"))]
Rulesset = unique(rules6)

write.csv(Rulesset,"Rulesset.csv")




