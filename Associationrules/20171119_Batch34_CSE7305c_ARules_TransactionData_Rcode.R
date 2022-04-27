rm(list=ls(all=TRUE))

setwd("")
#1. install.packages("arules")

#2. Read 'Transactions.csv' such that the arules package
#treats the input file as "transaction" data.

require(arules)

trans = read.transactions(file="Transactions.csv", 
                          format="single",
                          sep=",",cols=c(1,2))

#3. Explore and understand the data and items of transaction data
inspect(trans)
trans

#4. Understand Item frequencies
itemFrequency(trans)
itemFrequencyPlot(trans)
image(trans)

#5. Implementing association mining using 'Apriori' algorithm to extract rules
rules <- apriori(trans,parameter = list(sup = 0.2, conf = 0.6,target="rules"))

#6. Understanding the rules
summary(rules) 
inspect(rules)

##7. Print the 5 rules sorted by confidence and then support as a data.frame.
as((sort(rules, by = c("confidence", "support"))), "data.frame")
as(head(sort(rules, by = c("confidence", "support")), n=5), "data.frame")

rules.sorted <- sort(rules, by="lift")
inspect(rules)

