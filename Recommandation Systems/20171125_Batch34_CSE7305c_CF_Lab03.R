
# Goal
#To predict the user rating for a movie, based on a user-item ratings matrix


# Agenda

#Get/Simulate a user-item ratings matrix

#Preprocess the ratings matrix

#Build recommendation engine based on IBCF

#Build recommendation engine based on UBCF

#Compare the performance of IBCF and UBCF

# Loading Required Packages
install.packages("recommenderlab")
install.packages("lsa")
library(recommenderlab) 
library(lsa)

# Simulating the data
# Simulate a user-item rating matrix

set.seed(5643)
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
            replace=TRUE, prob=c(rep(.4/6,6),.6)),ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))  
m

#getRatingMatrix(as(m,"realRatingMatrix"))
#getRatings(as(m,"realRatingMatrix"))


# Building a Recommender

# Convert User-item Matrix to realRatingMatrix matrix
r <- as(m, "realRatingMatrix")

#The realRatingMatrix object stores data in sparse format


##Normalize the realRating matrix
r_m <- normalize(r)
getRatings(r_m)

(as(r_m, "data.frame"))

getRatingMatrix(r)-mean(getRatings(r))

##Visualize the ratings matrix

#Small portions of rating matrices can be visually inspected using image()
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")


# Create Recommender systems using IBCF & UBCF
#input is un-normalized  data 
getRatings(r)
r1 <- Recommender(r, method = "UBCF")
r1
getModel(r1)


#Split the data into train and evaluation sets
getRatingMatrix(r)
min(rowCounts(r))
e <- evaluationScheme(r, method="split", train=0.6,
                      given=3)
getRatingMatrix(getData(e,"train"))
getRatingMatrix(getData(e,"known"))
getRatingMatrix(getData(e,"unknown"))
getRatingMatrix(r)

getRatingMatrix(r)


#We create two recommenders 
#(user-based and item-based collaborative filtering) using the training data.
#getRatingMatrix(getData(e,"train"))
getRatings(getData(e, "train"))

#UBCF
r2 <- Recommender(getData(e, "train"), "UBCF")
r2

#IBCF
r3 <- Recommender(getData(e, "train"), "IBCF")
r3

# Predict recommendations
#Compute predictions for the known part of  the test data (3 items for each user) using the two algorithms.

getRatingMatrix(getData(e, "known"))

#Predict UBCF
p1 <- predict(r2, getData(e, "known"), type="ratings")
p1

as(p1, "list")
as(p1, "matrix")
getRatingMatrix(r)
getRatingMatrix(getData(e, "known")) 
getRatingMatrix(getData(e, "unknown")) 

#retains 3 rankings from each record of test-set
getRatingMatrix(getData(e, "unknown"))
#removes the 3 rankings from the test-set

#Predict IBCF
p2 <- predict(r3, getData(e, "known"), type="ratings")
p2
as(p2, "list")
as(p2, "matrix")
getRatingMatrix(getData(e, "known"))
getRatingMatrix(getData(e, "unknown"))

# Compare the performance of IBCF and UBCF
#Finally, we can calculate the error between the prediction and the unknown part of the test data.
error <- rbind(
  calcPredictionAccuracy(p1, getData(e, "unknown")),
  calcPredictionAccuracy(p2, getData(e, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
error

# References
#https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

