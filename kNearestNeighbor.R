#

########################
## K-nearest neighbor ##
########################
for(i in 1:10){
  #predict over test fold
  predictions <- knn(train = balancedData[trainPartitions[[i]], -n],
                     test = balancedData[testPartitions[[i]], -n],
                     cl = balancedData[trainPartitions[[i]], n], k=kn)
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
kagglePrediction <- knn(train = balancedData[, -n], test = testData[, -1],
                        cl = balancedData[, n], k=kn)