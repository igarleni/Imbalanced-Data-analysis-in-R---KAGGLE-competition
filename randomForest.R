#

library(randomForest)

###################
## Random Forest ##
###################
for(i in 1:10){
  #generate model
  model <- randomForest::randomForest(PV1MATH ~ ., data=balancedData[trainPartitions[[i]], ],
                                      ntree=ntrees)
  #predict over test fold
  predictions <- predict(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- randomForest::randomForest(PV1MATH ~ ., data=balancedData, ntree=ntrees)
kagglePrediction <- predict(model, newdata = testData[, -1])