#

#######################
## Linear Regression ##
#######################
balancedData$PV1MATH = as.numeric(balancedData$PV1MATH)
for(i in 1:10){
  #generate model
  model <-lm(formulaClass, data= balancedData[trainPartitions[[i]], ])
  #predict over test fold
  predictions <- predict(model, newdata = balancedData [testPartitions[[i]], -n])
  for(j in 1:length(predictions)){
    if (predictions[j] < 0.5)
      predictions[j] = 0
    else
      predictions[j] = 1
  }
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <-lm(formulaClass, data= balancedData)
kagglePrediction <- predict(model, newdata = testData[, -1])