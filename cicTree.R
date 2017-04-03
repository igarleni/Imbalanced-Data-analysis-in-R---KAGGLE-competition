#

library(party)

###############################################
## Conditional inference Classification Tree ##
###############################################
variableClass <- names(balancedData)[n]
formulaClass <- as.formula(paste(variableClass,"~.",sep=""))
for(i in 1:10){
  #generate model
  model <- ctree(formulaClass, balancedData[trainPartitions[[i]], ])
  #predict over test fold
  predictions <- predict(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- ctree(formulaClass, balancedData)
kagglePrediction <- predict(model, newdata = testData[, -1])