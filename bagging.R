#
library(rpart)
library(adabag)

#IN variables:
# maxdp = 5
# minsplt = 15

#############
## Bagging ##
#############
for(i in 1:10){
  #generate model
  model <- adabag::bagging(formulaClass, 
                           data = balancedData[trainPartitions[[i]], ], 
                           control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
  #predict over test fold
  predictions <- adabag::predict.bagging(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions$class)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- adabag::bagging(formulaClass, data = balancedData, 
                         control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
baggingPrediction <- adabag::predict.bagging(model, newdata = as.data.frame(testData[, -1]))
kagglePrediction <- baggingPrediction$class