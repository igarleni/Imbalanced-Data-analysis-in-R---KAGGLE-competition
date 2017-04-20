#
library(randomForest)

#IN variables:
# ntrees = number of trees

###################
## Random Forest ##
###################
for(i in 1:10){
  #generate model
  model <- randomForest::randomForest(formulaClass, data=balancedData[trainPartitions[[i]], ],
                                      ntree=ntrees)
  #predict over test fold
  predictions <- predict(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  aucPred[i] <- auc(predictions[1])
}
#Predict on KAGGLE test data
model <- randomForest::randomForest(formulaClass, data=balancedData, ntree=ntrees)
kagglePrediction <- predict(model, newdata = testData[, -1])
