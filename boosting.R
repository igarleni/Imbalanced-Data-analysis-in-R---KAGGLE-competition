#
library(rpart)
library(adabag)

#IN variables:
# maxdp = 2
# finalm = 10

##############
## Boosting ##
##############
#Algorithm
for(i in 1:10){
  print(i)
  #generate model
  model <- adabag::boosting(formulaClass, 
                            data = balancedData[trainPartitions[[i]], ],
                            mfinal = finalm, 
                            control = rpart::rpart.control(maxdepth = maxdp))
  #predict over test fold
  predictions <- adabag::predict.boosting(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions$class)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- adabag::boosting(formulaClass, data = balancedData, mfinal = finalm, 
                          control = rpart::rpart.control(maxdepth = maxdp))
boostingPrediction <- adabag::predict.boosting(model, newdata = as.data.frame(testData[, -1]))
kagglePrediction <- boostingPrediction$prob