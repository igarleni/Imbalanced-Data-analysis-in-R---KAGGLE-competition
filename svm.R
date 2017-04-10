#

library(kernlab)

#https://cran.r-project.org/web/packages/kernlab/kernlab.pdf
#########
## SVM ##
#########
for(i in 1:10){
  #generate model
  model <- ksvm(formulaClass,data=balancedData[testPartitions[[i]], ],
                scaled = TRUE, type = typeKSVM,
                kernel = kernelSVM,
                C = cSVM,
                prob.model=TRUE)
  #predict over test fold
  predictions <- predict(model,balancedData[testPartitions[[i]], -n],type="probabilities")
  #Save statistics
  for(j in 1:length(predictions)){
    if (predictions[j] < 0.5)
      predictions[j] = 0
    else
      predictions[j] = 1
  }
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- ksvm(formulaClass,data=balancedData, 
              scaled = TRUE, type = typeKSVM,
              kernel = kernelSVM,
              C = cSVM,
              prob.model=TRUE)
kagglePrediction <- predict(model, testData[, -1], type = "probabilities")
