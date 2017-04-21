#

###############
## basic SVM ##
###############
for(i in 1:10){
  #generate model
  model <- train(formulaClass,data=balancedData[testPartitions[[i]], ],
                 method = "svmRadial", preProc = c("center", "scale"),
                 prob.model=TRUE)
  #predict over test fold
  predictions <- predict(model,balancedData[testPartitions[[i]], -n],type="prob")
  #Save statistics
  aucPred[i] <- auc(balancedData[testPartitions[[i]], n],as.vector(predictions[,1]))
}
#Predict on KAGGLE test data
model <- train(formulaClass,data=balancedData, 
              method = "svmRadial", preProc = c("center", "scale"),
              prob.model=TRUE)
kagglePrediction <- predict(model, testData[, -1], type = "prob")
