#

library(kernlab)

######
## SVM
######
for(i in 1:10){
  #generate model
  model <- train(PV1MATH ~ ., data = balancedData[testPartitions[[i]], ], method = "svmRadial",
                 preProc = c("center", "scale"),
                 prob.model =  TRUE)
  #predict over test fold
  predictions <- predict(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- train(PV1MATH ~ ., 
               data = balancedData, 
               method = "svmRadial", preProc = c("center", "scale"),
               prob.model=TRUE)
kagglePrediction <- predict(model, newdata = testData[, -1], type = "prob")
