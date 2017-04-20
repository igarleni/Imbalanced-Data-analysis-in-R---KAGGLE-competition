#
library(kernlab)
#https://cran.r-project.org/web/packages/kernlab/kernlab.pdf

#IN variables:
# typeKSVM = "C-svc" (Classification --> C-svc, nu-svc, C-bsvc, spoc-svc, kbb-svc, one-svc
#     Regression --> eps-svr, nu-svr, eps-bsvr)
# kernelSVM = "polydot"
# cSVM = 1

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
  aucPred[i] <- auc(predictions[1])
}
#Predict on KAGGLE test data
model <- ksvm(formulaClass,data=balancedData, 
              scaled = TRUE, type = typeKSVM,
              kernel = kernelSVM,
              C = cSVM,
              prob.model=TRUE)
kagglePrediction <- predict(model, testData[, -1], type = "probabilities")
