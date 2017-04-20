#
library(caret)
library(pROC)

##Data postprocessing
balancedData<-cbind(data$X,data$Y)
colnames(balancedData) <- colnames(trainData)

##Imbalance ratio of balanced data
nClass1 <- sum(balancedData$PV1MATH == 1)
nClass0 <- sum(balancedData$PV1MATH == 0)
IR <- nClass0 / nClass1
IR

##5 cross fold validation setup
indexes <- seq(1,nrow(balancedData),by=1)
trainPartitions <- createFolds(indexes, k = 10,
                               returnTrain = TRUE)
testPartitions <- list()
for(i in 1:10){
  testPartitions[[i]] <- indexes[-trainPartitions[[i]]]
}
aucPred <- c()
