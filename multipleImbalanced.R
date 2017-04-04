#

# Multiple algorithm postprocessing
balancedData<-cbind(data$X,data$Y)
colnames(balancedData) <- colnames(balancedData)
output <- factor(balancedData$PV1MATH)
input <- balancedData[-n]

##Imbalance ratio of balanced data
nClass1 <- sum(balancedData$PV1MATH == 1)
nClass0 <- sum(balancedData$PV1MATH == 0)
IR <- nClass0 / nClass1
IR