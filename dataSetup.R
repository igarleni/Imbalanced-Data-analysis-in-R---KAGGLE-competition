#

#Read datafiles
trainData <- read.csv("pv1math-tra.csv")
testData <- read.csv("pv1math-tst.csv")

#Imbalance ratio
nClass1 <- sum(trainData$PV1MATH == 1)
nClass0 <- sum(trainData$PV1MATH == 0)
IR <- nClass0 / nClass1

#Convert class into factor
trainData$PV1MATH <- factor(trainData$PV1MATH)

#separate output and input for imbalanced algorithms
n <- ncol (trainData)
output <- factor(trainData$PV1MATH)
input <- trainData[-n]
