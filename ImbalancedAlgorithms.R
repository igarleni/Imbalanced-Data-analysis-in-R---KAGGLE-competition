#

#script reference, not for ecxecuting

##########################
## Imbalance algorithms ##
##########################
library(unbalanced)
#separate output and input for imbalanced algorithms
n <- ncol (trainData)
output <- factor(trainData$PV1MATH)
input <- trainData[-n]

####UNIQUE ALGORITHM:
#just choose and execute
####MULTIPLE ALGORITHMS:
#execute {Multiple algorithm postprocessing} between imbalanced algorithms
####POSTPROCESSING:
#execute {Imbalance postprocessing} when you are done

#####
#Algorithms
#####
#Oversampling
data <- ubOver(X = input, Y = output, k = 0, verbose=TRUE)
#Undersampling --> perc-->50 default
data <- ubUnder(X = input, Y = output, perc = 50, method = "percPos", w = NULL)
#SMOTE
data <- ubSMOTE(X= input, Y=output, k = 5, perc.over=200, perc.under=200, verbose=TRUE)
#OSS
data <- ubOSS(X = input, Y = output, verbose = TRUE)
#CNN
data <- ubCNN(X = input, Y = output, k = 1, verbose = TRUE)
#ENN
data <- ubENN(X = input, Y = output, k = 3, verbose = TRUE)
#NCL
data <- ubNCL(X = input, Y = output, k = 3, verbose = TRUE)
#Tomek
data <- ubTomek(X = input, Y = output, verbose = TRUE)

############
## Multiple algorithm postprocessing
############
source("multipleImbalanced.R")
#Show IR
IR

#############
## Imbalance postprocessing
#############
source("imbalancePostprocessing.R")
#Show imbalance ratio IR
IR