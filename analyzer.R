#

#...##...############...##...#
#..#..#..## README ##..#..#..#
#.#....#.############.#....#.#
#1.-Execute {Data preprocessing and setup}
#2.-Choose one or many {Imbalance algorithms}, choose your input variables and execute them
#3.-Execute {Imbalance postprocessing}
#4.-Launch {Variable selection} and watch which variables are better
#5.-Choose one of {Classification algorithms} and execute it
#6.-Watch results with {Results performance}
#7.-Generate KAGGLE test prediction file using {Generate KAGGLE output}
##
##At the end of this script are different models tested and its accuracy by using this process.
##

##################################
## Data preprocessing and setup ##
##################################
#Clear workspace
rm(list = ls())
#Read data and setup for imbalanced algorithms
source("dataSetup.r")
#Show imbalance ratio
IR


##########################
## Imbalance algorithms ##
##########################
library(unbalanced)
####UNIQUE ALGORITHM:
#just choose and execute
####MULTIPLE ALGORITHMS:
#execute {Multiple algorithm postprocessing} between imbalanced algorithms

####
#Algorithms
####
#Oversampling
data <- ubOver(X = input, Y = output, k = 0, verbose=TRUE)
#Undersampling
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


##############################
## Imbalance postprocessing ##
##############################
source("imbalancePostprocessing.R")
#Show imbalance ratio IR
IR


########################
## Variable selection ##
########################
source("variableSelectionAF.R")
#show results
final.weight.ordered
#create formula
idClass <- length(names(trainData))
classVariable <- names(trainData)[idClass]
formulaClassSelected <- as.formula(paste(classVariable, "~ MATHEFF + ESCS + ANXMAT + SCMAT + misced + SMATBEH + fisced + CLCUSE1 + INTMAT + FAILMAT + ST15Q01", sep = ""))
formulaClassAll <- as.formula(paste(classVariable, "~.", sep = ""))
#choose between variable selection or full variables
formulaClass <- formulaClassAll

###############################
## Classification algorithms ##
###############################

## Linear Regression
source("linearRegression.R")

## Generalized Lineal models GLM
source("glm.R")

## KNN
kn = 1
source("kNearestNeighbor.R")

## Conditional inference Classification Tree
source("cicTree.R")

## Bagging
maxdp = 5
minsplt = 15
source("bagging.R")

## Boosting
maxdp = 2
finalm = 10
source("boosting.R")

## Random Forest
ntrees = 50
source("randomForest.r")

## SVM
source("svm.R")


#########################
## Results performance ##
#########################
#Accuracy
accuracy <- mean(hits/(errors+hits))
accuracy

#Others
tpr <- sum(pv1math_train$PV1MATH[as.vector(CVperm)] == 0 & knn.pred == 1) / nClass0
tnr <- sum(pv1math_train$PV1MATH[as.vector(CVperm)] == 1 & knn.pred == 2) / nClass1
gmean <- sqrt(tpr * tnr)


############################
## Generate KAGGLE output ##
############################
kagglePrediction.final <- cbind(testData[,1], kagglePrediction)
kagglePrediction.final <- kagglePrediction.final[,c(1,2)]
colnames(kagglePrediction.final) <- c("Id","Prediction")
write.table(kagglePrediction.final, file = "kagglePrediction.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


###################
## MODELS TESTED ##
###################

###SMOTE + KNN
#SMOTE <- perc.over=200, perc.under=200
#KNN <- k = 1
#accuracy on CFV <- 0.9621914
#accuracy on KAGGLE <- 0.6

###SMOTE + linear regession
#SMOTE <- perc.over=200, perc.under=200
#accuracy on CFV <- 0.57
#accuracy on KAGGLE <-

###Tomek multiple (IR 1.27) + Classification Tree
#accuracy on CFV <- 0.7287119
#accuracy on KAGGLE <- 0.68914

###Tomek multiple (IR 1.209707) + Bagging
#accuracy on CFV <- 0.76382
#accuracy on KAGGLE <- 0.71122

###Tomek multiple (IR 1.217949) + Boosting
#accuracy on CFV <- 0.7580188
#accuracy on KAGGLE <- 0.70798

###Tomek multiple (IR 1.217949) + RandomForest (30 trees)
#accuracy on CFV <- 0.7820201
#accuracy on KAGGLE <- 0.70709

###Tomek multiple (IR 1.217949) + RandomForest (100 trees)
#accuracy on CFV <- 0.7981002
#accuracy on KAGGLE <- 0.71763

###Tomek multiple (IR 1.217949) + RandomForest (500 trees)
#accuracy on CFV <- 0.8001366
#accuracy on KAGGLE <- 0.72588

###OSS multiple (IR 1.239927) + RandomForest (100 trees)
#accuracy on CFV <- 0.8021281
#accuracy on KAGGLE <- 0.72084

###OSS multiple (IR 1.212454) + SVM
#accuracy on CFV <- 0.8538826
#accuracy on KAGGLE <- 0.71485

###OSS multiple (IR 1.425824) + SVM
#accuracy on CFV <- 0.8403523
#accuracy on KAGGLE <- 0.73598

###OSS multiple (IR 1.425824) + RandomForest (100 trees)
#accuracy on CFV <- 0.8048447
#accuracy on KAGGLE <- 0.72774

###Tomek multiple (IR 1.428571) + SVM
#accuracy on CFV <- 0.8442786
#accuracy on KAGGLE <- 0.73598

###OSS (IR 1.582418) + SVM
#accuracy on CFV <- 0.8421792
#accuracy on KAGGLE <- 0.74703


####################################
## predictions with probabilities ##
####################################

###OSS (IR 1.583333) + SVM
#accuracy on KAGGLE <- 0.82607

###Tomek (IR 1.586996) +SVM
#accuracy on KAGGLE <- 0.82342

###Tomek (IR 1.428571) +SVM
#accuracy on KAGGLE <- 0.81818

#OSS (IR 1.56685) + SVM
#accuracy on KAGGLE <- 0.82631
