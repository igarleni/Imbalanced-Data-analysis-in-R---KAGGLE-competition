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
data <- ubUnder(X = input, Y = output, perc = 39, method = "percPos", w = NULL)
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
#MATHEFF * ESCS * ANXMAT * SCMAT * misced * SMATBEH * fisced * CLCUSE1 * INTMAT * FAILMAT *ST15Q01
#create formula
idClass <- length(names(trainData))
classVariable <- names(trainData)[idClass]
formulaClassSelected <- as.formula(paste(classVariable, "~ MATHEFF + ESCS + ANXMAT + SCMAT + misced", sep = ""))
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
ntrees = 100
source("randomForest.r")

## SVM
#type Classification --> C-svc, nu-svc, C-bsvc, spoc-svc, kbb-svc, one-svc
#type Regression --> eps-svr, nu-svr, eps-bsvr
typeKSVM = "C-svc"
#
kernelSVM = "polydot"
#default = 1
cSVM = 0.5
source("svm.R")


#########################
## Results performance ##
#########################
#Accuracy
accuracy <- mean(hits/(errors+hits))
accuracy


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

#Tomek (IR 1.586996) + SVM with variable selection
#accuracy on 10cfv = 0.8184077
##accuracy on KAGGLE <- 0.81833

#Tomek (IR 1.586996) + SVM with variable selection multiplying
#accuracy on 10cfv = 0.8909751
##accuracy on KAGGLE <- 0.76495

#Tomek (IR 1.586996) + randomForest with variable selection multiplying
#accuracy on 10cfv = 0.7766337
##accuracy on KAGGLE <- 0.72226

#Tomek (IR 1.586996) + SVM kernlab library and variable selection
#accuracy on 10cfv = 1
##accuracy on KAGGLE <- 0.79567

#Undersampling 38.69 (IR 1.584249) + boosting
#accuracy on 10cfv = 0.7547634
##accuracy on KAGGLE <- 0.79754

#Undersampling 38.69 (IR 1.584249) + SVM
#accuracy on 10cfv = 1
##accuracy on KAGGLE <- 0.82467

###Tomek multiple (IR 1.232601) + SVM
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.80667

###Tomek (IR 1.586996) + SVM
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.80667

###Tomek (IR 1.586996) + RandomForest 100 trees
#accuracy on CFV <- 0.8021079
#accuracy on KAGGLE <- 0.72000

##Undersampling (IR 1.564103) and Oversampling (IR 1) + SVM
#accuracy on KAGGLE <- 0.81617

###OSS (IR 1.586996) + SVM all variables polydot c-svc
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82745 

###OSS (IR 1.586996) + SVM all variables polydot c-svc c=1.1
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82746(BEST ONE)

###OSS (IR 1.586996) + SVM all variables polydot c-svc c=2
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82744

###OSS (IR 1.586996) + SVM all variables polydot c-svc c=0.5
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82742