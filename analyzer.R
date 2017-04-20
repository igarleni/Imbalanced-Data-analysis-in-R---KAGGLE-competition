#

##################################
## Data preprocessing and setup ##
##################################
#Clear workspace
rm(list = ls())
#Read data and setup for imbalanced algorithms
source("dataSetup.r")
#Show imbalance ratio
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
formulaClassSelected <- as.formula(paste(classVariable, "~ MATHEFF + ESCS + ANXMAT + SCMAT +
                                         misced + SMATBEH + fisced + CLCUSE1 + INTMAT + FAILMAT
                                         + ST15Q01", sep = ""))
formulaClassSelectedMultiplying <- as.formula(paste(classVariable, "~ MATHEFF * ESCS * ANXMAT *
                                                    SCMAT * misced * SMATBEH * fisced *
                                                    CLCUSE1 * INTMAT * FAILMAT *ST15Q01",
                                                    sep = ""))
formulaClassAll <- as.formula(paste(classVariable, "~.", sep = ""))
#choose between variable selection or full variables
formulaClass <- formulaClassAll


####################################
## predictions with probabilities ##
####################################
library(unbalanced)

#Test 16
###OSS (IR 1.583333) + SVM (default mode)
data <- ubOSS(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on KAGGLE <- 0.82607

#Test 17
###Tomek (IR 1.586996) +SVM (default mode)
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on KAGGLE <- 0.82342

#Test 18
###Tomek (IR 1.428571) +SVM
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("multipleImbalanced.R")
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on KAGGLE <- 0.81818

#Test 19
#OSS (IR 1.56685) + SVM
data <- ubOSS(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on KAGGLE <- 0.82631

#Test 20
#Tomek (IR 1.586996) + SVM with variable selection
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassSelected
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on 10cfv = 0.8184077
##accuracy on KAGGLE <- 0.81833

#Test 21
#Tomek (IR 1.586996) + SVM with variable selection multiplying
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassSelectedMultiplying
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R") 
#accuracy on 10cfv = 0.8909751
##accuracy on KAGGLE <- 0.76495

#Test 22
#Tomek (IR 1.586996) + randomForest with variable selection multiplying
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassSelectedMultiplying
ntrees = 50
source("randomForest.R") 
#accuracy on 10cfv = 0.7766337
##accuracy on KAGGLE <- 0.72226

#Test 23 (start using kernlab)
#Tomek (IR 1.586996) + SVM kernlab library and variable selection
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassSelected
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on 10cfv = 1
##accuracy on KAGGLE <- 0.79567

#Test 24
#Undersampling 38.69 (IR 1.584249) + boosting
data <- ubUnder(X = input, Y = output, perc = 38.69, method = "percPos", w = NULL)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
maxdp = 2
finalm = 10
source("boosting.R")
#accuracy on 10cfv = 0.7547634
##accuracy on KAGGLE <- 0.79754

#Test 25
#Undersampling 38.69 (IR 1.584249) + SVM
data <- ubUnder(X = input, Y = output, perc = 38.69, method = "percPos", w = NULL)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on 10cfv = 1
##accuracy on KAGGLE <- 0.82467

#Test 26
###Tomek multiple (IR 1.232601) + SVM
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("multipleImbalanced.R")
data <- ubTomek(X = input, Y = output, verbose = TRUE) #not only 2 times, more than 2
source("imbalancePostprocessing.R")
formulaClass <- formulaClassSelected
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on CFV <- 1 
#accuracy on KAGGLE <- 0.80667

#Test 27
###Tomek (IR 1.586996) + SVM
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.80667

#Test 28
###Tomek (IR 1.586996) + RandomForest 100 trees
data <- ubTomek(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
ntrees = 100
source("randomForest.R") 
#accuracy on CFV <- 0.8021079
#accuracy on KAGGLE <- 0.72000

#Test 29
##Undersampling (IR 1.564103) and Oversampling (IR 1) + SVM
data <- ubUnder(X = input, Y = output, perc = 50, method = "percPos", w = NULL)
source("multipleImbalanced.R")
data <- ubOver(X = input, Y = output, k = 0, verbose=TRUE)
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "rbfdot"
cSVM = 1
source("svm.R")
#accuracy on KAGGLE <- 0.81617

#Test 30
###OSS (IR 1.586996) + SVM all variables polydot c-svc
data <- ubOSS(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "polydot"
cSVM = 1
source("svm.R")
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82745 

#Test 31
###OSS (IR 1.586996) + SVM all variables polydot c-svc c=1.1
data <- ubOSS(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "polydot"
cSVM = 1.1
source("svm.R")
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82746(BEST ONE)

#Test 32
###OSS (IR 1.586996) + SVM all variables polydot c-svc c=2
data <- ubOSS(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "polydot"
cSVM = 2
source("svm.R")
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82744

#Test 33
###OSS (IR 1.586996) + SVM all variables polydot c-svc c=0.5
data <- ubOSS(X = input, Y = output, verbose = TRUE)
source("imbalancePostprocessing.R")
formulaClass <- formulaClassAll
typeKSVM = "C-svc"
kernelSVM = "polydot"
cSVM = 0.5
source("svm.R")
#accuracy on CFV <- 1
#accuracy on KAGGLE <- 0.82742


#########################
## Results performance ##
#########################
#AUG meassure
aucPred <- mean(aucPred)


############################
## Generate KAGGLE output ##
############################
kagglePrediction.final <- cbind(testData[,1], kagglePrediction)
kagglePrediction.final <- kagglePrediction.final[,c(1,2)]
colnames(kagglePrediction.final) <- c("Id","Prediction")
write.table(kagglePrediction.final, file = "kagglePrediction.csv", quote = FALSE, sep = ",",
            row.names = FALSE)

########################################
## MODELS TESTED (before prob models) ##
########################################

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
