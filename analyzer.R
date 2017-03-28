#

#...##...############...##...#
#..#..#..## README ##..#..#..#
#.#....#.############.#....#.#

#1.-Execute {Data preprocessing and setup}

#2.-Choose one or many {Imbalance algorithms}, choose your input variables and execute them

#3.-Execute {Data preprocessing and setup}

#4.-Choose one of {Classification algorithms} and execute it

#5.-Watch results with {Results performance}

#6.-Generate KAGGLE test prediction file using {Generate KAGGLE output}

##
##At the end of this script are different models tested and its accuracy by using this process.
##

##################################
## Data preprocessing and setup ##
##################################

#
library(readr)
library(unbalanced)
library(class)
library(caret)

#Read file
pv1math_train <- read_csv("pv1math-tra.csv")
pv1math_test <- read_csv("pv1math-tst.csv")

#Imbalance ratio
nClass1 <- sum(pv1math_train$PV1MATH == 1)
nClass0 <- sum(pv1math_train$PV1MATH == 0)
IR <- nClass0 / nClass1
IR

#Convert class into factor
pv1math_train$PV1MATH <- factor(pv1math_train$PV1MATH)

#separate output and input
n <- ncol (pv1math_train)
output <- factor(pv1math_train$PV1MATH)
input <- pv1math_train[-n]


##########################
## Imbalance algorithms ##
##########################

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
balancedData<-cbind(data$X,data$Y)
colnames(balancedData) <- colnames(pv1math_train)
output <- factor(balancedData$PV1MATH)
input <- balancedData[-n]

##Imbalance ratio of balanced data
nClass1 <- sum(balancedData$PV1MATH == 1)
nClass0 <- sum(balancedData$PV1MATH == 0)
IR <- nClass0 / nClass1
IR

##############################
## Imbalance postprocessing ##
##############################

##Data postprocessing
balancedData<-cbind(data$X,data$Y)
colnames(balancedData) <- colnames(pv1math_train)

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
errors <- c()
hits <- c()


###############################
## Classification algorithms ##
###############################

########
## Linear Regression
########
balancedData$PV1MATH = as.numeric(balancedData$PV1MATH)
for(i in 1:10){
  #generate model
  model <-lm(PV1MATH~., data= balancedData[trainPartitions[[i]], ])
  #predict over test fold
  predictions <- predict(model, newdata = balancedData [testPartitions[[i]], -n])
  for(j in 1:length(predictions)){
    if (predictions[j] < 0.5)
      predictions[j] = 0
    else
      predictions[j] = 1
  }
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <-lm(PV1MATH~., data= balancedData)
kagglePrediction <- predict(model, newdata = pv1math_test[, -1])

########
## Generalized Lineal models GLM
########
#http://www.statmethods.net/advstats/glm.html
balancedData$PV1MATH = as.numeric(balancedData$PV1MATH)
for(i in 1:10){
  #generate model
  model <-glm(PV1MATH~., data= balancedData[trainPartitions[[i]], ])
  #predict over test fold
  predictions <- predict(model, newdata = balancedData [testPartitions[[i]], -n])
  for(j in 1:length(predictions)){
    if (predictions[j] < 0.5)
      predictions[j] = 0
    else
      predictions[j] = 1
  }
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <-glm(PV1MATH~., data= balancedData)
kagglePrediction <- predict(model, newdata = pv1math_test[, -1])

######
## KNN
######
#Variable input
kn = 1
#Algorithm
for(i in 1:10){
  #predict over test fold
  predictions <- knn(train = balancedData[trainPartitions[[i]], -n],
                     test = balancedData[testPartitions[[i]], -n],
                     cl = balancedData[trainPartitions[[i]], n], k=kn)
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
kagglePrediction <- knn(train = balancedData[, -n], test = pv1math_test[, -1],
                        cl = balancedData[, n], k=kn)

######
## Conditional inference Classification Tree
######
library(party)
#Algorithm
variableClass <- names(balancedData)[n]
formulaClass <- as.formula(paste(variableClass,"~.",sep=""))
for(i in 1:10){
  #generate model
  model <- ctree(formulaClass, balancedData[trainPartitions[[i]], ])
  #predict over test fold
  predictions <- predict(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- ctree(formulaClass, balancedData)
kagglePrediction <- predict(model, newdata = pv1math_test[, -1])


######
## Bagging
######
library(rpart)
library(adabag)
#Variables input
maxdp = 5
minsplt = 15
#Algorithm
for(i in 1:10){
  #generate model
  model <- adabag::bagging(PV1MATH ~ ., 
                            data = balancedData[trainPartitions[[i]], ], 
                            control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
  #predict over test fold
  predictions <- adabag::predict.bagging(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions$class)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- adabag::bagging(PV1MATH ~ ., data = balancedData, 
                         control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
baggingPrediction <- adabag::predict.bagging(model, newdata = as.data.frame(pv1math_test[, -1]))
kagglePrediction <- baggingPrediction$class

######
## Boosting
######
library(rpart)
library(adabag)
#Variables input
maxdp = 2
finalm = 10
#Algorithm
for(i in 1:10){
  print(i)
  #generate model
  model <- adabag::boosting(PV1MATH ~ ., 
                            data = balancedData[trainPartitions[[i]], ],
                            mfinal = finalm, 
                            control = rpart::rpart.control(maxdepth = maxdp))
  #predict over test fold
  predictions <- adabag::predict.boosting(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions$class)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- adabag::boosting(PV1MATH ~ ., data = balancedData, mfinal = finalm, 
                          control = rpart::rpart.control(maxdepth = maxdp))
boostingPrediction <- adabag::predict.boosting(model, newdata = as.data.frame(pv1math_test[, -1]))
kagglePrediction <- boostingPrediction$class


######
## Random Forest
######
library(randomForest)
#Variables input
ntrees = 100
#Algorithm
for(i in 1:10){
  #generate model
  model <- randomForest::randomForest(PV1MATH ~ ., data=balancedData[trainPartitions[[i]], ],
                                      ntree=ntrees)
  #predict over test fold
  predictions <- predict(model, newdata = balancedData[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- randomForest::randomForest(PV1MATH ~ ., data=balancedData, ntree=ntrees)
kagglePrediction <- predict(model, newdata = pv1math_test[, -1])

######
## SVM
######
library(kernlab)
#Algorithm
for(i in 1:10){
  #generate model
  model <- train(PV1MATH ~ ., data = balancedData[testPartitions[[i]], ], method = "svmRadial",
                 preProc = c("center", "scale"),
                 classProbs =  TRUE)
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
kagglePrediction <- predict(model, newdata = pv1math_test[, -1], type = "prob")


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

kagglePrediction.final <- cbind(pv1math_test[,1], kagglePrediction)
colnames(kagglePrediction.final) <- c("Id","Prediction")
write.table(kagglePrediction.final, file = "kagglePrediction.csv", quote = FALSE, sep = ",",
            row.names = FALSE)


########################################
## Generate KAGGLE output (prob mode) ##
########################################

kagglePrediction.final <- cbind(pv1math_test[,1], kagglePrediction)
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
