#

#...##...############...##...#
#..#..#..## README ##..#..#..#
#.#....#.############.#....#.#

#1.-Execute {Data preprocessing and setup}

#2.-Choose one or many {Imbalance algorithms}, choose your input variables and execute them

#3.-Execute {Data preprocessing and setup}

#4.-Choose one of {Classification algorithm} and execute it

#5.-Watch results with {Results performance}

#6.-Apply over KAGGLE test dataset using {Generate KAGGLE output}

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


##############################
## Imbalance postprocessing ##
##############################

##Data postprocessing
balancedData<-cbind(data$X,data$Y)
colnames(balancedData) <- colnames(pv1math_train)

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


######
## KNN
######
#Variable input
kn = 1
#Algorithm
for(i in 1:10){
  #predict over test fold
  predictions <- knn(train = balancedData[trainPartitions[[i]], -n], test = balancedData[testPartitions[[i]], -n],
                        cl = balancedData[trainPartitions[[i]], n], k=kn)
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}

##########
## Bagging
##########
for(i in 1:10){
  #Generate model
  model <- adabag::bagging(Class ~ ., 
                            data = Vehicle[trainPartitions[[i]], ], 
                            control=rpart::rpart.control(maxdepth=5, minsplit=15))
  #predict over test fold
  predictions <- adabag::predict.bagging(modelo, 
                                          newdata=Vehicle[testPartitions[[i]], -n ])
  
  #Save statistics
  hits[i] <- sum(balancedData[testPartitions[[i]], n] == predictions)
  errors[i] <- length(balancedData[testPartitions[[i]], n]) - hits[i]
}



#########################
## Results performance ##
#########################

#Accuracy
accuracy <- mean(hits/(errors+hits))
accuracy

#others
acc <- sum((pv1math_train$PV1MATH[as.vector(CVperm)] == 0 & knn.pred == 1) 
           | (pv1math_train$PV1MATH[as.vector(CVperm)] == 1 & knn.pred == 2)) / (nClass0 + nClass1)
tpr <- sum(pv1math_train$PV1MATH[as.vector(CVperm)] == 0 & knn.pred == 1) / nClass0
tnr <- sum(pv1math_train$PV1MATH[as.vector(CVperm)] == 1 & knn.pred == 2) / nClass1
gmean <- sqrt(tpr * tnr)


############################
## Generate KAGGLE output ##
############################





###################
## MODELS TESTED ##
###################

###SMOTE + KNN
#SMOTE <- perc.over=200, perc.under=200
#KNN <- k = 1
#accuracy on CFV <- 0.9621914
#accuracy on KAGGLE <- 


