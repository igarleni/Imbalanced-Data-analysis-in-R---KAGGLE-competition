#...##...############...##...#
#..#..#..## README ##..#..#..#
#.#....#.############.#....#.#

#1.-Execute {Data preprocessing and setup}

#2.-Choose one or many {Imbalance algorithms}, choose your input variables and execute them

#3.-Execute {Data preprocessing and setup}

#4.-Choose one of {Classification algorithm} and execute it

#5.-Watch results with {Results performance}

#6.-Apply over KAGGLE test dataset using {Generate KAGGLE output}


##################################
## Data preprocessing and setup ##
##################################

#
library(readr)
pv1math_train <- read_csv("pv1math-tra.csv")
pv1math_test <- read_csv("pv1math-tst.csv")
library(unbalanced)

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

#Data postprocessing
balancedData<-cbind(data$X,data$Y)
colnames(balancedData) <- colnames(pv1math_train)

#5 cross fold validation setup
neg <- (1:dim(balancedData)[1])[balancedData$PV1MATH==0]
pos <- (1:dim(balancedData)[1])[balancedData$PV1MATH==1]
CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)
CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)
CVperm <- rbind(CVperm_pos, CVperm_neg)


###############################
## Classification algorithms ##
###############################

#5CFV with 3NN
library(class)
knn.pred = NULL
for( i in 1:5){
  #change this line if you want other clasification function
  predictions <- knn(balancedData[-CVperm[,i], -3], balancedData[CVperm[,i], -3],
                     balancedData[-CVperm[,i], 3], k = 3)
  #
  knn.pred <- c(knn.pred, predictions)
}

#

#########################
## Results performance ##
#########################

acc <- sum((pv1math_train$PV1MATH[as.vector(CVperm)] == 0 & knn.pred == 1) 
           | (pv1math_train$PV1MATH[as.vector(CVperm)] == 1 & knn.pred == 2)) / (nClass0 + nClass1)
tpr <- sum(pv1math_train$PV1MATH[as.vector(CVperm)] == 0 & knn.pred == 1) / nClass0
tnr <- sum(pv1math_train$PV1MATH[as.vector(CVperm)] == 1 & knn.pred == 2) / nClass1
gmean <- sqrt(tpr * tnr)


############################
## Generate KAGGLE output ##
############################


