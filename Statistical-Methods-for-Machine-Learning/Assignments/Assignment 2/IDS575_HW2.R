rm(list=ls()) 

library(tidyverse) 
require(Matrix)
library(caret)
library(e1071)
library(SparseM)
library(lubridate)
library(dplyr)
library(ggplot2)


#5a) Train Data
# Change to the working directory where the data files are located.
# TODO: You should change the following ... to your working directory
# setwd("C:/Users/Chaitu/Desktop/UIC/Fall2020Classes/IDS575 - MachineLearning/hw2")
setwd('C:/Users/cjall/OneDrive/Desktop/Grad School/IDS 575/Assignments/hw2/hw2')
# Read all individual lines in a text file.
# m = the number of training examples
dataFile <- file("articles.train", "r")
dataFile1 <- file("articles.test", "r")
dataLines <- readLines(dataFile)
m <- length(dataLines)
close(dataFile)

# Split every string element by tokenizing space and colon.
dataTokens = strsplit(dataLines, "[: ]")

# Extract every first token from each line as a vector of numbers, which is the class label.
Y = sapply(dataTokens, function(example) {as.numeric(example[1])})
str(Y)
unique(Y, incomparables = FALSE, fromLast = FALSE,
       nmax = NA)

# Extract the rest of tokens from each line as a list of matrices (one matrix for each line)
# where each row consists of two columns: (feature number, its occurrences)
X_list = lapply(dataTokens, function(example) 
{n = length(example) - 1; matrix(as.numeric(example[2:(n+1)]), ncol=2, byrow=T)})


# Add one column that indicates the example number at the left
X_list = mapply(cbind, x=1:length(X_list), y=X_list)

# Merge a list of different examples vertcially into a matrix
X_data = do.call('rbind', X_list)

# Get a sparse data matrix X (rows: training exmaples, columns:
# of occurrences for each of features)
X = sparseMatrix(x=X_data[,3], i=X_data[,1], j=X_data[,2])

##Test Data
dataFile_test <- file("articles.test", "r")
dataLines_test <- readLines(dataFile_test)
m_test <- length(dataLines_test)
close(dataFile_test)

# Split every string element by tokenizing space and colon.
dataTokens_test = strsplit(dataLines_test, "[: ]")

# Extract every first token from each line as a vector of numbers, which is the class label.
Y_test = sapply(dataTokens_test, function(example) {as.numeric(example[1])})
str(Y_test)
unique(Y_test, incomparables = FALSE, fromLast = FALSE,
       nmax = NA)

# Extract the rest of tokens from each line as a list of matrices (one matrix for each line)
# where each row consists of two columns: (feature number, its occurrences)
X_list_test = lapply(dataTokens_test, function(example) 
{n = length(example) - 1; matrix(as.numeric(example[2:(n+1)]), ncol=2, byrow=T)})

# Add one column that indicates the example number at the left
X_list_test = mapply(cbind, x=1:length(X_list_test), y=X_list_test)

# Merge a list of different examples vertcially into a matrix
X_data_test = do.call('rbind', X_list_test)

# Get a sparse data matrix X (rows: training exmaples, columns:
# of occurrences for each of features)
X_test = sparseMatrix(x=X_data_test[,3], i=X_data_test[,1], j=X_data_test[,2])


#Train data converting the target variable into binary
Ytrain <- Y 
Y1train <- as.factor(ifelse(Ytrain=='1', 1, -1))
Y2train <- as.factor(ifelse(Ytrain=='2', 1, -1))
Y3train <- as.factor(ifelse(Ytrain=='3', 1, -1))
Y4train <- as.factor(ifelse(Ytrain=='4', 1, -1))
unique(Y)
unique(Y4train)

# Test Data converting the target variable into binary
Ytest <- Y_test
Y1test <- as.factor(ifelse(Y_test=='1', 1, -1))
Y2test <- as.factor(ifelse(Y_test=='2', 1, -1))
Y3test <- as.factor(ifelse(Y_test=='3', 1, -1))
Y4test <- as.factor(ifelse(Y_test=='4', 1, -1))

#Adjusting test and train data to equal features
dim(X)
summary(X)
dim(X_test)
Xtest <- X_test[,1:51949]
Xtest 
dim(Xtest)

##5b)
#Model1 Operating Systems train
operatingSVM <-svm(x=X, y = Y1train, kernel = "linear", probability = TRUE)
summary(operatingSVM)
pred1<- predict(operatingSVM, X)
tab <- table(Predicted = pred1, Y1train)
confusionMatrix(pred1, Y1train)

#Operating Systems test
pred1_test<- predict(operatingSVM, Xtest)
confusionMatrix(pred1_test, Y1test)

#Model2 Vehicles train
vehicleSVM <- svm(x=X, y = Y2train, kernel = "linear", probability = TRUE)
summary(vehicleSVM)
pred2<- predict(vehicleSVM, X)
tab <- table(Predicted = pred2, Y2train)
tab
confusionMatrix(pred2, Y2train)

#Vehicles test
pred2_test<- predict(vehicleSVM, Xtest)
confusionMatrix(pred2_test, Y2test)

#Model3 Sports
sportSVM <- svm(x=X, y = Y3train, kernel = "linear", probability = TRUE)
summary(sportSVM)
pred3<- predict(sportSVM, X,  probability = TRUE)
tab <- table(Predicted = pred3, Y3train)
tab
confusionMatrix(pred3, Y3train)

#Sports test
pred3_test<- predict(sportSVM, Xtest, probability = TRUE)
confusionMatrix(pred3_test, Y3test)

#Model4 Politics
politicsSVM<- svm(x=X, y = Y4train, kernel = "linear", probability = TRUE)
summary(politicsSVM)
pred4<- predict(politicsSVM, X, probability = TRUE)
confusionMatrix(pred4, Y4train)

#Politics Test
pred4_test<- predict(politicsSVM, Xtest, probability = TRUE)
confusionMatrix(pred4_test, Y4test)

#########5c
set.seed(1)
nr<-nrow(X)
trnIndex<- sample(1:nr, size = round(0.75*nr), replace=FALSE)
svmTrn <- X[trnIndex, ]
svmTst <- X[-trnIndex, ]

str(svmTrn)
Y1trainc <- Y1train[trnIndex]
Y1validc <- Y1train[-trnIndex]
Y2trainc <- Y2train[trnIndex]
Y2validc <- Y2train[-trnIndex]
Y3trainc <- Y3train[trnIndex]
Y3validc <- Y3train[-trnIndex]
Y4trainc <- Y4train[trnIndex]
Y4validc <- Y4train[-trnIndex]
unique(Y1trainc)
Ytrainc <- Ytrain[trnIndex]
Yvalidc <- Ytrain[-trnIndex]

##Using the tune.svm to find the better/optimized C value on training
set.seed(1)
svmfit1 <- tune.svm(x= svmTrn,y = factor(Y1trainc), kernel="linear",
                 cost = c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
svmfit1
model1 <- summary(svmfit1)  
summary(svmfit1) 
plot(svmfit1$error)

svmfit2t <- tune.svm(x= svmTrn,y = factor(Y2trainc), kernel="linear",
                    cost = c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfit2t)

svmfit3 <- tune.svm(x= svmTrn,y = factor(Y3trainc),kernel="linear",
                 cost =c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfit3)

svmfit4 <- tune.svm(x= svmTrn,y = factor(Y4trainc), kernel="linear",
                 cost =c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfit4)


##Using the tune.svm to find the better/optimized C value for validation data
set.seed(1)
svmfitv1 <- tune.svm(x= svmTrn,y = factor(Y1validc), kernel="linear",
                    cost = c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
svmfitv1
summary(svmfit1v1) 


svmfitv2t <- tune.svm(x= svmTrn,y = factor(Y2validc), kernel="linear",
                     cost = c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfitv2t)

svmfitv3 <- tune.svm(x= svmTrn,y = factor(Y3validc),kernel="linear",
                    cost =c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfitv3)

svmfitv4 <- tune.svm(x= svmTrn,y = factor(Y4validc), kernel="linear",
                    cost =c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfitv4)

#Validation
Cost =c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512)
cc <- log2(Cost)

#classifier1 plot
plot(cc, svmfit1$error, type="l", col="blue", ylab="ErrorClassifier1", xlab="Log_Cost")
lines(cc, svmfitv1$error, col="orange")
grid()
legend("topright", legend=co, lty = c(1,2), col=c("blue", "orange"))

#classifier2 plot
plot(cc, svmfit2t$error, type="l", col="blue", ylab="ErrorClassifier2", xlab="Log_Cost")
lines(cc, svmfitv2t$error, col="orange")
grid()
legend("topright", legend=co, lty = c(1,2), col=c("blue", "orange"))

#classifier3 plot
plot(cc, svmfit3$error, type="l", col="blue", ylab="ErrorClassifier3", xlab="Log_Cost")
lines(cc, svmfitv3$error, col="orange")
grid()
legend("topright", legend=co, lty = c(1,2), col=c("blue", "orange"))

#classifier4 plot
plot(cc, svmfit4$error, type="l", col="blue", ylab="ErrorClassifier4", xlab="Log_Cost")
lines(cc, svmfitv4$error, col="orange")
grid()
legend("topright", legend=co, lty = c(1,2), col=c("blue", "orange"))


#Overall Validation Data
svmfitval <- tune.svm(x= svmTst,y = factor(Yvalidc), kernel="linear",
                      cost =c(0.125, 0.5, 9, 27, 100, 275, 320, 450, 512), probability = TRUE)
summary(svmfitval)

#5d
#Train on soft classifier1 for all data 
svmd1 <- svm(X, Y1train, kernel = "linear", cost=0.125, probability = TRUE) 
# Prediction on Train
predtraind1 <- predict(svmd1, X, probability =  TRUE)
confusionMatrix(predtraind1 , Y1train) 
# Prediction on Test
predtestd1 <- predict(svmd1, Xtest, probability =  TRUE)
confusionMatrix(predtestd1 , Y1test) 

#Train on soft classifier2 for all data
svmd2 <- svm(X, Y2train, kernel = "linear", cost=0.125, probability = TRUE) 
# Prediction on Train
predtraind2 <- predict(svmd2, X, probability =  TRUE)
confusionMatrix(predtraind2 , Y2train) 
# Prediction on Test
predtestd2 <- predict(svmd2, Xtest, probability =  TRUE)
confusionMatrix(predtestd2 , Y2test) 

#Train on soft classifier3 for all data
svmd3 <- svm(X, Y3train, kernel = "linear", cost=0.125, probability = TRUE) 
# Prediction on Train
predtraind3 <- predict(svmd3, X, probability =  TRUE)
confusionMatrix(predtraind3 , Y3train) 
# Prediction on Test
predtestd3 <- predict(svmd3, Xtest, probability =  TRUE)
confusionMatrix(predtestd3 , Y3test) 


#Train on soft classifier4 for all data
svmd4 <- svm(X, Y4train, kernel = "linear", cost=0.125, probability = TRUE) 
# Prediction on Train
predtraind4 <- predict(svmd4, X, probability =  TRUE)
confusionMatrix(predtraind4 , Y4train) 
# Prediction on Test
predtestd4 <- predict(svmd4, Xtest, probability =  TRUE)
confusionMatrix(predtestd4 , Y4test) 


##5e
# We Normalized the feature vectors using the rowNorms function in the wordspace library in R

library(wordspace)
row_normalized<-normalize.rows(X,method="euclidean",p=2)

#Training the Soft Margin classifier 1 with Normalized feature space
svme1<- svm(row_normalized, Y1train, kernel = "linear", cost=0.125, probability = TRUE) 
svme1

# Predicting on Test
preds1N <- predict(svme1, Xtest, probability =  TRUE)
confusionMatrix(preds1N, Y1test) 

#Training the Soft Margin classifier 2 with Normalized feature space
svme2<- svm(row_normalized, Y2train, kernel = "linear", cost=0.125, probability = TRUE) 
svme2
# Predicting on Test
preds2N <- predict(svme2, Xtest, probability =  TRUE)
confusionMatrix(preds2N, Y2test) 


#Training the Soft Margin classifier 3 with Normalized feature space
svme3<- svm(row_normalized, Y3train, kernel = "linear", cost=0.125, probability = TRUE) 
svme3
# Predicting on Test
preds3N <- predict(svme3, Xtest, probability =  TRUE)
confusionMatrix(preds3N, Y3test) 


#Training the Soft Margin classifier 4 with Normalized feature space
svme4 <- svm(row_normalized, Y4train, kernel = "linear", cost=0.125, probability = TRUE) 
svme4

# Predicting on Test
preds4N <- predict(svme4, Xtest, probability =  TRUE)
confusionMatrix(preds4N, Y4test) 

