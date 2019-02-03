##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment

## Read the data
xTrain=read.csv("ecoli_xTrain.csv")
yTrain=read.csv("ecoli_yTrain.csv")
xTest=read.csv("ecoli_xTest.csv")
yTest=read.csv("ecoli_yTest.csv")


#### Part 1 ####
logProd <- function(x){
  
}

logSum <- function(x){
  
}

#### Part 2 ####
prior <- function(yTrain){
  
}

likelihood <- function(xTrain, yTrain){
  
}

naiveBayesClassify <- function(xTest, M, V, p){
  
}

#### Part 3 ####
sigmoidProb <- function(y, x, w){
  
}

logisticRegressionWeights <- function(xTrain, yTrain, w0, nIter){
  
}

logisticRegressionClassify <- function(xTest, w){
  
}