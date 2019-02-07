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
    
    logProduct=0
    for(i in 1:length(x)){
        logProduct=logProduct+x[i]
    }
    
    return(logProduct)
}

logSum <- function(x){
    
    # use logsum function from https://www.rdocumentation.org/packages/tileHMM/versions/1.0-7/topics/logSum
    # logSummation=logSum(x,y=null,base=0);
    
    sortedX = sort(x, decreasing = TRUE)
    
    x_1 = x[1]
    
    logSecondTerm=0
    for(i in 2:length(x)){
        
        logSecondTerm = logSecondTerm + exp(x[i]-x_1)
    
    }
    
    logTerm=1+logSecondTerm

    logSummation=x_1+log(logTerm)
    
    return(logSummation)
}

###### Testing the functions...............
# testingX=c(0,1.609437,1.386294,2.1972245,2.3978952)
# productOfX=logProd(testingX)
# print(productOfX)
# 
# sumOfX=logSum(testingX)
# print(sumOfX)



