##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment

## Read the data
setwd("/Users/Sherlock/Box\ Sync/PSU\ Spr19/STAT\ 557/project1/datamining_project_1")

xTrain=read.csv("ecoli_new.xTrain.csv",  header = FALSE)
yTrain=read.csv("ecoli_new.yTrain.csv",  header = FALSE)
xTest=read.csv("ecoli_new.xTest.csv",  header = FALSE)
yTest=read.csv("ecoli_new.yTest.csv",  header = FALSE)


#### Part 1 ####
logProd <- function(x){
  logProduct=0
  for(i in 1:length(x)){
    if (!is.infinite(x[i]) && !is.nan(x[i])){
      logProduct=logProduct+x[i]
    }
  }
  return(logProduct)
}

logSum <- function(x){

}

#### Part 2 ####
prior <- function(yTrain){
  #print(yTrain)
  freq<-as.data.frame(table(yTrain))
  v<-freq[[2]]
  sumF<- sum(v)
  vec<-v/sumF

  return(vec)
}

likelihood <- function(xTrain, yTrain){
  combinedMtx<- as.data.frame(cbind(xTrain,yTrain))
  
  dims = dim(combinedMtx)
  nRow = dims[1]
  nCol = dims[2]

  M<-aggregate(combinedMtx[,1:(nCol-1)], combinedMtx[nCol], mean)
  M<-M[,2:(nCol)]
  
  V<-aggregate(combinedMtx[,1:(nCol-1)], combinedMtx[nCol], var)
  V<-V[,2:(nCol)]
  
  M<-data.matrix(M)
  V<-data.matrix(V)
  #print(dim(M))
  #print(M)
  #print(combinedMtx)
  #print(groupBy(combinedMtx, combinedMtx[,6], mean))
  retList<- list("M" = M, "V" = V)
  #print("IS MATRIX")
  #print(is.matrix(retList$M))
  #print(retList)
  return(retList)
}

naiveBayesClassify <- function(xTest, M, V, p){
  xTest<-as.data.frame(xTest)
  xTest<-as.matrix(xTest)

  classification<-vector()
  print(c("nrow in xTEST ",nrow(xTest), " length of p ", length(p)))
  for(j in 1:nrow(xTest))
  {
    lglikelihood<-vector()
    x_j<-as.vector(xTest[j,])
    for(i in 1:length(p))
    {

      M_i <- as.vector(M[i,])
      V_i <- as.vector(V[i,])

      ################# breaking it up ##############
      
      

        V_i <- 1/V_i
        
        x_j_M_i_diff <- (x_j - M_i) ^ 2 * V_i
        p_x_given_y_exp <- x_j_M_i_diff
        
        p_x_given_y_exp = lapply(p_x_given_y_exp, function(x) x[is.finite(x)])
        p_x_given_y_exp = unlist(p_x_given_y_exp, use.names=FALSE)
        #print(p_x_given_y_exp)
        p_x_given_y_var <- logProd(log(V_i))
        p_x_given_y <- (p_x_given_y_var - sum(p_x_given_y_exp))/2
        p_y_given_x <- p_x_given_y + log(p[i])
        
        #################### taking normal distribution function ##############
        #p_x_given_y<-dnorm(x_j,mean = M_i, sd = V_i)
        #print(c("P(X|Y)", p_x_given_y))
        ##p_x_given_y<-c(log(p_x_given_y), log(p[i]))
        #print(c("logP(X|Y) ", p_x_given_y))
        #p_y_given_x<-logProd(p_x_given_y)
        #print(c("P(Y|X)", p_y_given_x))
        
        lglikelihood<-(c(lglikelihood, p_y_given_x))
     
      
      
    }

    classification<-c(classification, which.max(lglikelihood))
    #print(which.max(lglikelihood))
  }
  #print(is.vector(classification))
  #print(length(classification))
  #print(classification)
  return(classification)
}

getPrecisionAndRecall<-function(X, predictedLabels, goldLabels){
  predictedTable=table(predictedLabels)

  goldTable=table(goldLabels)

  print(predictedTable)
  print(goldTable)
  totalPredictedX=predictedTable[X]
  totalGoldX=goldTable[X]

  TP_X=0
  correctPredictionCount=0;
  for(i in 1:length(predictedLabels)){

    if(predictedLabels[i]==goldLabels[i]){
      correctPredictionCount=correctPredictionCount+1;
    }

    if(predictedLabels[i]==X){
      if(goldLabels[i]==X){
        TP_X=TP_X+1;

      }
    }
  }

  precision=TP_X/totalPredictedX;
  recall=TP_X/totalGoldX;
  correctPredictionCount=correctPredictionCount/length(predictedLabels)
  print('before return')
  return(c(precision,recall,correctPredictionCount) )
}



######################################################
######################################################
#### To compare with part 3 : Logistic Regression ####
######################################################

l = likelihood(xTrain, yTrain)
predictions<-naiveBayesClassify(xTest, l$M, l$V, prior(yTrain))
predictions = predictions - 1 

yTest = (as.vector(unlist(yTest)))
yPred = as.vector(predictions)

tn =  sum (mapply (function(yp, yt) as.integer(yt==yp && yt==0), yPred, yTest))
tp =  sum (mapply (function(yp, yt) as.integer(yt==yp && yt==1), yPred, yTest))
fn =  sum (mapply (function(yp, yt) as.integer(yt!=yp && yt==1), yPred, yTest))
fp =  sum (mapply (function(yp, yt) as.integer(yt!=yp && yt==0), yPred, yTest))

precision = tp / (tp + fp )
recall = tp / ( tp + fn)
accuracy = (tp+tn) / ( tp + tn + fp + fn)

cat(sprintf("TN %d\n", tn))
cat(sprintf("FP %d\n", fp))
cat(sprintf("FN %d\n", fn))
cat(sprintf("TP %d\n", tp))

cat(sprintf("Precision %f\n", 100*precision))
cat(sprintf("Recall %f\n", 100*recall))
cat(sprintf("Accuracy %f\n", 100*accuracy))



######################################################
################## End of Part 3 #####################
######################################################