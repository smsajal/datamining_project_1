##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment

## Read the data
xTrain=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_xTrain.csv")
yTrain=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_yTrain.csv")
xTest=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_xTest.csv")
yTest=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_yTest.csv")


#### Part 1 ####
logProd <- function(x){
  logProduct=0
  for(i in 1:length(x)){
    logProduct=logProduct+x[i]
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
  print(dim(combinedMtx))
  options(max.print = 999999)
  M<-aggregate(combinedMtx, combinedMtx[6], mean)
  print(c("combinedMtx", dim(combinedMtx)))
  print(c("M", dim(M)))
  print(c("M", M))
  M<-M[,2:6]
  print(c("Mafter", dim(M)))
  
  print(c("Mafter", M))
  V<-aggregate(combinedMtx, combinedMtx[6], var)
  V<-V[,2:6]
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
      #print(c("x_j ", x_j))
      #print(c("M-i", M_i))
      #print(c("V_i", V_i))
      #print(x_j_M_i_diff)
      p_x_given_y_exp <- x_j_M_i_diff
      
      p_x_given_y_var <- logProd(log(V_i))
      p_x_given_y <- (p_x_given_y_var - sum(p_x_given_y_exp))/2
      p_y_given_x <- p_x_given_y + log(p[i])
      
      #################### taking normal distribution function ##############
      #p_x_given_y<-dnorm(x_j,mean = M_i, sd = V_i)
      #print(c("P(X|Y)", p_x_given_y))
      #p_x_given_y<-c(log(p_x_given_y), log(p[i]))
      #print(c("logP(X|Y) ", p_x_given_y))
      #p_y_given_x<-logProd(p_x_given_y)
      #print(c("P(Y|X)", p_y_given_x))
      
      lglikelihood<-(c(lglikelihood, p_y_given_x))
    }
    print(lglikelihood)
    classification<-c(classification, which.max(lglikelihood))
  }
  #print(is.vector(classification))
  #print(length(classification))
  print(classification)
  return(classification)
}

#### Part 3 ####
sigmoidProb <- function(y, x, w){
  
}

logisticRegressionWeights <- function(xTrain, yTrain, w0, nIter){
  
}

logisticRegressionClassify <- function(xTest, w){
  
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

#print(prior(yTrain))
#likelihood(xTrain,yTrain)

predictions<-naiveBayesClassify(xTest, likelihood(xTrain, yTrain)$M, likelihood(xTrain, yTrain)$V, prior(yTrain))


yTest <- as.data.frame(yTest)
yTest <- as.vector(yTest[[1]])
is.vector(yTest)
is.vector(predictions)
precisionRecall<-getPrecisionAndRecall(2, predictions, goldLabels = yTest)
print(c(precisionRecall[1], "  ", precisionRecall[2], "  ", precisionRecall[3]))