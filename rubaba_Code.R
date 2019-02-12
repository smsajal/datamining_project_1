##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment

## Read the data
xTrain_old=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_xTrain.csv", header=FALSE)
yTrain_old=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_yTrain.csv", header=FALSE)
xTest_old=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_xTest.csv", header=FALSE)
yTest_old=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_yTest.csv", header=FALSE)

xTrain_new=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_new.xTrain.csv",  header = FALSE)
yTrain_new=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_new.yTrain.csv",  header = FALSE)
xTest_new=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_new.xTest.csv",  header = FALSE)
yTest_new=read.csv("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1/ecoli_new.yTest.csv",  header = FALSE)

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

#### Part 2 ####
prior <- function(yTrain){
  #print(yTrain)
  freq<-as.data.frame(table(yTrain))
  name<-freq[[1]]
  v<-freq[[2]]
  sumF<- sum(v)
  vec<-v/sumF
  
  return(vec)
}

likelihood <- function(xTrain, yTrain){
  combinedMtx<- as.data.frame(cbind(xTrain,yTrain))
  options(max.print = 999999)
  M<-aggregate(combinedMtx, combinedMtx[ncol(xTrain)+1], mean)
  
  M<-M[,2:(ncol(xTrain)+1)]
  print(c("ncol xtrain ",ncol(xTrain)))
  V<-aggregate(combinedMtx, combinedMtx[ncol(xTrain)+1], var)
  #print(c(V) )
  V<-V[,2:(ncol(xTrain)+1)]
  #print(print(c(V) ))
  M<-data.matrix(M)
  V<-data.matrix(V)
  #print(dim(M))
  #print(V)
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

      #print(V_i)
      ################# breaking it up ##############     
      #V_i <- 1/V_i
      #print(V_i)
      #x_j_M_i_diff <- (x_j - M_i) ^ 2 * V_i
      #p_x_given_y_exp <- x_j_M_i_diff
      
      #p_x_given_y_var <- logProd(log(V_i))
      #p_x_given_y <- (p_x_given_y_var - sum(p_x_given_y_exp))/2
      #print(p_x_given_y)
      #p_y_given_x <- p_x_given_y + log(p[i])
      #print(p_y_given_x)
      #################### taking normal distribution function ##############
      p_x_given_y<-dnorm(x_j,mean = M_i, sd = V_i, log)
      #print(c("P(X|Y)", p_x_given_y))
      p_x_given_y<-c((p_x_given_y), log(p[i]))
      #print(c("logP(X|Y) ", p_x_given_y))
      p_y_given_x<-logProd(p_x_given_y)
      #print(c("P(Y|X)", p_y_given_x))
      
      lglikelihood<-(c(lglikelihood, p_y_given_x))
    }
    #print(lglikelihood)
    classification<-c(classification, which.max(lglikelihood))
  }
  #print(is.vector(classification))
  #print(length(classification))
  #print(classification)
  return(classification)
}

#### Part 3 ####
sigmoidProb <- function(y, x, w){
  w0 = 0
  z = w0 + (t(w) %*% x)  # should I include the bias w0 ? w0+z or only z?
  ##cat(sprintf("z %f\n", z))
  e = exp(z)
  ##cat(sprintf("e %f\n", e))
  b = 1.0 + e
  ##cat(sprintf("b %f\n", b))
  p_Y0_given_XW = 1/b
  p_Y1_given_XW = e/b
  ##cat(sprintf("p_Y0_given_XW %f\n", p_Y0_given_XW))
  ##cat(sprintf("p_Y1_given_XW %f\n", p_Y1_given_XW))
  cond_prob_of_y <- if(y == 0) p_Y0_given_XW else p_Y1_given_XW
  #ifelse(a>0,a/sum(a),1)
  cond_prob_of_y
}

logisticRegressionWeights <- function(xTrain, yTrain, w0, nIter){
  w = w0 # initial weight vector
  eta = 0.1 #stepsize
  
  dims = dim(xTrain)
  nRow = dims[1]
  nCol = dims[2]
  x = xTrain
  
  for (j in 1:nIter) {
    
    gradient = (rep( 0 , nCol ))
    
    for (i in 1:nRow) {
      x2 = as.numeric(x[i, ]) # one example at a time
      y2 = yTrain[i,1] - sigmoidProb(1,x2,w) 
      y2 = as.numeric(y2[,1])
      
      gradient = gradient + (x2 * y2) 
      
      
      # if(i==1 && j==1){
      #   print(x2)
      #   print(y2)
      #   print(x2*y2)
      #   print(gradient)
      # }
    }
    
    w = w + eta*gradient
    #print(w)
  }
  
  w
}


logisticRegressionClassify <- function(xTest, w){
  threshold = 0.5
  yPred = 0
  cond_prob_of_y_being_one = sigmoidProb(1, xTest, w) 
  if ((cond_prob_of_y_being_one) > threshold){
    yPred = 1
  }
  yPred
}



#### Part 3 Helper Functions ####
getInitialWeight <- function(xTrain){
  dims = dim(xTrain)
  nRow = dims[1]
  nCol = dims[2]
  
  #### random initialization
  #hundredths <- seq(from=0, to=1, by=.01)
  #w = sample(hundredths, size=nCol, replace=TRUE)
  ## or ## w <- round(runif(100, 0.0, 1.0), digits=2)
  
  #initialize by zero
  w = (rep( 0 , nCol ))
  w
}

logisticRegressionClassifyBatch <- function(xTest, w){
  threshold = 0.5
  dims = dim(xTest)
  nRow = dims[1]
  yPred = (rep( 0 , nRow ))
  
  for (i in 1:nRow) {
    cond_prob_of_y_being_one = sigmoidProb(1, as.numeric(xTest[i, ]), w) #ith row, all column
    if ((cond_prob_of_y_being_one) > threshold){
      yPred[i] = 1
    }
  }
  yPred
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
  
  goldLabels <- as.data.frame(goldLabels)
  goldLabels <- as.vector(goldLabels[[1]])
  
  print(c("predicted ", length(predictedLabels), is.vector(predictedLabels), " gold ", length(goldLabels), is.data.frame(goldLabels), nrow(goldLabels), ncol(goldLabels)))
  for(i in 1:length(predictedLabels)){
    if(predictedLabels[i]==goldLabels[i]){
      correctPredictionCount=correctPredictionCount+1;
    }
  }
  
  accuracy=correctPredictionCount/length(predictedLabels)
  
  if(is.na(predictedLabels[X])){
    return(0,0,accuracy)
  }
  
  
  for(i in 1:length(predictedLabels)){
    if(predictedLabels[i]==X){
      if(goldLabels[i]==X){
        TP_X=TP_X+1;
        
      }
    }
  }
  
  precision=TP_X/totalPredictedX;
  recall=TP_X/totalGoldX;
  
  
  return(c(precision,recall,accuracy) )
}
#print(prior(yTrain))
#likelihood(xTrain,yTrain)

likelhd<-likelihood(xTrain_old, yTrain_old)
predictions<-naiveBayesClassify(xTest_old, likelhd$M, likelhd$V, prior(yTrain_old))

predictions_new<-naiveBayesClassify(xTest_new, likelihood(xTrain_new, yTrain_new)$M, likelihood(xTrain_new, yTrain_new)$V, prior(yTrain_new))
freq<-as.data.frame(table(yTrain_new))
name<-freq[[1]]

print("GGGGGGGGGGG")
print(predictions)
is.vector(predictions)
print("FFFFFFFFF")
#print(yTest_new)


##yTest_new <- as.data.frame(yTest_new)
#yTest_new <- as.vector(yTest_new[[1]])
#yTest_old <- as.data.frame(yTest_old)
#yTest_old <- as.vector(yTest_old[[1]])
#yTest <- as.data.frame(yTest)
#yTest <- as.vector(yTest[[1]])
#is.vector(yTest)
#is.vector(predictions)
precisionRecall<-getPrecisionAndRecall(1, predictions, goldLabels = yTest_old)
print(c(precisionRecall[1], "  ", precisionRecall[2], "  ", precisionRecall[3]))

#precisionRecall<-getPrecisionAndRecall(1, predictions_new, goldLabels = yTest_new)
#print(c(precisionRecall[1], "  ", precisionRecall[2], "  ", precisionRecall[3]))
