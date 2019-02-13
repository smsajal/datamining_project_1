##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment

#setwd("/Users/Sherlock/Box\ Sync/PSU\ Spr19/STAT\ 557/project1/datamining_project_1")
#setwd("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1")
## Read the data



######## Helper Functions ########
logProdNB <- function(x){
  logProduct=0
  for(i in 1:length(x)){
    if (!is.infinite(x[i]) && !is.nan(x[i])){
      logProduct=logProduct+x[i]
    }
  }

  return(logProduct)
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

logisticRegressionClassifyBatchTest <- function(xTest, w){
  threshold = 0.5
  dims = dim(xTest)
  nRow = dims[1]
  yPred = (rep( 0 , nRow ))

  for (i in 1:nRow) {
    yPred[i] = logisticRegressionClassify(as.numeric(xTest[i,]), w)
  }
  yPred
}

getPrecisionAndRecall<-function(X, predictedLabels, goldLabels){

  predictedTable=table(predictedLabels)

  goldTable=table(goldLabels)

  ##print(predictedTable)
  ##print(goldTable)

  totalPredictedX=predictedTable[X]

  totalGoldX=goldTable[X]


  TP_X=0


  correctPredictionCount=0;

  goldLabels <- as.data.frame(goldLabels)
  goldLabels <- as.vector(goldLabels[[1]])

  for(i in 1:length(predictedLabels)){
    #print(c(predictedLabels[i], goldLabels[i]))
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



#### Part 1 ####
logProd <- function(x){
  logProduct=0
  for(i in 1:length(x)){
    logProduct=logProduct+x[i]
  }

  return(logProduct)
}

logSum <- function(x){
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

  dims = dim(combinedMtx)
  nRow = dims[1]
  nCol = dims[2]

  M<-aggregate(combinedMtx[,1:(nCol-1)], combinedMtx[nCol], mean)
  M<-M[,2:nCol]

  V<-aggregate(combinedMtx[,1:(nCol-1)], combinedMtx[nCol], var)
  V<-V[,2:nCol]

  M<-data.matrix(M)
  V<-data.matrix(V)
  retList<- list("M" = M, "V" = V)
  return(retList)
}

naiveBayesClassify <- function(xTest, M, V, p){
  xTest<-as.data.frame(xTest)
  xTest<-as.matrix(xTest)

  classification<-vector()
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

      p_x_given_y_var <- logProdNB(log(V_i))

      p_x_given_y <- (p_x_given_y_var - sum(p_x_given_y_exp))/2
      p_y_given_x <- p_x_given_y + log(p[i])
      #################### taking normal distribution function ##############
      #p_x_given_y<-dnorm(x_j,mean = M_i, sd = V_i, log)
      #print(c("P(X|Y)", p_x_given_y))
      #p_x_given_y<-c((p_x_given_y), log(p[i]))
      #print(c("logP(X|Y) ", p_x_given_y))
      #p_y_given_x<-logProd(p_x_given_y)
      #print(c("P(Y|X)", p_y_given_x))

      lglikelihood<-(c(lglikelihood, p_y_given_x))
    }
    classification<-c(classification, which.max(lglikelihood))
  }
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



##### TESTER FUNCTIONS ####

tester3 <- function(){
  #setwd("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1")
  #setwd("/Users/Sherlock/Box\ Sync/PSU\ Spr19/STAT\ 557/project1/datamining_project_1")
  print("################")
  print("TESTING PART3")
  print("################")

  xTrain=read.csv("ecoli_new.xTrain.csv",  header = FALSE)
  yTrain=read.csv("ecoli_new.yTrain.csv",  header = FALSE)
  xTest=read.csv("ecoli_new.xTest.csv",  header = FALSE)
  yTest=read.csv("ecoli_new.yTest.csv",  header = FALSE)

  w = getInitialWeight(xTrain)
  learned_w = logisticRegressionWeights(xTrain, yTrain, w, 100)
  yPred = logisticRegressionClassifyBatchTest(xTest, learned_w)
  yTest = as.numeric(yTest[,1])

  tn =  sum (mapply (function(yp, yt) as.integer(yt==yp && yt==0), yPred, yTest))
  tp =  sum (mapply (function(yp, yt) as.integer(yt==yp && yt==1), yPred, yTest))
  fn =  sum (mapply (function(yp, yt) as.integer(yt!=yp && yt==1), yPred, yTest))
  fp =  sum (mapply (function(yp, yt) as.integer(yt!=yp && yt==0), yPred, yTest))

  precision = tp / (tp + fp )
  recall = tp / ( tp + fn)
  accuracy = (tp+tn) / ( tp + tn + fp + fn)
  # print(tn)
  # print(fp)
  # print(fn)
  # print(tp)
  cat(sprintf("Precision %f\n", 100*precision))
  cat(sprintf("Recall %f\n", 100*recall))
  cat(sprintf("Accuracy %f\n", 100*accuracy))

  ### Test for sigmoidProb
  #xx =  (as.numeric(xTrain[1, ]))
  #print(sigmoidProb(0,xx, w))


  #################### Naive Bayes: new dataset with 2 classes ####################
  l<-likelihood(xTrain, yTrain)
  predictions_new<-naiveBayesClassify(xTest, l$M, l$V, prior(yTrain))
  #predictions_new<-predictions_new-1
  precisionRecall<-getPrecisionAndRecall(2, predictions_new, goldLabels = yTest+1)

  ### precision recall: TODO double check
  print(c("New Dataset: ","Precision = ",precisionRecall[1], "  , Recall = ",  precisionRecall[2], "  , Accuracy = ", precisionRecall[3]))

}

tester2 <- function(){
  print("################")
  print("TESTING PART2")
  print("################")

  #setwd("/Users/rxh655/Documents/Spring2019/STAT557/project1/project1Code/datamining_project_1")
  #setwd("/Users/Sherlock/Box\ Sync/PSU\ Spr19/STAT\ 557/project1/datamining_project_1")
  xTrain_old=read.csv("ecoli_xTrain.csv",  header = FALSE)
  yTrain_old=read.csv("ecoli_yTrain.csv",  header = FALSE)
  xTest_old=read.csv("ecoli_xTest.csv",  header = FALSE)
  yTest_old=read.csv("ecoli_yTest.csv",  header = FALSE)
  #################### old dataset with 5 classes ####################
  l_old<-likelihood(xTrain_old, yTrain_old)
  predictions<-naiveBayesClassify(xTest_old, l_old$M, l_old$V, prior(yTrain_old))
  precisionRecall<-getPrecisionAndRecall(1, predictions, goldLabels = yTest_old)
  print(c("Old Dataset: ","Precision = ", precisionRecall[1], "  , Recall = ", precisionRecall[2], "  , Accuracy = ", precisionRecall[3]))

}

tester1 <- function(){
  print("################")
  print("TESTING PART1")
  print("################")
  testingX=c(0,1.609437,1.386294,2.1972245,2.3978952)
  productOfX=logProd(testingX)

  print("Logproduct: ")
  print(productOfX)

  sumOfX=logSum(testingX)

  print("Logsum: ")
  print(sumOfX)
}


# tester1()
# tester2()
# tester3()
