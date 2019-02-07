##################################
###### STAT 557 (Project 1) ######
##################################

rm(list=ls()) ## To clear your environment
## Read the data
#setwd("/Users/Sherlock/Box\ Sync/PSU\ Spr19/STAT\ 557/project1/datamining_project_1")

xTrain=read.csv("ecoli_new.xTrain.csv",  header = FALSE)
yTrain=read.csv("ecoli_new.yTrain.csv",  header = FALSE)
xTest=read.csv("ecoli_new.xTest.csv",  header = FALSE)
yTest=read.csv("ecoli_new.yTest.csv",  header = FALSE)


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

w = getInitialWeight(xTrain)
learned_w = logisticRegressionWeights(xTrain, yTrain, w, 1000)
yPred = logisticRegressionClassifyBatch(xTest, learned_w)
print(sum(yPred != yTest))
cat(sprintf("Accuracy %f\n", 100*sum(yPred == yTest)/(dim(yTest))[1]))


### Test for sigmoidProb
#xx =  (as.numeric(xTrain[1, ]))
#print(sigmoidProb(0,xx, w))
