library(tm)
library(plyr)
library(rpart)
library(cvTools)
library(kernlab)
library(randomForest)



normalizeNumerics <- function(dat){
  x <- dat[,]
  numCols <- laply(x, is.numeric)
  normed <- scale(x[,numCols])
  x[, numCols] <- normed
  return(x)
}

# Calculate sensitivity and specificity from confusion matrix
evalConf <- function(x){
  conf.mtrx <- table(x$label, x$prediction)
  print(conf.mtrx)
  tn <- conf.mtrx[1,1]
  fn <- conf.mtrx[2,1]
  fp <- conf.mtrx[1,2]
  tp <- conf.mtrx[2,2]
  sens <- tp / (tp+fn)
  spec <- tn / (tn+fp)
  acc <- (tp+tn) / sum(conf.mtrx)
  return(list(spec=spec, sens=sens, acc=acc))
}


runCV <- function(data.train, trainNPredict, ...){
  
  # Create specified number of folds (10)
  set.seed(22)
  folds <- cvFolds(nrow(data.train), K = 10)
  
  # Create empty prediction list
  predictions <- NULL
  
  # run decision tree fitting and evaluation 10 times (number of folds), using data from each fold as test set
  for(f in 1:10){
    
    # select training and test samples according to fold splits
    train.samples <- folds$subsets[folds$which!=f,]
    test.samples <- folds$subsets[folds$which==f,]
    
    # select "training" and "test" data and "test" labels for current fold
    train.dat <- data.train[train.samples,]
    test.dat <- data.train[test.samples,]
    test.labels <- data.train$label[test.samples]
    
    # run function that train a model and test on on testing set
    tmp <- trainNPredict(train.dat, test.dat, ...)
    tmp <- data.frame(label=test.dat$label, prediction=tmp)
    
    if(is.null(predictions)){
      predictions <- tmp
    }else{
      predictions <-  rbind(predictions, tmp)
    }
    
  }
  return(predictions)
}

runSVM <- function(train.dat, test.dat){
  fit_svm <- ksvm(label~., train.dat, kernel='vanilladot')
  pred_svm <- predict(fit_svm, newdata = test.dat, type='response')  
  #tmp_svm <- table(test.dat$label, pred_svm)
  return(pred_svm)
}

runRF <- function(train.dat, test.dat, ...){
  fit_rf <-randomForest(label~.,train.dat, ...)
  pred_rf <- predict(fit_rf, newdata=test.dat, type='response')
#   tmp_rf <- table(test.dat$label, pred_rf)
  return(pred_rf)
}

# 
# idf_featnm <- c("label", "price_tfidf","custom_tfidf", 
#                  "product_tfidf", "look_tfidf","buy_tfidf")

labeledFeatures <- read.csv('labeledFeatures.csv')
r1 <- runCV(labeledFeatures, runSVM)
evalConf(r1)

r2 <- runCV(labeledFeatures, runRF) #, mtry=6,ntree=30,sampsize=40
evalConf(r2)


# predict unlabled email

unlabeledFeatures <- read.csv('unlabeledFeatures.csv')
predictionRF <- runRF(labeledFeatures, unlabeledFeatures)
write.csv(predictionRF, 'predictionRF.csv')
predictionSVM <- runSVM(labeledFeatures, unlabeledFeatures)
write.csv(predictionSVM, 'predictionSVM.csv')

 
