library(tm)
library(plyr)
library(rpart)
library(cvTools)
library(kernlab)
library(randomForest)
library(e1071)
library(argparse)
source("spClassR/helper.R")


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


runCV <- function(data.train, trainNPredict, runID='', ...){
  
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
    tmp <- data.frame(test.samples, test.dat$label, tmp)
    colnames(tmp) <- c('id', 'label', sprintf('prediction_%s', runID))
    
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

runNBC <- function(train.dat, test.dat){
  fit_nbc <- naiveBayes(label~.,train.dat)
  pred_nbc <- predict(fit_nbc, newdata=test.dat, type='class') 
  return(pred_nbc)
}

comparePredictions <- function(results){
  comparision <- NULL
  for (ri in results){
#     print(ri)
    if (is.null(comparision)){
      comparision <- ri
    }else{
      comparision <- cbind(comparision, ri)
    }
  }
#   label <- results[1]['label']
#   comparision <- 
#   cbind(label=label,
#         comparision[, colnames(comparision != 'label')])
  return(comparision)
}


ensemble <- function(predictions){
  allPred <- comparePredictions(predictions)
  index <- grepl('prediction', colnames(allPred), perl=TRUE)
  preds <- allPred[, index]
  s <- NULL
  for (pred_i in preds){
    if (is.null(s)){
      s <- as.integer(pred_i)
    }else{
      s <- s + as.integer(pred_i)
    }
  }
  en <- round(s/length(predictions))
  en <- factor(en, labels=c('good', 'spam'), ordered=TRUE)
  result <- cbind(allPred[,c('id', 'label')],
                  preds,
                  prediction_en=en)
  return(result)
}



tokensOrig <- stemDocument(c("price", "customer", "product", "look", "buy"),
                           language='english')
tokensNew <- stemDocument(c("price", "customer", "product", "look", "buy", "manage"),
                          language='english')

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-i", "--inputFilePath",
                    default="../results/labeledFeatures.fake.csv",
                    type="character",
                    help="CSV of features for unlabeled emails")
                                        
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
print(args)

# their respective r object name is the same as their file name
labeledFeatures <- read.csv("../results/labeledFeatures.csv")
unlabeledFeatures <- read.csv(args$inputFilePath)



idf_featnm <- c("label", "price_tfidf","custom_tfidf", 
                 "product_tfidf", "look_tfidf","buy_tfidf")

featIndex <- c('label','product',
               'senLength','questionCount','exclaimCount','iCount','iCount', 
               'myCount', 'countPercent')

# # labeledFeatures <- normalizeNumerics(labeledFeatures)
# r1 <- runCV(labeledFeatures, runSVM, runID='SVM')
# evalConf(r1)

# r2 <- runCV(labeledFeatures, runRF, runID='RF') #, mtry=6,ntree=30,sampsize=40
# evalConf(r2)

# r3 <- runCV(labeledFeatures, runNBC, runID='NBC') #, mtry=6,ntree=30,sampsize=40
# evalConf(r3)

# comparision <- comparePredictions(list(r1,r2, r3))

# en <- ensemble(list(r1,r2, r3))
# sum(as.integer(en$label)==as.integer(en$prediction_en))


#   c("price_tfidf","custom_tfidf", 
#                 "product_tfidf", "look_tfidf","buy_tfidf", "mn"

# predict unlabled email
# 
# unlabeledFeatures <- read.csv('unlabeledFeatures.csv')
# unlabeledFeatures <- unlabeledFeatures[,]
# 
# predictionRF <- runRF(labeledFeatures, unlabeledFeatures)
# write.csv(predictionRF, 'predictionRF.csv')
# predictionSVM <- runSVM(labeledFeatures, unlabeledFeatures)
# write.csv(predictionSVM, 'predictionSVM.csv')
# predictionNBC <- runNBC(labeledFeatures, unlabeledFeatures)
# write.csv(predictionSVM, 'predictionNBC.csv')


# outNickname <- getNickname(args$inputFilePath)
# outFilePath <- paste(c("../results/unlabeledFeatures",
#                       outNickname, "csv"), collapse=".")
# fit_rf <-randomForest(label~.,labeledFeatures)
# 
# pred_rf <- predict(fit_rf, newdata=unlabeledFeatures, type='response')
 
