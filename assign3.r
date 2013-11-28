library(tm)
library(plyr)
library(rpart)
library(cvTools)
library(kernlab)
library(randomForest)

fileName <- 'GoodnSpam.txt'
emailRaw <- readChar(fileName, file.info(fileName)$size)
x <- unlist(strsplit(emailRaw, "\\s*\\d+ ((Good/non-spam)|(SPAM)) emails below\\s*", perl=TRUE))

splitEmails <- function(eStr){
  emailList <- unlist(strsplit(eStr, "Email\\s+#\\d+\\s*", perl=TRUE))
  # remove the front of the list that doesn't contain any email
  emailList <- emailList[2:length(emailList)] 
  return(emailList)
}

goodEmails <- splitEmails(x[2])
spamEmails <- splitEmails(x[3])

labelEmails <- function(gdE, spE){
  gdELabeled <- cbind(emailText=gdE, label=rep('good', length(gdE)))
  spELabeled <- cbind(emailText=spE, label=rep('spam', length(spE)))
  allLabeled <- rbind(gdELabeled, spELabeled)
  return(data.frame(allLabeled))
}
eDat <- labelEmails(goodEmails, spamEmails)
eDat$emailText <- as.character(eDat$emailText)

strsplit_space_tokenizer <- function(x){
  return(unlist(strsplit(x, "\\W*\\s+\\W*", perl=TRUE)))
}

prepEmailTokens <- function(emailText, stopMore=c()){
  # lower letters
  emailLower <- tolower(emailText)
  
  # split the document into single words
  strsplit_token_email <- strsplit_space_tokenizer(emailLower)
  
  # stemming
  stem_email <- stemDocument(strsplit_token_email, language="english")
  
  # remove words from stop word set (very frequently occuring words)
  originalstopword <- stopwords("english")
  mystopword <- c(originalstopword, stopMore)
  stem_removed_email <- stem_email[!stem_email %in% mystopword]  
  
  # paste tokens into a document
  document <- paste(c(stem_removed_email), collapse=" ")
  return(document)
}

extractFreq <- function(tf, tokens){
  tfSubset <- tf[tokens]
  tfSubset[is.na(tfSubset)] <- 0 # set freq =  0 if token not occur in document
  names(tfSubset) <- tokens
  return(tfSubset)
}

getTermFreqs <- function(emailText){
  myCorpus <- Corpus(VectorSource(emailText))
  tf <- termFreq(myCorpus[[1]])
  return(tf)
}

dtfFeatures <- function(x, tokens){
  features <- dtfFeatures_help(x$emailText, tokens)
  return(features)
}

dtfFeatures_help <- function(emailText, tokens){
  cleanText <- prepEmailTokens(emailText)
  tfAll <- getTermFreqs(cleanText)
  tfTokens <- extractFreq(tfAll, tokens)
  
  tfTokensAug <- (tfTokens * 0.5 / max(tfAll)) + 0.5
  print(tfTokensAug)
  result <- c(
    tokenCount=length(strsplit_space_tokenizer(emailText)),
    tfTokensAug
  )
  result <- rbind(result)
  return(result)
}

computeDtfIdf <- function(tf){
  idf <- log(length(tf) / sum(tf > 0.5)) #where tf = 0.5 is the base line of augmented tf
  tfIdf <- tf * idf
  return(tfIdf)
}

getDtfIdfAll <- function(dtf, tokens){
  dtfIdf <- dtf[,]
  for(t in tokens){
    newFeature <- data.frame(nf=computeDtfIdf(dtf[,t]))
    colnames(newFeature) <- c(sprintf('%s_tfidf', t))             
    dtfIdf <- cbind(dtfIdf, newFeature)
  }
  return(dtfIdf)
}

tokens <- stemDocument(c("price", "customer", "product", "look", "buy"),
                       language='english')
dtf <- adply(eDat, .margins=1, .fun=dtfFeatures, tokens=tokens)
dtf <- dtf[,colnames(dtf) != 'emailText'] # remove row number column
write.csv(dtf, "Dtf.csv")

dtfIdf <- getDtfIdfAll(dtf, tokens)
write.csv(dtfIdf, "Dtfidf.csv")

# SVM

# Calculate sensitivity and specificity from confusion matrix
evalConf <- function(conf.mtrx){
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
  folds <- cvFolds(nrow(dtf), K = 10)
  
  # Create empty confusion matrix to store results from each fold that will be run
  conf.matrix <- NULL
  
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
    
    if(is.null(conf.matrix)){
      conf.matrix <- tmp
    }else{
      conf.matrix <-  conf.matrix + tmp
    }
    
  }
  return(conf.matrix)
}

normalizeNumerics <- function(dat){
  x <- dat[,]
  numCols <- laply(x, is.numeric)
  normed <- scale(x[,numCols])
  x[, numCols] <- normed
  return(x)
}

runSVM <- function(train.dat, test.dat){
  fit_svm <- ksvm(label~., train.dat, kernel='vanilladot')
  pred_svm <- predict(fit_svm, newdata = test.dat, type='response')  
  tmp_svm <- table(test.dat$label, pred_svm)
  return(tmp_svm)
}

runRF <- function(train.dat, test.dat, ...){
  fit_rf <-randomForest(label~.,train.dat, ...)
  pred_rf <- predict(fit_rf, newdata=test.dat, type='response')
  tmp_rf <- table(test.dat$label, pred_rf)
  return(tmp_rf)
}

idf_featnm <- c("label", "price_tfidf","custom_tfidf", 
                 "product_tfidf", "look_tfidf","buy_tfidf")

(confDtf <- runCV(dtf, runSVM))
evalConf(confDtf)

(confDtfIdf <- runCV(dtfIdf, runSVM))
evalConf(confDtfIdf)

 
