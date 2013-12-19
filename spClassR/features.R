library(tm)
library(plyr)

# their respective r object name is the same as their file name
load("labeledEmails.Rda")
load("unlabeledEmails.Rda")

strsplit_space_tokenizer <- function(x){
  return(unlist(strsplit(x, "\\W*\\s+\\W*", perl=TRUE)))
}

prepEmailTokens <- function(emailText, stops=stopwords("english")){
  # lower letters
  emailLower <- tolower(emailText)
  
  # split the document into single words
  strsplit_token_email <- strsplit_space_tokenizer(emailLower)
  
  # stemming
  stem_email <- stemDocument(strsplit_token_email, language="english")
  
  # remove words from stop word set (very frequently occuring words)
  stem_removed_email <- stem_email[!stem_email %in% stops]  
  
  return(stem_removed_email) # a vector of all tokens
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
  emailTokens <- prepEmailTokens(emailText)  
  # paste tokens into a document
  cleanText <- paste(emailTokens, collapse=" ")
  
  tfAll <- getTermFreqs(cleanText)
  tfTokens <- extractFreq(tfAll, tokens)
  
  tfTokensAug <- (tfTokens * 0.5 / max(tfAll)) + 0.5
#   print(tfTokensAug)
  result <- c(
    tokenCount=length(strsplit_space_tokenizer(emailText)),
    cleanTokenCount=length(emailTokens),
    tfTokens#tfTokensAug
  )
  result <- rbind(result)
  return(result)
}

computeDtfIdf <- function(tf){
  idf <- log(length(tf) / sum(tf > 0)) #where tf = 0.5 is the base line of augmented tf
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

sentenceLength <- function(text){
  sentences <- unlist(strsplit(text, "(\\?|\\.|\\!)", perl=TRUE))
  sLengths <- laply(sentences, nchar)
  return(sLengths)
}

countUpperCase <- function(text){
  orig <- unlist(strsplit(text,''))
  upper <- unlist(strsplit(toupper(text),''))
  upcount <- sum(orig == upper)
  return(upcount)
}

countI <- function(rawEmail){
  emailTokens <- strsplit_space_tokenizer(tolower(rawEmail))
  iregex <- "(\\Ai|(i\\'\\w*))\\Z" # i, i'll, i've etc, but not idea, or hi
  cntI <- length(grep(iregex, emailTokens, perl=TRUE, value=TRUE))
  myregex <- "\\Amy\\Z"
  cntMy <- length(grep(myregex, emailTokens, perl=TRUE, value=TRUE))
  return(cntI)
}

countPercent <- function(email){
    digitMatches <- gregexpr("\\%", email, perl=TRUE)# look for 2 digits or more continously
    cnt <- length(digitMatches[[1]])
    return(cnt)
}

countMy <- function(rawEmail){
  emailTokens <- strsplit_space_tokenizer(tolower(rawEmail))
  myregex <- "\\Amy\\Z"
  cntMy <- length(grep(myregex, emailTokens, perl=TRUE, value=TRUE))
  return(cntMy)
}

countDigits <- function(email){
  digitMatches <- gregexpr("\\d{2,}", email, perl=TRUE) # look for 2 digits or more continously
  cnt <- length(digitMatches[[1]])
  return(cnt)
}

nonTokenFeatures <- function(emailRaw){
  senlens <- sentenceLength(emailRaw)
  senLength <- mean(senlens)
  
  # cap letter not occuring at beginning of sentence
  capCount <- countUpperCase(emailRaw) - length(senlens) 
  questionCount <- length(unlist(strsplit(emailRaw,'\\?'))) - 1
  exclaimCount <- length(unlist(strsplit(emailRaw,'\\!'))) - 1
  iCount <- countI(emailRaw)
  digitCount <- countDigits(emailRaw)
  feat <- data.frame(
    senLength=senLength,
    capCount=capCount,
    questionCount=questionCount,
    exclaimCount=exclaimCount,
    iCount=iCount,
    myCount=countMy(emailRaw),
    digitCount=digitCount,
     countPercent=countPercent(emailRaw)
  )
  return(feat)
}


extraFeatures <- function(eDat, tokens){
  
  features <- adply(eDat, .margins=1, .fun=function(x, tokens){
    featSet1 <- dtfFeatures(x, tokens)
    featSet2 <- nonTokenFeatures(x[1,'emailText'])
    return(cbind(featSet1, featSet2))
  }, tokens=tokensNew)
    
  features <- getDtfIdfAll(features, tokens) # add DtfIdf features
  #features <- features[,!(colnames(features) %in% tokens)] # remove tf columns
  features <- features[,colnames(features) != 'emailText'] # remove row number column
  return(features)

}

tokensOrig <- stemDocument(c("price", "customer", "product", "look", "buy"),
                           language='english')
tokensNew <- stemDocument(c("price", "customer", "product", "look", "buy", "manage"),
                          language='english')

labeledFeatures <- extraFeatures(labeledEmails, tokensNew)
write.csv(labeledFeatures,
          file='labeledFeatures.csv',
          row.names=FALSE)

unlabeledFeatures <- extraFeatures(unlabeledEmails, tokensNew)
write.csv(unlabeledFeatures,
          file='unlabeledFeatures.csv',
          row.names=FALSE)
