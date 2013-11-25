library(tm)
library(plyr)

fileName <- 'GoodnSpam.txt'
emailRaw <- readChar(fileName, file.info(fileName)$size)
x <- unlist(strsplit(emailRaw, "\\s*\\d+ ((Good/non-spam)|(SPAM)) emails below\\s*", perl=TRUE))

splitEmails <- function(eStr){
  emailList <- unlist(strsplit(eStr, "\\s*Email\\s+#\\d+\\s*", perl=TRUE))
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

getTermFreqs <- function(emailText, tokens){
  myCorpus <- Corpus(VectorSource(emailText))
  tf <- termFreq(myCorpus[[1]])
  return(extractFreq(tf, tokens))
}

dtfFeatures <- function(x, tokens){
  cleanText <- prepEmailTokens(x$emailText)
  tfs <- getTermFreqs(cleanText, tokens)
  result <- c(
    tokenCount=length(strsplit_space_tokenizer(x$emailText)),
    tfs
  )
  result <- rbind(result)
  return(result)
}

computeDtfIdf <- function(tf){
  idf <- log(length(tf) / sum(tf > 0))
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