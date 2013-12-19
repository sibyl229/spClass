library(plyr)

splitEmails <- function(eStr){
  emailList <- unlist(strsplit(eStr, "Email\\s+#\\d+\\s*", perl=TRUE))
  # remove the front of the list that doesn't contain any email
  emailList <- emailList[2:length(emailList)] 
  return(emailList)
}

labelEmails <- function(gdE, spE){
  gdELabeled <- cbind(emailText=gdE, label=rep('good', length(gdE)))
  spELabeled <- cbind(emailText=spE, label=rep('spam', length(spE)))
  allLabeled <- rbind(gdELabeled, spELabeled)
  return(data.frame(allLabeled))
}

loadTrainSample <- function(filename){
  emailRaw <- readChar(filename, file.info(filename)$size)
  x <- unlist(strsplit(emailRaw, "\\s*\\d+ ((Good/non-spam)|(SPAM)) emails below\\s*", perl=TRUE))
  
  
  goodEmails <- splitEmails(x[2])
  spamEmails <- splitEmails(x[3])
  
  eDat <- labelEmails(goodEmails, spamEmails)
  eDat$emailText <- as.character(eDat$emailText)
  return(eDat)
}

loadUnlabeledSample <- function(filename){
  emailRaw <- readChar(filename, file.info(filename)$size)
  emails <- splitEmails(emailRaw)
  
  eDat <- data.frame(emailText=emails)
  eDat$emailText <- as.character(eDat$emailText)
  eDat$label <- as.character('', nrow(eDat))
  return(eDat)
}

args <- commandArgs(TRUE)

override <- function(argVal, defaultVal){
  val <- NULL
  if(is.null(argVal)){
    val <- defaultVal
  }else{
    val <- argVal
  }
  return(val)
}

labeledEmails <- loadTrainSample('../GoodnSpam.txt')
save(labeledEmails,file="../data/labeledEmails.Rda")
unlabeledEmails <- loadUnlabeledSample('../fakeTestEmails.txt')
save(unlabeledEmails,file="../data/unlabeledEmails.Rda")

