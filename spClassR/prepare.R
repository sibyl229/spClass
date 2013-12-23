library(plyr)
source("helper.R")

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



labeledEmails <- loadTrainSample('../data/GoodnSpam.txt')
saveRDS(labeledEmails, file="../data/labeledEmails.Rda")


## args <- miniParse(commandArgs(trailingOnly=TRUE),
##                   list(inputFilePath="../data/testemails.fake.txt"))

suppressPackageStartupMessages(library("argparse"))

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-i", "--inputFilePath",
                    default="../data/testemails.fake.txt",
                    type="character",
                    help="Text file of unlabeled emails")
                                        
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
print(args)

# generate output file name for the test email
unlabeledEmails <- loadUnlabeledSample(args$inputFilePath)
outputFilePath <- paste(c("../data/testemails",
                          getNickname(args$inputFilePath),
                          "Rda"), collapse=".")

print(paste(c("Creating ", outputFilePath), collapse=""))

saveRDS(unlabeledEmails, file=outputFilePath)

