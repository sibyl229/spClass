
# Parse keyword arguments in the form of "--arg1 v1 --arg2 v21 v22"
# into list(arg1="v" arg2=c("21","v22")),
# overriding the default keyword arguments (kwArgsDef)

miniParse <- function(args, kwArgsDef=list()){
    print(args)
    argsL <- unlist(strsplit(args, "--"))
    
    # removing leading and trailing whitespaces in non-empty arguments
    argsL <- regmatches(argsL,
                        regexpr("\\S.*\\S", argsL,
                                perl=TRUE))
    
    for (a in argsL){
        argX <- unlist(strsplit(a, "\\s", perl=TRUE))
        if (length(argX) > 1){
            key <- argX[1]
            val <- argX[2:length(argX)]
            kwArgsDef[[key]] <- val
        }
    }
    return(kwArgsDef)
}

# The nickname of "data/testemails.xyz.txt" is "xyz"
getNickname <- function(filePath){
    fileName <- tail(unlist(strsplit(filePath, "\\/", perl=TRUE)), 1)
    nickname <- unlist(strsplit(fileName, "\\.", perl=TRUE))[2]
    return(nickname)
}
            


override <- function(argVal, defaultVal){
  val <- NULL
  if(is.null(argVal)){
    val <- defaultVal
  }else{
    val <- argVal
  }
  return(val)
}
