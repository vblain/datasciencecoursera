log <- function(msg){
  Sys.sleep(0.1)
  print(msg)
  flush.console()
}

setDir <- function(directory) {
  if(!any(grepl(directory, getwd()))) { 
    fileDir <- paste(getwd(), directory, sep = '/')
    log(fileDir)
    setwd(fileDir) 
  }
}

loadData <- function(id = 1:332) {
  list.filenames <- list.files(pattern="*.csv")
  ldf <- lapply(list.filenames[id], read.csv, header = TRUE)
  do.call(rbind.data.frame, ldf)
}

corr <- function(directory, threshold = 0) {
  setDir(directory)
  myData <- loadData()
  result <- myData[complete.cases(myData),]
  
  cNum <- numeric(0)
  
  nobs <- complete("specdata")
  
  nobs <- nobs[nobs$nobs > threshold, ]
  
  for(cid in nobs$id) {
    cNum <- c(cNum, cor(result$sulfate[result$ID==cid], result$nitrate[result$ID==cid], use = "pairwise.complete.obs"))
  }
  
  cNum
}
