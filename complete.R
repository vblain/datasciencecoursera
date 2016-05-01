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

complete <- function(directory, id = 1:332) {
  setDir(directory)
  myData <- loadData(id)
  result <- myData[complete.cases(myData),]
  freq <- data.frame(table(result$ID))
  names(freq)[1] = 'id'
  names(freq)[2] = 'nobs'
  freq
}