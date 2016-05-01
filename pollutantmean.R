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

pollutantmean <- function(directory, pollutant, id = 1:332){
  ## directory - is a character vector of lenght 1 indicating the location of the csv file
  ## id - is an integer vector indicating the monitor ID numbers to be used
  
  ## return the mean of the pollutant of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## note: do not round the result!
  
  ## pollutant - is a character vector of lenght 1 indicating the name of the pollutant for 
  ##  which we will calculate the mean; either "sulfate" or "nitrate"
  
  #log(getwd()) #for debugging
  
  setDir(directory)
  
  myData <- loadData(id)
  
  result <- NULL
  
  if(pollutant == "sulfate") {
    #result <- columnMean(myData$sulfate, TRUE)  
    result <- mean(myData$sulfate,na.rm = TRUE)
  }
  else if(pollutant == "nitrate") {
    #result <- columnMean(myData$nitrate, TRUE) 
    result <- mean(myData$nitrate,na.rm = TRUE)
  }
  
  result
}