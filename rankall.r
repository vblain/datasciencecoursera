# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
# ing (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.
# 
# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.

rankall1 <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  # re-use from previous best.r function
  
  # For Development
  # num <- 4
  # outcome <- "heart attack"
  
  ## Check that state and outcome are valid
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  # Get column index from outcome
  index <- switch (outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # we have a valid outcome - now lets load the data for state and
  myData <- read.csv("outcome-of-care-measures.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", colClasses = "character")
  
  # convert the outcome column index to a numeric
  # suppress warning to prevent display of "not available" 
  # these will be converted to NA
  myData[,index] <- suppressWarnings(as.numeric(myData[,index]))
  # supress NA
  myData <- na.omit(myData)
  
  # I like SQL statement so load sqldf
  if(!require(sqldf)){
    install.packages("sqldf")
    install.packages("tcltk")
    library(sqldf)  
    library(tcltk)  
  }
  
  # retrieve only the data we need
  cName <- colnames(myData[index])
  sql <- sprintf("SELECT myData.[Hospital.Name], myData.State, myData.[%s] FROM myData order by myData.[%s], myData.[Hospital.Name]", cName, cName)
  Hospitals <- sqldf(sql)
  #remove any possible NA
  Hospitals <- na.omit(Hospitals)
  
  HospDataFun <- function(state, num) {
    
    # Get the subset from dataframe for state requested
    subData <- subset(Hospitals, State==state)
    
    # check values given / R doesn't handle switch default well
    # set default as if statement
    numSubs <- switch (num, "best" = 1, "worst" = nrow(subData))
    if(is.null(numSubs)){ numSubs <- as.numeric(num) }
    
    # order by columns from hospital - since we only have 3 columns
    # and the order is the same 1,2
    subData <- subData[numSubs, c(1,2,3)]
    # return the subset
    return(subData)
  }
  
  # retrieve the distinct state values from the dataframe
  states <- sqldf('SELECT DISTINCT myData.State FROM myData ')
  # can't use list so convert to vector of characters
  states <- sort(unique(states[,1]))
  
  df <- as.data.frame(do.call(rbind, lapply(states, HospDataFun, num)), row.names=states)
  colnames(df) <- c("hospital", "state")
  return (df)
}

# head(rankall1("heart attack", 20), 10)
# tail(rankall1("pneumonia", "worst"), 3)
# tail(rankall1("heart failure"), 10)