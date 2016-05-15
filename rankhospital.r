# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
#
# rankhospital("MD", "heart failure", 5)
#
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas ("TX"),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  # re-use from previous best.r function
  
  # For Development
  # num <- 4
  # state <- "TX"
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
  
  # retrieve the distinct state values from the dataframe
  states <- sqldf('SELECT DISTINCT myData.State FROM myData ')
  
  # check if state given is part of states available
  if(!state %in% states$State){
    stop("invalid state")
  }
  
  # Get the subset from dataframe for state requested
  subData <- subset(myData, State==state)
  # order by rate
  subData <- subData[order(subData[,index], subData[,2], na.last=TRUE),2]
  # remove NA
  subData <- na.omit(subData)
  
  # check values given / R doesn't handle switch default well
  # set default as if statement
  numSubs <- switch (num,
    best = 1,
    worst = length(subData)
  )
  if(is.null(numSubs)){ numSubs <- as.numeric(num) }
  
  # return subData index
  subData[numSubs]
}

# test
# rankhospital("TX", "heart failure", 4)      # [1] "DETAR HOSPITAL NAVARRO"
# rankhospital("MD", "heart attack", "worst") # [1] "HARFORD MEMORIAL HOSPITAL"
# rankhospital("MN", "heart attack", 5000)    # [1] NA

