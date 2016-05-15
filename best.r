# initial look at files - for testing
#
# setwd("D:/Users/vince/Google Drive/training/Coursera/working/ProgrammingAssignment3")
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# outcome[, 11] <- as.numeric(outcome[, 11])
# You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])
#
# Assignment - class descrition
#
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
#
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).
#
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message \invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message \invalid outcome".

best <- function(state, outcome) {
  
  # For Development
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
  # order the data
  subData <- subData[order(subData[,index], na.last=TRUE),2]
  # remove NA
  subData <- na.omit(subData)
  
  # Return hospital name in that state with lowest 30-day death
  subData[1]
}

# testing
# best("TX", "heart attack")   # [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# best("TX", "heart failure")  # [1] "FORT DUNCAN MEDICAL CENTER"
# best("MD", "heart attack")   # [1] "JOHNS HOPKINS HOSPITAL, THE"
# best("MD", "pneumonia")      # [1] "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack")   # Error in best("BB", "heart attack") : invalid state
# best("NY", "hert attack")    # Error in best("NY", "hert attack") : invalid outcome
