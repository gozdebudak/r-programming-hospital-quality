## The rankhospital function returns the numth hospital in the given
## state and outcome
rankhospital <- function(state, outcome, num = "best"){
  setwd("data")
  ## Reading the data frame from CSV file
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Filtering the outcomes by given state
  stateOutcomes <- outcomes[outcomes$State == state,]
  ## Checking if the state is valid
  if(nrow(stateOutcomes) != 0){
    ## Setting the outcome type to the data frame column name
    if(outcome == "heart attack"){
      outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }else if(outcome == "heart failure"){
      outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }else if(outcome == "pneumonia"){
      outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else{
      setwd("..")
      stop("Invalid Outcome")
    }
    setwd("..")
    stateOutcome <- stateOutcomes[c("Hospital.Name",outcome)]
    ## Changing outcome column's type for ordering
    stateOutcome[c(outcome)] <- apply(stateOutcome[c(outcome)], 2,
                                      function(x) as.numeric(as.character(x)))
    ## Ordering outcomes first by outcome value, then by hospital name
    orderedOutcomes <- stateOutcome[order(stateOutcome[c(outcome)],
                                          stateOutcome[c("Hospital.Name")]),]
    ## Removing NA values
    orderedOutcomes <- orderedOutcomes[complete.cases(orderedOutcomes),]
    ## Setting the rank value by checking the given num 
    if(num == "best"){
      rank <- 1
    } else if(num == "worst"){
      rank <- nrow(orderedOutcomes)
    } else if (num > nrow(orderedOutcomes)-1){
      return(NA)
    } else{
      rank <- num
    }
    ## Returning the hospital at the given rank
    orderedOutcomes[rank,1]
    
  } else{
    # If the state is invalid, calls stop function
    setwd("..")
    stop("Invalid State")
  }
}