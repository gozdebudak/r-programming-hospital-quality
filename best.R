## The function that finds the best hospital at given outcome and state
best <- function(state, outcome){
  ## Setting directory to data file
  setwd("data")
  ## Reading data from the CSV file
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Getting the state's outcomes data
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
      ## If the outcome is invalid, program enters here and calls stop function
      setwd("..")
      stop("Invalid Outcome")
    }
    setwd("..")
    stateOutcome <- stateOutcomes[c("Hospital.Name",outcome)]
    
    ## Changing outcome column's type for ordering
    stateOutcome[c(outcome)] <- apply(stateOutcome[c(outcome)], 2,
                        function(x) as.numeric(as.character(x)))
    ## Ordering outcomes first by outcome value, then by hospital name
    orderedOutcome <- stateOutcome[order(stateOutcome[c(outcome)],
                                         stateOutcome[c("Hospital.Name")]),]
    ## Returning the first(best) hospital that owns best outcome
    orderedOutcome[1,1]
  } else{
    # If the state is invalid, calls stop function
    setwd("..")
    stop("Invalid State")
  }
}