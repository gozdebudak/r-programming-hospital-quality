rankall <- function(outcome, num = "best"){
  setwd("data")
  ## Reading the data frame from CSV file
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  ## Changing outcome column's type for ordering
  outcomes[c(outcome)] <- apply(outcomes[c(outcome)], 2,
                                    function(x) as.numeric(as.character(x)))
  ## Ordering outcomes first by state then by outcome value and hospital name
  orderedOutcomes <- outcomes[order(outcomes[c("State")],
                                      outcomes[c(outcome)],
                                      outcomes[c("Hospital.Name")]),]
  ## Sub-setting data frame with "Hospital.Name", "outcome" and "State" columns
  orderedOutcomes <- orderedOutcomes[c("Hospital.Name",outcome, "State")]
  ## Setting the rank value by checking the given num 
  if(num == "best"){
    rank <- 1
  } else if(num == "worst"){
    ## This statement left blank because the rank value will be set
    ## in the for loop
  } else{
    rank <- num
  }
  ## Getting all state names and removing duplicates by unique function
  states <- unique(orderedOutcomes$State)
  ## Creating a data frame for numth hospital for each state
  hospitals <- data.frame(matrix(ncol = 2, nrow = 0))
  for(i in 1:length(states)){
    ## Getting the outcomes by state
    stateOutcome <- orderedOutcomes[orderedOutcomes$State == states[i],]
    ## If the num is equal to worst, setting rank with outcomes count
    if(num == "worst"){
      rank <- nrow(stateOutcome)
    }
    ## If the rank is bigger than outcomes count, adding NA to hospitals
    ## data frame for that state
    if(nrow(stateOutcome) < rank){
      hospitals <- rbind(hospitals, c(NA, states[i]))
    } else{
      ## If the rank is smaller than outcomes count, the NA values're removed
      stateOutcome <- stateOutcome[complete.cases(stateOutcome),]
      ## Setting rank to completed outcomes length
      if(num == "worst"){
        rank <- nrow(stateOutcome)
      }
      stateHospitals <- stateOutcome[, c("Hospital.Name", "State")]
      ## Adding the hospital for state that is at the rank
      hospitals <- rbind(hospitals, c(stateHospitals[rank,"Hospital.Name"],
                                      stateHospitals[rank,"State"]))
    }
  }
  ## Changing column names of data frame
  colnames(hospitals) <- c("hospital", "state")
  ## Returning the hospitals data frame
  hospitals

}