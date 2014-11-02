rankhospital <- function ( state, outcome, num = "best" ) {
	
	if (num == "best") { 
		num <- "1" 
	} else if (num == "worst") { 
		num <- "-1" 
	} 
	suppressWarnings(num <- as.numeric(num))
	if (is.na(num)) { 
		print("not a valid rank provided")
	} 
	
	## Check that state and outcome are valid

	states <- unique(data[, "State"])
	outcomes <- c("heart attack", "heart failure", "pneumonia")
	
	if (! state %in% states & ! outcome %in% outcomes) {
		stop("invalid state and outcome")
    }	
    if (! state %in% states) {
    	   stop("invalid state")
    }
    if (! outcome %in% outcomes) {
	   stop("invalid outcome")
    }
    
	## Read outcome data
	
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character") ## loading data
	
    ## Return hospital name in that state with lowest 30-day death rate
    
    if (outcome == "heart attack") {
    		deaths <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
		deaths <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else {
		deaths <-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
    
    ## trim the dataset down from 46 column to 3 that we need
    outcome_data <- data[, c("Hospital.Name", "State", deaths)]
    
    ## and dump all but the state we are interested in
    outcome_data<-subset(outcome_data,outcome_data[,'State'] == state)
    
    ## we know, we WANT to force the NA converstions & remove NAs
    suppressWarnings(outcome_data[,3] <- as.numeric(outcome_data[, 3]))
    outcome_data <- na.omit(outcome_data)
    
    ord_data<- outcome_data[order(outcome_data[,deaths],outcome_data[,"Hospital.Name"]),]
    
    if ( num == -1 ) {
	 	ord_data[nrow(ord_data),"Hospital.Name"]
	}else{
		ord_data[num,"Hospital.Name"]
	}
	
}