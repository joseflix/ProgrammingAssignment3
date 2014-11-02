rankall <- function ( outcome, num = "best"){

	## convert num to a value we can easily handle
		
	if (num == "best") { 
		num <- "1" 
	} else if (num == "worst") { 
		num <- "-1" 
	} 
	suppressWarnings(num <- as.numeric(num))
	if (is.na(num)) { 
		print("not a valid rank provided")
	} 

	## Check that outcome is valid

	outcomes <- c("heart attack", "heart failure", "pneumonia")

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

	## building an empty data frame for the result
	rank_all <- data.frame( hospital = character(), state = character(), stringsAsFactors = FALSE)

    ## trim the dataset down from 46 column to 3 that we need
    outcome_data <- data[, c("Hospital.Name", "State", deaths)]
    
    ## we know, we WANT to force the NA converstions & remove NAs
    suppressWarnings(outcome_data[,3] <- as.numeric(outcome_data[, 3]))
    outcome_data <- na.omit(outcome_data)
   
   	## How many states are available in the dataset
	states <- unique(data[, "State"])
	
   	## loop over the states to obtain the results
	for (state_index in states) {
		hosp <- NA
		outcome_data2<-subset(outcome_data,outcome_data[,'State'] == state_index)
		ord_data<- outcome_data2[order(outcome_data2[,deaths],outcome_data2[,"Hospital.Name"]),]
        if ( num == -1 ) {
		 	hosp <- ord_data[nrow(ord_data),"Hospital.Name"]
		}else{
			hosp <- ord_data[num,"Hospital.Name"]
		}
		rank_all <- rbind(rank_all, data.frame( hospital = hosp, 
						  state = state_index, stringsAsFactors = FALSE))
	}

	## ordering the rank_all by states
	rank_all <- rank_all[order(rank_all$state), ]
	return(rank_all)
	
}