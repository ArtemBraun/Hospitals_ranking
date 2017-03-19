best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
	## Check that state and outcome are valid
        
	if (!any(state == state.abb)){
                stop("invalid state")
        }
        if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        
        if (outcome == "heart attack"){
                target_column <- 11
                data[, 11] <- as.numeric(data[, 11])
        }
        
        if (outcome == "heart failure"){
                target_column <- 17
                data[, 17] <- as.numeric(data[, 17])
        }
        
        if (outcome == "pneumonia"){
                target_column <- 23
                data[, 23] <- as.numeric(data[, 23])
        }
        
        ## Return hospital name in that state with lowest 30-day death

        subset_data <- data[data[,7] == state,]
        subset_data <- na.omit(subset_data)
	subset_data <- subset_data[,c(2,target_column)]
        target_min <- min(subset_data[,2])
        subset_data<- subset_data[subset_data2[,2] == target_min,]
        hospital_name <- sort(subset_data[,1])
        hospital_name[1]
        
}