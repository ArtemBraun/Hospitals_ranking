rankhospital <- function(state, outcome, num) {
        
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
        }
        
        if (outcome == "heart failure"){
                target_column <- 17
        }
        
        if (outcome == "pneumonia"){
                target_column <- 23
        }
        

        ## Return hospital name in that state with the given rank
        
        subset_data <- data[data[,7] == state,]
        subset_data <- subset_data[,c(2,target_column)]
        subset_data[, 2] <- as.numeric(subset_data[, 2])
        subset_data <- subset_data[!is.na(subset_data[,2]),]
        
        ## Check that num is valid        
        
        if ((is.na(as.integer(num) == TRUE)) & (num != "best") & (num != "worst")){
                stop("NA")
        }

        if (num == "best"){
                num_row <- 1
        } else {
                if (num == "worst"){
                        num_row <- nrow(subset_data)
                } else {
                        num <- as.integer(num)
                        num_row <- num                
                        }
        }
        if ((num_row > nrow(subset_data)) | (num_row == 0) ){
                stop("NA")
        }
        
        subset_data<- subset_data[order(subset_data[,2],subset_data[,1]),]
        
        subset_data[num_row,1]
                
        
}