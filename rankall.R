rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        
        
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
        
        ## Tidying data
        
        subset_data <- data[,c(2,7,target_column)]
        subset_data[, 3] <- as.numeric(subset_data[, 3])
        subset_data <- subset_data[!is.na(subset_data[,3]),]
        

        ## For each state, find the hospital of the given rank
        
        colnames(subset_data) <- c("hospital", "state", "rate") 
        
        bystates <- split(subset_data, subset_data$state)
        outcome <- lapply(bystates,function(x,num) {
                x <- x[order(x$rate, x$hospital),]
                
                ## Check that num is valid        
                
                if ((is.na(as.integer(num) == TRUE)) & (num != "best") & (num != "worst")){
                        stop("NA")
                }
                
                if (num == "best"){
                        num_row <- 1
                } else {
                        if (num == "worst"){
                                num_row <- nrow(x)
                        } else {
                                num <- as.integer(num)
                                num_row <- num                
                        }
                }
                if ((num_row > nrow(subset_data)) | (num_row == 0) ){
                        stop("NA")
                }
                
                return(x$hospital[num_row])
                
        }, num)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        return (data.frame(hospital=unlist(outcome), state=names(outcome)))
        
}