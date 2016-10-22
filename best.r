best <- function(state, outcome) {
        ## Read the data from outcome-of-care-measures
        care_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state is valid
        if(!state %in% unique(care_data[,7])) {
                stop("invalid state")
        }
        
        ## Check that outcome is valid
        valid_outcome <- c("heart attack","heart failure","pneumonia")
        if(!outcome %in% valid_outcome) {
                stop("invalid outcome")
        }
        
        ## create a subset with the right data depending on the given outcome
        if(outcome=="heart attack") {
                care_data_a <- care_data[,c(2,7,11)]
        }
        
        if(outcome=="heart failure") {
                care_data_a <- care_data[,c(2,7,17)]
        }
        
        if(outcome=="pneumonia") {
                care_data_a <- care_data[,c(2,7,23)]
        }
        
        ## split the subset per state
        care_data_b <- split(care_data_a,care_data_a$State)
        
        ## filter the state from the userprompt
        care_data_c <- care_data_b[[state]]
        
        ## just use column 1 (hospital name) and 3 (mortality rate)
        care_data_d <- care_data_c[,c(1,3)]
        
        ## change the columnnames
        colnames(care_data_d) <- cbind('Hospital.Name', outcome)
        
        ## the second column (mortality) should be numeric
        care_data_d[,2] <- as.numeric(care_data_d[,2])
        
        ## filter the hospital with the lowest (MIN) mortality
        care_data_e <- care_data_d[which(care_data_d[,2]==min(care_data_d[,2],na.rm = TRUE)),]
        
        ## get the hospital name
        best_hospitals <- care_data_e$Hospital.Name
        best_hospital <- sort(best_hospitals)[1]
        
        
        ## Return hospital name in that state with lowest 30-day death
        best_hospital


}
