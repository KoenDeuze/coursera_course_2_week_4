rankhospital <- function(state, outcome, rank_num='best') {
        
        ## check if the user input for rank is valid
        if(is.character(rank_num)) {
                if(rank_num %in% c('best','worse') == FALSE) {
                        stop('invalid user-input: rank')
                }
        }
        
        ## read the hospital-data
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
        
        ## for the best hospital
        if(rank_num == 'best') {
                
                 ## filter the hospital with the lowest (MIN) mortality
                care_data_e <- care_data_d[which(care_data_d[,2]==min(care_data_d[,2],na.rm = TRUE)),]
                
                ## get the hospital name
                best_hospitals <- care_data_e$Hospital.Name
                best_hospital <- sort(best_hospitals)[1]
                
        }
        
        
        ## for the worst hospital
        if(rank_num == 'worst') {
                
                ## filter the hospital with the highest (MAX) mortality
                care_data_e <- care_data_d[which(care_data_d[,2]==max(care_data_d[,2],na.rm = TRUE)),]
                
                ## get the hospital name
                best_hospitals <- care_data_e$Hospital.Name
                best_hospital <- sort(best_hospitals)[1]
                
        }
        
        ## for a given rank (user input)
        if(is.numeric(rank_num) & rank_num < length(care_data_d$Hospital.Name)) {
        
                ## remove the NA's
                care_data_e <- care_data_d[!is.na(care_data_d[,2]),]
                
                ## remove duplicate rows and sort an the mortality colum (2)
                care_data_f <- unique(sort(care_data_e[,2]))
                
                ## create an empty vector
                care_data_g <- c()
                
                
                for(i in 1:length(care_data_f)) {
                        temp <- which(care_data_e[,2]==care_data_f[i])
                        care_data_g <- rbind(care_data_g,care_data_e[temp,])
                }
                
                ## get the mortality rate from the hospital that has the rank from the unser-input 
                rank_rate <- care_data_g[rank_num,2]

                ## all hospitals that have a lower rate
                care_data_i <- which(care_data_g[,2] < rank_rate)

                ## all hospitals that have the same rate
                care_date_k <- which(care_data_g[,2] == rank_rate)

                ## the user-input rank minus the the number of hospitals that have a lower rate                
                rank_num_2 <- rank_num-length(care_data_i)

                
                ## al the hospitals that have the same mortality
                best_hospitals <- sort(care_data_g[care_date_k,1])

                ## the hospital on the rank the user has given
                best_hospital <- best_hospitals[rank_num_2]
                
        }
        
        if(is.numeric(rank_num) & rank_num > length(care_data_d$Hospital.Name)) {
                ## return NA when the given rank is higher than the number of hospitals in a state
                return(NA)
                
        }
                
                
        
        
        
        
        best_hospital
        
        
}