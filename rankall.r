rankall <- function(outcome, rank_num='best') {
        
        ## check if the user input for rank is valid
        if(is.character(rank_num)) {
                if(rank_num %in% c('best','worst') == FALSE) {
                        stop('invalid user-input: rank')
                }
        }
        
        ## read the hospital-data
        care_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
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
        
        ## the third column (mortality) should be numeric
        care_data_a[,3] <- as.numeric(care_data_a[,3])
        
        ## change the columnnames
        colnames(care_data_a) <- cbind('hospital', 'state', 'outcome')
        
        ## sorteer op state (unieke rijen)
        states <- sort(unique(care_data_a$state))
        
        ## split per state
        care_data_b <- split(care_data_a,care_data_a$state)
        
        ans_state <- c()
        ans_hospital <- c()
        
        for(letter in states) {
                care_data_c <- care_data_b[[letter]]
                care_data_c <- care_data_c[,c(1,3)]
              
                if (rank_num=='best') {
                        
                        care_data_d <- care_data_c[which(care_data_c[,2]==min(care_data_c[,2],na.rm=TRUE)),]
                        
                        hospitals<-care_data_d$hospital
                        hospital<-sort(hospitals)[1]
                }
                
                if (rank_num=='worst') {
                        
                        care_data_d <- care_data_c[which(care_data_c[,2]==max(care_data_c[,2],na.rm=TRUE)),]
                        
                        hospitals<-care_data_d$hospital
                        hospital<-sort(hospitals)[1]
                }
                
                if(is.numeric(rank_num) & rank_num < length(care_data_c$hospital)) {
                        
                        ## remove the NA's
                        care_data_d <- care_data_c[!is.na(care_data_c[,2]),]
                        
                        ## remove duplicate rows and sort an the mortality colum (2)
                        care_data_e <- unique(sort(care_data_d[,2]))
                        
                        ## create an empty vector
                        care_data_f <- c()
                        
                        
                        for(i in 1:length(care_data_e)) {
                                temp <- which(care_data_d[,2]==care_data_e[i])
                                care_data_f <- rbind(care_data_f,care_data_d[temp,])
                        }
                        
                        ## get the mortality rate from the hospital that has the rank from the unser-input 
                        rank_rate <- care_data_f[rank_num,2]
                        
                        ## all hospitals that have a lower rate
                        care_data_h <- which(care_data_f[,2] < rank_rate)
                        
                        ## all hospitals that have the same rate
                        care_date_i <- which(care_data_f[,2] == rank_rate)
                        
                        ## the user-input rank minus the the number of hospitals that have a lower rate                
                        rank_num_2 <- rank_num-length(care_data_h)
                        
                        
                        ## al the hospitals that have the same mortality
                        hospitals <- sort(care_data_f[care_date_i,1])
                        
                        ## the hospital on the rank the user has given
                        hospital <- hospitals[rank_num_2]

                        
                }

                if(is.numeric(rank_num) & rank_num > length(care_data_c$hospital)) {
                        ## return NA when the given rank is higher than the number of hospitals in a state
                        hospital <- NA
                        
                }
                
                ans_state <- c(ans_state,letter)
                ans_hospital <- c(ans_hospital,hospital)
                
        }
        
        df <- data.frame(ans_hospital,ans_state)
        colnames(df) <- c('hospital','state')
        
        df
        
        
        
        

}