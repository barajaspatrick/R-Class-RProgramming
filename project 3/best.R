
best <- function(state, outcome) {
        
        
        table1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        st <- unique(table1$State) ## creates a list of the states in our data table
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if (!(state %in% st)) {
                stop("invalid state")
        }
        if (!(outcome %in% outcomes)) {
                stop("invalid state")
        }
        ##col_by_state <- subset(table1, table1[ ,"State"] == state)
        
        col_by_state <- table1[table1$State == state, ]
        col_by_state[ , c(11, 17, 23)] <- sapply(col_by_state[ ,c(11, 17, 23)], as.numeric)
        col_by_state <- col_by_state[order(col_by_state[, 2]), ] ## alphabetizes list
                ## will automatically pick is min.
        
        if (outcome == "heart attack") {
                bestie <- col_by_state[which.min(col_by_state[ ,11]), "Hospital.Name"]
        }
        else if (outcome == "heart failure") {
                bestie <- col_by_state[which.min(col_by_state[ ,17]), "Hospital.Name"]
        }
        else {
                bestie <- col_by_state[which.min(col_by_state[ ,23]), "Hospital.Name"]
        }
        
        bestie
}


