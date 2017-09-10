rankhospital <- function(state, outcome, num = "best") {
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        p_states <- unique(df[,"State"]) ## list of possible states
        p_outcomes <- c("heart attack", "heart failure", "pneumonia") ## list of possible outcomes
        
                        ## check validity of inputs
        if (!(state %in% p_states)) {
                stop("invalid state")
        }
        if (!(outcome %in% p_outcomes)) {
                stop("invalid outcome")
        }
        
        df2 <- subset(df, df[,"State"] == state) ## create a subset dataframe bases on state selected
        
        if (num == "best"){
                num <- 1
        }
        if (num == "worst") { 
                num <- nrow(df2)
        }
        
        
        
        df2[, c(11, 17, 23)] <- sapply(df2[, c(11, 17, 23)], as.numeric) ## sets specific rows as numeric
        df2 <- df2[order(df2[,"Hospital.Name"]), ]
        
        
        if (outcome == "heart attack") {
                df3 <- df2[order(df2[,11]), ] ## sorts rows depending on heart attack rate
                best <- list(df3[num, c(2, 11)], num, "heart attack")
        } else if (outcome == "heart failure") {
                df3 <- df2[order(df2[,17]), ] ## sorts rows depending on heart attack rate
                best <- list(df3[num, c(2, 17)], num, "heart failure")
        } else {
                df3 <- df2[order(df2[,23]), ] ## sorts rows depending on heart attack rate
                best <<- list(df3[num, c(2, 23)], num, "pneumonia")
        }
        best <- unlist(best)
        names(best) <- c("hosital name", "mortality rates", "rank", "disorder")
        best
        
}

# 
# rankhospital("TX", "pneumonia", 10)
# [1] "SETON SMITHVILLE REGIONAL HOSPITAL"
