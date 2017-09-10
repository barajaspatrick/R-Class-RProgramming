rankall <- function( outcome, num = "best") {
        
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        
        ## check for valid outcome
        if (!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        if (num == "best") { 
                num <- 1
        }
        
        
        df[, c(11, 17, 23)] <- sapply(df[, c(11, 17, 23)], as.numeric)
        df2 <- subset(df[ ,c(2, 7, 11, 17, 23)])
                ## gets rid of any information we dont need
                ##df2 <- subset(df, df[ ,c(2, 7, 11, 17, 23)])
                ## hospital.name, state, heart attack, heart failure, pneumonia 
        
        colnames(df2) <- c("hospital.name", "state", "heart.attack", "heart.failure", "pneumonia")
        df2 <- split(df2, df$State)
        
        
        
        ## order data frame based on condition input
        if (outcome == "heart attack"){
                df2 <- lapply(df2, function(df2){
                        df2[order(df2$heart.attack, df2$hospital.name, na.last = NA),]
                })
                
                subs <- function(df2) {
                        subset(df2, df2[,3])
                        }
                
                
                df2 <<- lapply(df2, subs)
                
                
        } else if (outcome == "heart failure") {
                df2 <- lapply(df2, function(df2){
                        df2[order(df2$heart.failure, df2$hospital.name, na.last = NA),]
                })
        } else { 
                df2 <- lapply(df2, function(df2){
                        df2[order(df2$pneumonia, df2$hospital.name, na.last = NA),]
                })
        }
        
        ## ok so if "worst" is input i want the last row from every data frame in list "df2"
        if (outcome == "worst")  {

                df3 <- data.frame()
                
                for( i in 1:length(df2)) {
                        o <<- do.call(rbind.data.frame, df2[i])
                        na.omit(o)
                        last_element <- tail(o, 1)
                        df3 <- rbind(df3, last_element)
                }
                
        #                 
        # df3 <- lapply(df2, tail(df2,1))
        #          df3 <- lapply( function(df2) {df2[complete.cases(df2),]})
        #          df3 <- lapply(df3, function(tail){ tail(df3, 1)})

        } else { 
                
        df3 <- lapply(df2, function(df2) df2[num,])
        }
        
        df3
}   


