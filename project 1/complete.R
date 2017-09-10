complete <- function(directory, id = 1:332) {
        
        list_all_files <- list.files(directory, full.names = TRUE)
        ## creates a vector containing the path of all the spec data files
        case_vec <- vector()
        id_vec <- vector()
        combination_vec <-c()
        
        comp <- data.frame()
        ## names(comp) <- c("id", "nobs")
        
        for(i in id) {
                
                data <- read.csv(list_all_files[i])
                complete_vec <- complete.cases(data) ## creates vector of TRUE/FALSE
                
                case_vec <- sum(complete_vec) ## number of complete cases (TRUE)
                id_vec <- i
                
                combination_vec <- c(id_vec, case_vec)
                comp <- rbind(comp, combination_vec)
        }
        # comp <- cbind(comp, case_vec, id_vec)
        names(comp) <- c("id", "nobs")
        print(comp)
}

