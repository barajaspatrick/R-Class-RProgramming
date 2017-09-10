# setwd("~/Documents/Coursera_WorkingDir/first_assignment")

pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        list_all_files <- list.files(directory, full.names = TRUE) 
        ## tells us the contents of the desiered directory
        empty_file <- data.frame() ## need a empty dataframe to store data
        
        for(i in id) {
                
                empty_file <- rbind(empty_file, read.csv(list_all_files[i])) 
                ##row-binds csv files
        }
        
        empty_file_pol <- empty_file[ , pollutant] 
        ## creates a vector out of the "pollutant" column elements
        
        mean_pol <- mean(empty_file_pol, na.rm = TRUE)
        ## finds the mean of the "empty_file_pol" vector dropping missing values 
        
        mean_pol

}
        