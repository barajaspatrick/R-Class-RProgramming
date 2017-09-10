corr <- function(directory, threshold = 0) {
        
        list_all_files <- list.files(directory, full.names = TRUE)

        current_col <- c() ## need empty vector for later
        correlations <-c()
        empty_file <- data.frame() ## need a empty dataframe to store data
        
        for(i in 1:332) { ## could use 'length(list_all_files)' to generalize
                
                currentfile <- read.csv(list_all_files[i])
                ## opens a data file in directory
                complete_file <- complete.cases(currentfile)
                ## creates a vector of true and false elements in current directory
                thresh <- sum(complete_file)
                
                if(thresh > threshold) {
                        
                        no_na_current_file <- currentfile[complete.cases(currentfile), ] ## we pick out the true "values"
                        ##  !!we should remove na values at this point!!
                        sulf <- no_na_current_file[ ,"sulfate"] 
                        ## create vector using sulfate element row
                        nite <- no_na_current_file[ ,"nitrate"] 
                        ## create vector using nitrate element row
                        current_col <- cor(sulf, nite) 
                        ## finds correlation between sulfer and nitrate
                        
                        correlations <- c(correlations, current_col)
                        ## adds the new correlation to existing vector
                        
                } else { 
                        
                        correlations <- c(correlations, c())
                        
                }
              
        }
correlations

}                
