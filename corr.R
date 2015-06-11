# Coursera "R programming" course (June 1 - June 29, 2015)
# Programming Assignment 1: Air Pollution
# Part 3: corr.R
# Prepared by Daulet Moldabayev 

corr <- function(directory , threshold = 0){
        # set the working directory to the 'directory' temporarily
        setwd(directory)     
        
        # read names of all files in the 'directory'
        filenames <- list.files( pattern = "*.csv", full.names = T )
        
        # compute number of complete observations in each file
        output <- complete(directory = ".", id = 1:length(filenames))
        
        # get IDs of files with 'nobs' greater than 'threshold'
        myids <- output$id[output$nobs > threshold]
        
        # if there ARE NO files with 'nobs' > than 'threshold'
        # assign 0 to required output 'correlation'
        if(length(myids) == 0){
                correlation <- c(0)
        }
        # if there ARE files with 'nobs' > than 'threshold'
        # compute correlations between "sulfate" and "nitrate"
        # in each file and store them in ouput vector "correlations"
        if(length(myids) > 0){
                # select only the "sulfate" and "nitrate" columns
                # when reading data form a file
                select <- c("NULL", NA, NA, "NULL") 
                # initialize the output vector "correlations"
                correlation <- c()
                # loop through ID numbers of files that satifsy 'threshold'
                for(i in myids){
                        name <- filenames[i]
                        mydata <- read.csv(name, header = TRUE,
                                                colClasses = select)
                        # delete lines with 'NA'
                        mydata <- na.omit(mydata)
                        # compute correlation between "sulfate" and "nitrate"
                        correl <- cor(mydata$sulfate, mydata$nitrate)
                        # append the computed value to 
                        correlation <- c(correlation, correl)
                }
        }
        remove(filenames, select, mydata, correl, ouput, myids)
        # set the working directory to back to the main directory
        if(directory != "."){
                setwd("..")                
        }
        correlation
}