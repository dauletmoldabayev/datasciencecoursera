# Programming Assignment 1: Air Pollution
# Prepared by Daulet Moldabayev 

complete <- function(directory = "specdata", id = 1:10){
        # set the working directory to the 'directory' temporarily
        setwd(directory)     
        
        # testing that 'id' is not greater than number of files in the 
        # directory
        
        # 1st: compute number of files in the 'directory'
        number_of_files <- length(list.files(
                                        pattern = "*.csv", full.names = T))
        # 2nd: check if values of 'id' are 
        # in range between 1 and 'number_of_files'
        if( length(id[id >0 & id < number_of_files + 1]) != length(id) ){
                print("Error: the values of 'id' are not in range!")
                break
        }
        
        # a handy function to construct filenames from 'id' values
        namestrings <- function(id){
                names <- vector( length(id), mode = "character" )
                for(i in 1:length(id)){
                        if(id[i]<10){
                                names[i] <- paste( 0, 0, id[i], 
                                                   ".csv", sep = "")
                        }
                        else if(id[i]>9 & id[i]<100){
                                names[i] <- paste( 0, id[i], ".csv", sep = "")        
                        } 
                        else{
                                names[i] <- paste( id[i], ".csv", sep = "")
                        }
                }
                names
        }
        
        # function to gather values of 'pollutant' accross files with given 'id'
        # ecluding 'NA'  
        
        filenames <- namestrings(id) # requested filenames
        # output data frame with columns 'id' and 'nobs' as integers
        output <- data.frame(id = integer(0), nobs = integer(0)) 
        
        # a handy function for appending new rows to the data frame 'output'
        insertrow <- function(dataframe, newrow){
                dataframe[ nrow(dataframe) + 1, ] <- newrow 
                dataframe
        }
        
        # select only 2 columns "sulfate" and "nitrate"
        selectpollutants <- c("NULL", NA, NA, "NULL") 
        # select "ID" column to read the id number
        selectid <- c("NULL", "NULL", "NULL", NA)
        
        for(name in filenames){ # loop through requested filenames
                # read the needed columns -- saves memory
                mydata <- read.csv(name, colClasses = selectpollutants)
                # compute rows without 'NA' values from data
                amount <- c(0)
                for(i in 1:length(mydata[[1]])){
                        
                        if(!is.na(mydata[[1]][i]) && !is.na(mydata[[2]][i])){
                                amount <- amount + 1
                        }
                }
                # collect amount of observations to final vector 'amount'
                amount <- as.integer(amount)
                # read the ID number
                idcolumn <- read.csv(name, nrows = 2, colClasses = selectid)
                idnumber <- idcolumn$ID[1]
                output <- insertrow(output, c(idnumber, amount))
                # free memory
                rm(mydata, idcolumn, amount, idnumber)
        }
        remove(filenames, selectpollutants, selectid )
        print(paste("The number of observations accross file ids ", 
                        min(id), ":" , max(id), " is computed!!!", sep =""))
        
        # set the working directory to back to the main directory
        setwd("..")
        output
}