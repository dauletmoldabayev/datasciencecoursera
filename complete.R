# Coursera "R programming" course (June 1 - June 29, 2015)
# Programming Assignment 1: Air Pollution
# Part 2: complete.R
# Prepared by Daulet Moldabayev 

complete <- function(directory , id = 1:10){
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
        
        # take file-names that are required
        filenames <-  list.files(pattern = "*.csv", full.names = T)
        filenames <- filenames[id]
        
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
                mydata <- na.omit(mydata)
                # collect amount of observations to final vector 'amount'
                amount <- as.integer(length(mydata[[1]]))
                # read the ID number
                idcolumn <- read.csv(name, nrows = 2, colClasses = selectid)
                idnumber <- idcolumn$ID[1]
                output <- insertrow(output, c(idnumber, amount))
                # free memory
                rm(mydata, idcolumn, amount, idnumber)
        }
        remove(filenames, selectpollutants, selectid )
        # print(paste("The number of complete observations accross file ids ", 
        #               min(id), ":" , max(id), " is computed!!!", sep =""))
        
        # set the working directory to back to the main directory
        if(directory != "."){
                setwd("..")                
        }

        output
}