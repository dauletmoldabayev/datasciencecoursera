# Coursera "R programming" course (June 1 - June 29, 2015)
# Programming Assignment 1: Air Pollution
# Part 1: pollutantmean.R
# Prepared by Daulet Moldabayev 

pollutantmean <- function(directory , pollutant, id = 1:332){
        # set the working directory to the 'directory'
        setwd(directory)
        
        # testing that 'pollutant' got a correct value: "sulfate" or "nitrate"
        if(!identical(pollutant,"sulfate") & !identical(pollutant,"nitrate")){
                print("Error: pollutant name is not correct!")
                break
        }
        
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
        
        # function to gather values of 'pollutant' accross files with given 'id'
        # excluding 'NA'  
   
        # take file-names that are required
        filenames <-  list.files(pattern = "*.csv", full.names = T)
        filenames <- filenames[id]
        
        values <- c(0) # stores the sum of pollutant values
        amount <- c(0) # stores number of summed observations 
        # define which column is needed
        prep <- read.csv(filenames[1], nrows = 2)
        columns <- colnames(prep) 
        select <- rep("NULL", length(columns))
        select[ columns == pollutant ] <- NA
        for(name in filenames){ # loop through requested filenames
                # read the needed column -- saves memory
                mydata <- read.csv(name, colClasses = select)
                # exclude 'NA' values from data
                mydata <- na.omit(mydata)
                # collect data to final vector 'values'
                values <- values + sum(mydata[[1]])
                amount <- amount + length(mydata[[1]])
        }
        remove(filenames, prep, columns, select, mydata)

        # print(paste("Mean value for ", pollutant, " accross file ids ", 
        #            min(id), ":" , max(id), " is computed!!!", sep =""))
        # set the working directory to back to the main directory
        if(directory != "."){
                setwd("..")                
        }
        values/amount
}