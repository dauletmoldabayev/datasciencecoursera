# Programming Assignment 1: Air Pollution
# Prepared by Daulet Moldabayev 

pollutantmean <- function(directory = "specdata", pollutant, id = 1:10){
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
        
        # make a handy function to construct filenames from 'id' values
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
        pollutantvalues <- function( dummy_variable ){
                filenames <- namestrings(id) # requested filenames
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
                        mydata <- mydata[!is.na(mydata)]
                        # collect data to final vector 'values'
                        values <- values + sum(mydata)
                        amount <- amount + length(mydata)
                }
                remove(filenames, prep, columns, select, mydata)
                c(values,amount) #this gives the required mean value
        }
        cleandata <- pollutantvalues()
        print(paste(c("Mean value for ", pollutant, " accross file ids ", 
                as.character(min(id)), ":" , as.character(max(id)),  
                        " is computed!!!"), collapse = ""))
        # set the working directory to back to the main directory
        setwd("..")
        cleandata[1]/cleandata[2]
}