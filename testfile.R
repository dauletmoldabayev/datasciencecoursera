directory <- "specdata"
filenames <- list.files(path = directory, pattern = "*.csv", full.names = T)

source('complete.R')

output <- complete(id = 1:332)
myids <- output$id[output$nobs > 5000]

if(length(myids) == 0){
        corr <- c(0)
}
if(length(myids) > 0){
        select <- c("NULL", NA, NA, "NULL") 
        corr <- c()
        
        for(i in myids){
                name <- filenames[i]
                mydata <- read.csv(name, header = TRUE, colClasses = select)
                mydata <- na.omit(mydata)
                correl <- cor(mydata$sulfate, mydata$nitrate)
                corr <- c(corr, correl)
        }
}
corr