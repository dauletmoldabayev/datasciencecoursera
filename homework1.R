# R programming course
# Homework 1
# Prepared by Daulet Moldabayev

# Setting the correct working directory
setwd("~/Desktop/Rprog")

# Reading data from the file "hw1_data.csv" into variable "data"
data <- read.csv("hw1_data.csv")

# Printing out the columns' names of "data"  -- Q11
Q11 <- colnames(data)

# Printing out the first 2 rows of the "data"  -- Q12
Q12 <- data[1:2,]

# Printing out number of rows in "data"  -- Q13
Q13 <- nrow(data)

# Printing the last 2 rows of "data"  -- Q14
Nr <- nrow(data)
Q14 <- data[c(Nr - 1):Nr,]

# Printing the value of "Ozone" in the 47th row of "data"  -- Q15
Q15 <- data[47, "Ozone"]

# Finguring out number of missing values in the "Ozone" column of "data"  -- Q16
bad <- is.na(data[,"Ozone"])
subdata <- data[bad,"Ozone"]
Q16 <- length(subdata)  # can also be written as <- length(data[bad, "Ozone"])

# The mean of "Ozone" column excluding NA values  -- Q17
Q17 <- mean(data[!bad, "Ozone"])

# Subsetting "data" according to required conditions and
# calculating the mean for "Solar.R" column  -- Q18

conditioned_data_SolarR <-
        subset.data.frame(data, subset = Ozone > 31 &cTemp > 90, 
                                                select = Solar.R , drop = T)

Q18 <- mean(conditioned_data_SolarR)

# Subsetiing "data" with "Month"==6 and calculating the mean of "Temp"  -- Q19

conditioned_data_Temp <-
        subset.data.frame(data, subset = Month == 6,
                                        select = Temp, drop = T)

Q19 <- mean(conditioned_data_Temp)

# Subsetting "data" by "Month"==5 & !is.na("Ozone") and taking maximum of
# "Ozone"  -- Q20

conditioned_data_Ozone <- subset.data.frame(
                data, subset = Month == 5 & !is.na(Ozone),
                                        select = Ozone, drop = T)

Q20 <- max(conditioned_data_Ozone)
