#Part 1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#particulate matter data from the directory specified in the 'directory' argument 
#and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA. A prototype of the function is as follows

pollutantmean <- function(directory, pollutant, id = 1:332){
  #format the numbers so that it is not 1 but 001
  id_f <- formatC(id, flag = 0, width = 3)
  #create an empty numeric vector to store the results for each station
  means <- numeric()
  #start loop for each station
  for (i in seq_along(id)){
    station <- id_f[i]
    file <- paste(directory,"/", station,".csv",sep = "")
    values <- read.csv(file, header = TRUE, sep = ",")
    #Add the results to the vector
    means<-c(means, values[,pollutant])
  }
mean(means, na.rm = TRUE)
}
