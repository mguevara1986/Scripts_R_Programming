#Part 1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#particulate matter data from the directory specified in the 'directory' argument 
#and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA. A prototype of the function is as follows

pollutantmean <- function(directory, pollutant, id){
  id_f <- formatC(id, flag = 0, width = 3)
  means <- numeric()
  for (i in seq_along(id)){
    station <- id_f[i]
    file <- paste(directory, station,".csv",sep = "")
    values <- read.csv(file, header = TRUE, sep = ",")
    means<-c(means, values[,pollutant])
  }
mean(means, na.rm = TRUE)
}
