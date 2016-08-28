#Part 3
#Write a function that takes a directory of data files and a threshold for 
#complete cases and calculates the correlation between sulfate and nitrate for
#monitor locations where the number of completely observed cases (on all variables) 
#is greater than the threshold. The function should return a vector of correlations
#for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return
#a numeric vector of length 0. 
#A prototype of this function follows

corr <- function (directory, threshold = 0) {
  #get all the files inside the directory
  files <- list.files(path = directory)
  #create an empty numeric vector to store the correlations for each station
  r<-c()
  #start a loop for each station
  for (i in seq_along(files)){
    station <- paste(directory, files[i],sep = "/")
    values <- read.csv(station, header = TRUE, sep = ",")
    #start loop for each row of each station
    N <- nrow(values)
    n <- 0
    for (j in 1:N){
      if (is.na(values[j, "nitrate"]) == FALSE && is.na(values[j, "sulfate"]) == FALSE) {
        n <- n+1
      }
    }
    if (n > threshold){
      r <- c(r, cor(values[,"sulfate"], values[,"nitrate"], use = "complete.obs"))
    }
  }
  if (length(r) == 0) {
    c(0)
  } else {
    r
  }
}