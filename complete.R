#Part 2
#Write a function that reads a directory full of files and reports the number of 
#completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of
#the file and the second column is the number of complete cases. 
#A prototype of this function follows

complete <- function(directory, id){
  #format the numbers so that it is not 1 but 001
  id_f <- formatC(id, flag = 0, width = 3)
  #create an empty dataframe to store the results for each station
  report <- data.frame(id = numeric(), nobs = numeric())
  #create a variable to count number of complete cases
  n <- 0
  #start loop for each station
  for (i in seq_along(id)){
    station <- id_f[i]
    file <- paste(directory,"/",station,".csv",sep = "")
    values <- read.csv(file, header = TRUE, sep = ",")
    #start loop for each row of each station
    N <- nrow(values)
    for (j in 1:N){
      if (is.na(values[j, "nitrate"]) == FALSE && is.na(values[j, "sulfate"]) == FALSE) {
        n <- n+1
      }
    }
    report[i, "id"] <- id[i]
    report[i,"nobs"] <- n
    n <- 0
  }
  report
}
