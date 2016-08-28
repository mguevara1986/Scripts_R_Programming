#Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
#state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
#The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
#of the hospital that has the ranking specified by the num argument.
#The num argument can take values "best", "worst", or an integer indicating the ranking
#(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
#state, then the function should return NA. Hospitals that do not have data on a particular outcome should
#be excluded from the set of hospitals when deciding the rankings.
#Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
#of death. In those cases ties should be broken by using the hospital name (alphabetic order).
#One can use the order function to sort multiple vectors in this
#manner (i.e. where one vector is used to break ties in another vector).

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Check that state and outcome are valid. If not, stop the function and print message
  if ((state %in% data[,"State"]) == FALSE){
    stop("Invalid state")
  } 
  if ((outcome == "heart attack" || outcome == "heart failure" ||
       outcome == "pneumonia") == FALSE){
    stop("Invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  #split dataframe among state (column n. 7)
  data_states <- split(data,data[,7])
  #Select from the resulting list the datframe of the input state
  data_states_sel <- as.data.frame(data_states[state])
  #Convert to numeric the 30-day mortality columns and create reduced dataframe
  hospitals <- as.character(data_states_sel[, 2])
  Heart_Att<-as.numeric(data_states_sel[, 11])
  Heart_Fail<-as.numeric(data_states_sel[, 17])
  Pneumo<-as.numeric(data_states_sel[, 23])
  reduced<-data.frame(hospitals, Heart_Att, Heart_Fail, Pneumo)
  #Select what column to look at as a function of the input outcome
  if (outcome == "heart attack"){
    j <- 2
  }else if (outcome == "heart failure"){
    j <- 3
  }else if (outcome == "pneumonia"){
    j <- 4
  }
  #Remove hospitals with incomplete information
  reduced<-reduced[!(is.na(reduced[,j])==TRUE),]
  #Check if the number given by argument "num" is larger than the number of 
  #hospitals in that state, then the function should return NA
  N<-nrow(reduced)
  if (is.numeric(num) == TRUE & (N< num)){
    return("NA")
  } else {
    #Sort dataframe by mortality rate (j column) and hospital name (both ascending)
    reduced<-reduced[ order(reduced[,j], reduced[,1]), ]
    #Return name of hospital with the specified ranking
    if (num == "best"){
      hospital_name <- reduced[1,1]
    } else if (num == "worst") {
      hospital_name <- reduced[N,1]
    } else {
      hospital_name <- reduced[num,1]
    }
  }
hospital_name
}
