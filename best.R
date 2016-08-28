#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
#Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
#be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals \b", \c",
#and \f" are tied for best, then hospital \b" should be returned).

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Create empty vector to save name of hospitals
  hospital_names <- c()
  # Check that state and outcome are valid. If not, stop the function and print message
  if ((state %in% data[,"State"]) == FALSE){
    stop("Invalid state")
  } 
  if ((outcome == "heart attack" || outcome == "heart failure" ||
      outcome == "pneumonia") == FALSE){
    stop("Invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  #split dataframe among state (column n. 7)
  data_states <- split(data,data[,7])
  #Select from the resulting list the datframe of the input state
  data_states_sel <- as.data.frame(data_states[state])
  #Convert to numeric the 30-day mortality columns.
  Hospitals <- as.character(data_states_sel[, 2])
  Heart_Att<-suppressWarnings(as.numeric(data_states_sel[, 11]))
  Heart_Fail<-suppressWarnings(as.numeric(data_states_sel[, 17]))
  Pneumo<-suppressWarnings(as.numeric(data_states_sel[, 23]))
  reduced<-data.frame(Hospitals, Heart_Att, Heart_Fail, Pneumo)
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
  #Sort dataframe by mortality rate (j column) and hospital name (both ascending)
  reduced<-reduced[ order(reduced[,j], reduced[,1]), ]
  hospital <- reduced[1,1]
  hospital
}