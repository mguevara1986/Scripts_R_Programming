#Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
#(num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
#containing the hospital in each state that has the ranking specified in num. For example the function call
#rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
#are the best in their respective states for 30-day heart attack death rates. The function should return a value
#for every state (some may be NA). The first column in the data frame is named hospital, which contains
#the hospital name, and the second column is named state, which contains the 2-character abbreviation for
#the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
#hospitals when deciding the rankings.
#NOTE: For the purpose of this part of the assignment (and for eficiency), your function should NOT call
#the rankhospital function from the previous section.
#The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
#the function should throw an error via the stop function with the exact message "invalid outcome". The num
#variable can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better).
#If the number given by num is larger than the number of hospitals in that state, then the function should
#return NA.

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that outcome is valid
  if ((outcome == "heart attack" || outcome == "heart failure" ||
       outcome == "pneumonia") == FALSE){
    stop("Invalid outcome")
  }
  ## Create empty dataframe to store output
  hospital_state <- data.frame(hospital = character() ,state = character(), stringsAsFactors=FALSE)
  ## For each state, find the hospital of the given rank
  #Get vector with names of states
  state_names <- unique(data[,7])
  state_names<-sort(state_names)
  #split dataframe among state (column n. 7)
  data_states <- split(data,data[,7])
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  for (i in seq_along(state_names)){
    state <- state_names[i]
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
      hospital_name <- "NA"
    } else {
      #Sort dataframe by mortality rate (j column) and hospital name (both ascending)
      reduced<-reduced[ order(reduced[,j], reduced[,1]), ]
      #Return name of hospital with the specified ranking
      if (num == "best"){
        hospital_name <- as.character(reduced[1,1])
      } else if (num == "worst") {
        hospital_name <- as.character(reduced[N,1])
      } else {
        hospital_name <- as.character(reduced[num,1])
      }
    }
    #Save values in the dataframe
    hospital_state[i,1] <- hospital_name
    hospital_state[i,2] <- state
  }
  #Return the dataframe
  hospital_state
}