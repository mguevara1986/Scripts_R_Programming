setwd(dir = "/Users/Marc/Documents/Coursera/Data Science Specialization/R Programming/Week4/Assignment")
#1 Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)

#Because we originally read the data in as character 
#we need to coerce the column to be numeric.
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])