
setwd ("/Dev/Git/tester_survey")

mydata <- read.csv("survey_results_raw.csv", 
                   header = TRUE, sep =",")

# Make an index of all the people which currently work in testing
# People that currently do not work in testing have been excluded 
Current_testers <- which(mydata[,2] == "Yes")

#Apply this index to the data
mydata <- mydata[Current_testers,]


# Create a sub set of columns 30-53, these are true/false responses to positive and negative questions
# And also convert this sub set of true/false answers to logical vectors
HappyData <- apply(mydata[,30:53], 2, as.logical)

# Make a vector index of positive questions
pos_index <- c(1, 2, 4, 7, 8, 10, 11, 12, 14, 15, 20, 21)

# Make a vector index of negative questions
neg_index <- c(3, 5, 6, 9, 13, 16, 17, 18, 19, 22, 23, 24)

# Positive questions score +1 for each True answer, pos_score is a vector of scores from 0 to +12 
pos_score <- rowSums(HappyData[,pos_index])

# Negative questions score -1 for each True answer, neg_score is a vector of scores from 0 to -12  
neg_score <- -rowSums(HappyData[,neg_index])

# Add negative and positive scores to make the final score named Happiness Index
# The happiness index is a reflection of how positive or negative a job is
WorkplaceHappinessIndex <- pos_score+neg_score

#bind the workplace happiness index into a final column
mydata <- cbind(mydata, WorkplaceHappinessIndex)
levels(mydata[,11])

saddest_tester <- which(mydata[,11] == "None of the above")

saddest_tester <- mydata[saddest_tester,]
