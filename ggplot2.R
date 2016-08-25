library(ggplot2)
library(RColorBrewer)

# Set working dir
setwd ("/Dev/Git/tester_survey")
#setwd("~/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv",
                   header = TRUE, sep =",")

# Make an index which ignores people that have never worked in software testing
testers <- which(mydata[,3] != "No")

# Apply this index to only get data for all the testers
mydata <- mydata[testers,]



# What made you apply for first testing job 
reasons <- mydata[,12]

str(reasons)

# Convert reasons to character which will allow each response to be split up
chareasons <- as.character(reasons)

# Split all the reason data on ; to get a list of industries
split_reasons <- strsplit(chareasons, split=";")

# Reasons are grouped in a list and need unlisting
all_reasons <- unlist(split_reasons)

str(all_reasons)

reason_table <- table(all_reasons)

sorted_reasons <- sort(reason_table, decreasing = TRUE)

#Manually shorten names of some reasons so they fit on the plot
reasonames <- names(sorted_reasons)  
reasonames[2] <- "Wanted to work with tech and computers" 
reasonames[3] <- "Thought testing would be fun" 
reasonames[4] <- "Unemployed and needed a job"
reasonames[5] <- "Wanted a new challenge"
reasonames[6] <- "Thought testing had good career prospects"
reasonames[7] <- "Thought better than previous job"
reasonames[8] <- "Did not like previous job"
reasonames[9] <- "Wanted a way to use computing knowledge"
reasonames[10] <- "Knew how to code but did not want to be a dev"
reasonames[11] <- "Wanted to work in software but couldn't code"
reasonames[12] <- "Other testers recommended testing"
reasonames[13] <- "Changed role within company"
reasonames[14] <- "Testing paid better than previous job"
reasonames[15] <- "Recommended by someone that was not a tester"
reasonames[16] <- "To gain entry to a specific company"
reasonames[17] <- "Job advert stated no experience necessary"
reasonames[18] <- "Thought testing would be easy"
reasonames[19] <- "Saw lots of testing jobs advertised"
reasonames[20] <- "Decided to apply after careers/jobs fair"
reasonames[21] <- "Applied so could relocate"

names(sorted_reasons) <- reasonames





reasonsdf <- as.data.frame(sorted_reasons)

colnames(reasonsdf) <- c("Reason", "Frequency")  

reasonsdf

str(reasonsdf)
factor(reasonsdf)

# "#f8efea,"#f1e8d6","#eaeac3"
#http://www.colorhexa.com/color-gradient

colours <- c( "#f8efea","#f1e8d6","#eaeac3", "#d2e3b0","#b3dc9f",  
             "#8ed48e", "#8ed48e","#7ecd98","#7ecd98", "#6ec6a9","#6ec6a9", "#60bfbf", "#60bfbf",
             "#5296b8","#5296b8", "#4569b1",  "#4569b1","#4948af","#4948af", "#3939aa", "#3939aa")

colours2 <- rev(colours)

#plot reasons people get into testing
ggplot(reasonsdf, aes(x=Reason, y=Frequency))+
  geom_bar(colour="#3939aa", fill= colours2, stat="identity")+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  coord_flip()+
  labs(x="Reason for application", y="Frequency")


