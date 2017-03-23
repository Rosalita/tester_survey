# Survey response analysis
# 2016 Rosie Hamilton 
# testingfuntime.blogspot.co.uk

#install ggplot2 & dependencies
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("scales")
#install.packages("tibble")

#import ggplot2
library(ggplot2)

# Set working dir
 setwd ("/Dev/Git/tester_survey")
#setwd("~/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv", 
                   header = TRUE, sep =",")

###################################################

# Section 1 - Why is it difficult to hire testers

###################################################

# Make an index of all the people which currently work in testing
# People that currently do not work in testing have been excluded 
Current_testers <- which(mydata[,2] == "Yes")

#Apply this index to the data
mydata <- mydata[Current_testers,]

# Total number of testers analysed 
nrow(mydata)

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

# Summary stats
summary(WorkplaceHappinessIndex)

# Count how many testers work in places with a maximum score on the Workplace Happiness Index
BestWorkplaces <- which(WorkplaceHappinessIndex == "12")

# Convert this to a percentage
round (length(BestWorkplaces) / length(WorkplaceHappinessIndex) * 100, digits = 1)

# Plot a Histogram of Workplace Happiness
breaks <- c(min(WorkplaceHappinessIndex):max(WorkplaceHappinessIndex))
hist(WorkplaceHappinessIndex, 
     breaks = breaks,
     xlim = c(-12,12),
     xlab = "Workplace Happiness Index",
     col = rainbow(20),
     xaxt = "n",
     main = "Histogram of Happiness at work"
)
axis(1, at = seq(-12, 12, by = 1))
abline(v = mean(WorkplaceHappinessIndex), col = "black", lty = 5, lwd = 2)
legend(-12,15,
       legend=paste0("mean = ", round(mean(WorkplaceHappinessIndex), digits=1)), 
       col = "black", 
       lty = 5, 
       lwd = 2
)


# Make a new dataframe to work with where all the true false question are replaced by Workplace Happiness Index
mydata2 <- cbind(mydata[,1:29],WorkplaceHappinessIndex)

# Tester happiness compared to workplace happiness index
# Create an index of all the testers that say they are happy 
Happy <- which(mydata2[,8] == "Yes")
# Create an index of all the testers that say they are not happy
Not_Happy <- which(mydata2[,8] == "No")

# Make a vector containing total numbers of Happy and Not Happy testers
HappyYesNo <- c(length(Happy), length(Not_Happy)) 
# Generate labels for pie chart containing % of Happy and Not Happy
pielabels <- c("Yes", "No")
percent <- round(HappyYesNo/sum(HappyYesNo)* 100, digits = 1)
pielabels <- paste(pielabels, percent)    
pielabels <- paste(pielabels, "%", sep="")

#Plot a pie chart of Happy vs Not Happy testers
pie(HappyYesNo, labels = pielabels, col = c("lawngreen", "red"), main = "Are you happy in your current testing job?")


#Look for pattern between testers saying they are happy or not and workplace happiness index

# Padding is needed to prevent vector recycling 
# find out the difference between number of people that are happy and not happy
to_pad <- length(WorkplaceHappinessIndex[Happy]) - length(WorkplaceHappinessIndex[Not_Happy])
# now use rep to pad out the Not Happy vector with missing NA values until it is the same length as the happy vector
# join two columns together for workplace happiness index of happy and not happy people 
HappyData <- cbind(WorkplaceHappinessIndex[Happy],c(WorkplaceHappinessIndex[Not_Happy],rep(NA, to_pad)))

#check the median values for happy and not happy groups

median(WorkplaceHappinessIndex[Happy])
median(WorkplaceHappinessIndex[Not_Happy])

# Fix Column Names on this HappyData
colnames(HappyData) <- c("Happy", "Not Happy")

# Summary
summary(HappyData)

# Now box plot the workplace happiness index against testers happiness
boxplot(HappyData, 
        col = c("cyan", "red"), 
        xlab = "Tester Happiness",
        ylab = "Workplace Happiness Index",
        ylim=c(-12,12), 
        yaxt = "n", 
        main = 'Workplace Happiness Index for Tester Happiness')
axis(2, at = seq(-12, 12, by = 2))


# Look at workplace happiness index for groups of testers based on how likely they are to look for a new testing job
# Create indexes first
VL <- which(mydata2[,9] == "Very Likely")
L <- which(mydata2[,9] == "Likely")
N <- which(mydata2[,9] == "Not sure")
U <- which(mydata2[,9] == "Unlikely")
VU <- which(mydata2[,9] == "Very unlikely")

# Find the largest group
length(VL)
length(L)
length(N)
length(U)
length(VU)

# The largest group is the "unlikely" group

# Prevent vector recycling by padding all the groups so they are the same size as the unlikely group 
# Find out how much padding needs to be added
(pad_VL <- length(U) - length(VL))
(pad_L <- length(U) - length(L))
(pad_N <- length(U) - length(N))
(pad_U <- length(U) - length(U))
(pad_VU <- length(U) - length(VU))


# Use rep function to pad out with the missing value NA, index this against worplace happiness index
# Then bind this data into a new dataframe
leave_testing <- cbind(c(WorkplaceHappinessIndex[VL], rep(NA, pad_VL)),
                       c(WorkplaceHappinessIndex[L], rep(NA, pad_L)),
                       c(WorkplaceHappinessIndex[N], rep(NA, pad_N)),
                       c(WorkplaceHappinessIndex[U],  rep(NA, pad_U)),
                       c(WorkplaceHappinessIndex[VU], rep(NA, pad_VU)))

# Fix column names
colnames(leave_testing) <- c("Very likely", "likely", "Not sure", "Unlikely", "Very unlikely")

#Median values
median(WorkplaceHappinessIndex[VL])
median(WorkplaceHappinessIndex[L])
median(WorkplaceHappinessIndex[N])
median(WorkplaceHappinessIndex[U])
median(WorkplaceHappinessIndex[VU])

# Box plox the groups of testers by likelihood to look for a new testing job against workplace happiness index

boxplot(leave_testing, 
        #col = c("coral2","chocolate1","darkgoldenrod1","darkolivegreen1","chartreuse4"), 
        col = rainbow(8),
        xlab = "Likelihood to look for a new testing job",
        ylab = "Workplace Happiness Index",
        ylim=c(-12,12), 
        yaxt = "n",
        main = "Workplace Happiness vs likelihood to leave"
        )
axis(2, at = seq(-12, 12, by = 2))

# ggplot 2
# transform the leave_testing matrix into a dataframe so can plot it with ggplot2
dfleave <- stack(as.data.frame(leave_testing))


# change the order of thefactor levels by specifying order explicity


dfleave$ind

#Levels keep being sorted alphabetically which is no good for plots
levels(dfleave$ind)  # shows the levels are "likely", "Not sure", "Unlikely", "Very likely", "Very unlikely"

# To reorder the levels use factor() and specify a new order
dfleave$ind <- factor(dfleave$ind,levels(dfleave$ind)[c(4,1,2,3,5)])



#plot in ggplot2
p <- ggplot(dfleave, aes(factor(ind),values))
p + geom_boxplot(outlier.shape = NA, aes(fill = ind)) + 
  coord_flip() + 
  ggtitle("Happiness of testers grouped by likelihood to look for a new job") +
  labs(x = "Likelihood to look for a new testing job", y = "Happiness Index") +
  guides(fill=FALSE) +
  scale_y_continuous(breaks = c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_fill_manual(values=c("#EA220A", "#EC620F", "#F2DE1A","#99F726","#35FC31")) +
  geom_jitter(colour ="black", alpha =0.5, width = 0.3, height = 0.3)



# Make some plots of how long people have worked in testing

# Create indexes first
lessthanone <- which(mydata2[,14] == "less than a year")
onetotwo <- which(mydata2[,14] == "1 - 2 years")
twotofive <- which(mydata2[,14] == "2 - 5 years")
fivetoten <- which(mydata2[,14] == "5 - 10 years")
tentotwenty <- which(mydata2[,14] == "10 - 20 years")
twentyplus <- which(mydata2[,14] == "More than 20 years")

table(mydata2[,14])

LTO <- length(lessthanone)
OTT <- length(onetotwo)
TTF <- length(twotofive)
FTT <- length(fivetoten)
TTT <- length(tentotwenty)
TP <- length(twentyplus)

plotcolnames <- c("0 - 1", "1 - 2", "2 - 5", "5 - 10", "10 - 20", "20+") 

barplot(c(LTO, OTT, TTF, FTT, TTT, TP ), 
        space = NULL, 
        names.arg = plotcolnames,
        col = rainbow(10), 
        ylim = c(0,60),
        xlab="Duration testing in years", 
        ylab="Frequency",
        main="Testing experience"
        )
axis(2,at=seq(0,60,10))


#add up all the testers with less than 2 years experience
lessthantwoyears <- LTO + OTT

#add up all the testers with more than 2 years experience
morethantwoyears <- TTF + FTT + TTT + TP

# Make a vector containing less than and more than 2 years experience
experience <- c(lessthantwoyears, morethantwoyears) 
# Generate labels for a pie chart containing % of lessthan and morethan
pielabels <- c("Less than 2 years", "More than 2 years")
percent <- round(experience/sum(experience)* 100, digits = 1)
pielabels <- paste0(pielabels, "\n", percent)    
pielabels <- paste(pielabels, "%", sep="")

#Plot a pie chart of two years experience
pie(experience, 
    labels = pielabels, 
    col = c("lawngreen", "red"), 
    main = "Tester experience levels",
    cex = 1)

