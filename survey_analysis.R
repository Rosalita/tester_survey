# Survey response analysis

# Set working dir
# setwd ("/Dev/Git/tester_survey")
 setwd("/git/tester_survey")

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

length(WorkplaceHappinessIndex)

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
        ylab = "Workplace Hapiness Index",
        ylim=c(-12,12), 
        yaxt = "n")
axis(2, at = seq(-12, 12, by = 2))


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
        ylab="Frequency")
axis(2,at=seq(0,60,10))





# Experience is column 14
mydata2[,14]
exp <- mydata2[,14]
# Check levels
levels(exp)
# Reorder levels to be shortest to longest
exp <- relevel(exp, "", "less than a year", "1 - 2 years", "2 - 5 years", "5 - 10 years", "10 - 20 years", "More than 20 years")
levels(exp)
# Get rid of unused factor levels
exp <- droplevels(exp)
levels(exp)
# Check vector
exp
test_exp <- table(exp)

# Fix ordering
test_exp <- test_exp[c(5,1,3,4,2,6)]

# Tidy up naming
names(test_exp) <- c("Less than 1 year", "1 - 2 years", "2 - 5 years", "5 - 10 years", "10 - 20 years", "20+ years")

barplot (test_exp, space = NULL, 
         col = rainbow(10), 
         xlab="Duration testing", 
         ylab="Frequency")










# Are people who studied CS or related topic more or less happy

# Happy or not boxplot
# Create indexes first
CS <- which(mydata2[,18] == "Yes")
No_CS <- which(mydata2[,18] == "No")
length(CS)
length(No_CS)
# prevent vector recycling so find out how many values we are missing
to_pad <- length(WorkplaceHappinessIndex[CS]) - length(WorkplaceHappinessIndex[No_CS])
# now use rep to pad out the Not Happy version of Happy index with the missing
# value NA
Happy_data3 <- cbind(WorkplaceHappinessIndex[CS],c(WorkplaceHappinessIndex[No_CS],rep(NA, to_pad)))
# Fix rownames
colnames(Happy_data3) <- c("Studied Computing", "Did not study computing")
# Finsihed matrix
Happy_data3

# Now to plot
boxplot(Happy_data3, col = c("darkolivegreen1", "coral2"), ylim=c(-12,12), yaxt = "n")
axis(2, at = seq(-12, 12, by = 2))
# Looks like no difference 


# Edu lvl Vs Happy Index
# Does not seem to be a pattern

# education level boxplot
# Create indexes first
None <- which(mydata2[,17] == "None")
GCSE <- which(mydata2[,17] == "GCSEs or equivalent")
Alevel <- which(mydata2[,17] == "A-Levels or equivalent")
Foun <- which(mydata2[,17] == "Foundation course")
Bdegree <- which(mydata2[,17] == "Bachelors degree")
Mdegree <- which(mydata2[,17] == "Masters degree")
Phd <- which(mydata2[,17] == "Doctorate")

#total numbers: 
table(mydata2[,17])

# prevent vector recycling so find out how many values we are missing
(pad_None <- length(Bdegree) - length(None))
(pad_GCSE <- length(Bdegree) - length(GCSE))
(pad_Alevel <- length(Bdegree) - length(Alevel))
(pad_Foun <- length(Bdegree) - length(Foun))
(pad_Bdegree <- length(Bdegree) - length(Bdegree))
(pad_Mdegree <- length(Bdegree) - length(Mdegree))
(pad_Phd <- length(Bdegree) - length(Phd))

# now use rep to pad out the Not Happy version of Happy index with the missing
# value NA
Happy_data4 <- cbind(c(WorkplaceHappinessIndex[None], rep(NA, pad_None)),
                     c(WorkplaceHappinessIndex[GCSE], rep(NA, pad_GCSE)),
                     c(WorkplaceHappinessIndex[Alevel], rep(NA, pad_Alevel)),
                     c(WorkplaceHappinessIndex[Foun],  rep(NA, pad_Foun)),
                     c(WorkplaceHappinessIndex[Bdegree], rep(NA, pad_Bdegree)),
                     c(WorkplaceHappinessIndex[Mdegree], rep(NA, pad_Mdegree)),
                     c(WorkplaceHappinessIndex[Phd], rep(NA, pad_Phd)))


# Fix rownames
colnames(Happy_data4) <- c("None", "GCSE", "A-Level", "Foundation Course", "Bachelors Degree", "Masters Degree", "Phd")
# Finsihed matrix
Happy_data4

# Now to plot
boxplot(Happy_data4, col = c("coral2","chocolate1","darkgoldenrod1","darkolivegreen1","chartreuse4"), ylim=c(-12,12), yaxt = "n")
axis(2, at = seq(-12, 12, by = 2))
# Nothing to see here

# Findout if there is any link between how long someone has worked in testing
# and how happy they are


# Create indexes first
lessthanone <- which(mydata2[,14] == "less than a year")
onetotwo <- which(mydata2[,14] == "1 - 2 years")
twotofive <- which(mydata2[,14] == "2 - 5 years")
fivetoten <- which(mydata2[,14] == "5 - 10 years")
tentotwenty <- which(mydata2[,14] == "10 - 20 years")
twentyplus <- which(mydata2[,14] == "More than 20 years")

table(mydata2[,14])

# prevent vector recycling so find out how many values we are missing
(pad_lessthanone  <- length(twotofive) - length(lessthanone))
(pad_onetotwo <- length(twotofive) - length(onetotwo))
(pad_twotofive <- length(twotofive) - length(twotofive))
(pad_fivetoten <- length(twotofive) - length(fivetoten))
(pad_tentotwenty <- length(twotofive) - length(tentotwenty))
(pad_twentyplus <- length(twotofive) - length(twentyplus))


# now use rep to pad out the Not Happy version of Happy index with the missing
# value NA
Happy_data5 <- cbind(c(WorkplaceHappinessIndex[lessthanone], rep(NA, pad_lessthanone)),
                     c(WorkplaceHappinessIndex[onetotwo], rep(NA, pad_onetotwo)),
                     c(WorkplaceHappinessIndex[twotofive], rep(NA, pad_twotofive)),
                     c(WorkplaceHappinessIndex[fivetoten],  rep(NA, pad_fivetoten)),
                     c(WorkplaceHappinessIndex[tentotwenty], rep(NA, pad_tentotwenty)),
                     c(WorkplaceHappinessIndex[twentyplus], rep(NA, pad_twentyplus)))


Happy_data5


# Fix rownames
colnames(Happy_data5) <- c("less than a year", "1 - 2 years", "2 - 5 years", "5 - 10 years", "10 - 20 years", "20+ years")

# Finished matrix
Happy_data5

# Now to plot
boxplot(Happy_data5, col = c("coral2","chocolate1","darkgoldenrod1","darkolivegreen1","chartreuse4"), ylim=c(-12,12), yaxt = "n")
axis(2, at = seq(-12, 12, by = 2))

# Look for pattern between length of time testing and whether they would
# recomment testing

# Experience is column 14
mydata2[,14]
exp <- mydata2[,14]
# Check levels
levels(exp)
# Reorder levels to be shortest to longest
exp <- relevel(exp, "", "less than a year", "1 - 2 years", "2 - 5 years", "5 - 10 years", "10 - 20 years", "More than 20 years")
levels(exp)
# Get rid of unused factor levels
exp <- droplevels(exp)
levels(exp)
# Check vector
exp

# Recommending is column 13
mydata2[,13]
rec <- mydata2[,13]
#check levels
levels(rec)
#reorder levels to be from very unlikely to very likely
rec <- relevel(rec, "", "Very unlikely", "Unlikely", "Not sure", "Likely", "Very Likely")
levels(rec)
#get rid of the unused factor levels
rec <- droplevels(rec)
levels(rec)
rec


#compare two columns in a table
timerecommend <- table(rec, exp)

# Fix ordering
timerecommend <- timerecommend[c(5,3,2,1,4),c(5,1,3,4,2,6)]

barplot(timerecommend, space = NULL, 
        col = c("salmon", "lightgoldenrod2", "seagreen1", "lightblue", "ivory4"),
        beside = FALSE, legend.text=TRUE, xlab="length of time spent testing", ylab="likelyhood to recommend testing")

# Compare level of education with whether or not studied computing

# Level of education is column 17 of mydata2
# Studied cs is column 18 of mydata2
edu <- mydata2[,17]
# Inverted in to make index
index <- !edu %in% "None"
edu <- edu[index]
edu <- droplevels(edu)
# Extract studied CS or not data
cs <- mydata2[,18]
# Remove those who did not study at all
cs <- cs[index]
cs <- droplevels(cs)
studycs <- table(cs, edu)
# Fix ordering
studycs <- studycs[c(2,1),c(5,1,4,2,6,3)]


barplot(studycs, space = NULL, 
        col = c("seagreen1", "salmon"),
        beside = FALSE, legend.text=TRUE, xlab="education level", ylab="studied computing")


# Which industries people have tested in
industry <- mydata2[,15]

# Need to split the string up into a vector
str(industry)
# industry is currently a factor with 126 levels
industry <- as.character(industry)
# industry converted to character
str(industry)
table(industry)

# split multiple options on ;
industry2 <- strsplit(industry, split=";")
industry2

# This gives a list which we unlist
industry3 <- unlist(industry2)
industry3

# Now need to clean data up
write(industry3, file = "indy3.txt") 

# Read back in
industry4 <- scan(file = "indy_clean.txt", what = character(), sep = "\n")

indy_tab <- table(industry4)
indy_tab <- sort(indy_tab, decreasing = TRUE)

barplot(indy_tab)
pie(indy_tab)



# Test how long it took to onboard with whether this would put someone off changing job
putoff <- which(mydata[,29] == "Yes")
putoff
cput<- mydata[putoff,]
timeputoff <- mydata[putoff,27]
timeputoff
barplot(table(timeputoff))


notputoff <- which(mydata[,29] == "No")
timenotputoff <- mydata[notputoff, 27]
barplot(table(timenotputoff))


barplot(table(mydata[,29],mydata[,27]))


# If you knew you wated to work in testing whilst studying
want2test <- mydata[,19]
# Remove blank responces from those who did not study
want2test <- want2test[!want2test == ""]
# ifelse to convert Yes to TRUE and No to FALSE
want2test <- ifelse(want2test == "Yes", TRUE, FALSE)
# Check numbers
table(want2test)
# Mean of a logical vector * 100 gives percentage true
round(mean(want2test)* 100, digits = 1)
