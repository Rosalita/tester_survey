# Survey response analysis

# Set working dir
setwd("/Users/Rosie/Desktop/")

# Read in data
mydata <- read.csv("Software Testing Survey (187).csv", 
                  header = TRUE, sep =",")


# Exclude those not testing currently
Current_testers <- which(mydata[,2] == "Yes")
mydata <- mydata[Current_testers,]

# Colnames
colnames(mydata)

# No. cols and rows etc.
dim(mydata)
nrow(mydata)
ncol(mydata)

# Look at rows 1-6
head(mydata)

# Sub set out cols 30-53, convert factors to logical vectors
HappyData <- apply(mydata[,30:53], 2, as.logical)

# Vector index of pos questions
pos_index <- c(1, 2, 4, 7, 8, 10, 11, 12, 14, 15, 20, 21)

# Vector index of neg questions
neg_index <- c(3, 5, 6, 9, 13, 16, 17, 18, 19, 22, 23, 24)

# Pos questions out score up to +12 +1 for each True answer
pos_score <- rowSums(HappyData[,pos_index])
# Neg questions out score up to -12 -1 for each True answer 
neg_score <- -rowSums(HappyData[,neg_index])
# Add neg and pos scores to make final score
HappyScore <- pos_score+neg_score
# Pople at zero are neither happy or sad
summary(HappyScore)



# Histogram
breaks <- c(min(HappyScore):max(HappyScore))
hist(HappyScore, 
     breaks = breaks,
     xlim = c(-12,12),
     xlab = "Happyness Score",
     col = "darkolivegreen1",
     xaxt = "n",
     main = "Histogram of Tester Happyness"
     )
axis(1, at = seq(-12, 12, by = 1))
abline(v = mean(HappyScore), col = "blue", lty = 5, lwd = 2)
legend(-12,15,
       legend=paste0("mean = ", round(mean(HappyScore), digits=1)), 
       col = "blue", 
       lty = 5, 
       lwd = 2
)

# Next question for each Happyness score how likley are people to change job,
# nothing impressive here.
# cbind HappyScore with subsetted mydata
mydata2 <- cbind(mydata[,1:29],HappyScore)
leave_factor <- mydata2[,9]
# Check levels
levels(leave_factor)
# We need levels to be a score 1 to 5
levels(leave_factor) = c("", 4, 3, 2, 5, 1)
# Convert factor to character to numeric to obtain values
leave_vector <- as.numeric(as.character(leave_factor))

# Happy or not boxplot
# Create indexes first
Happy <- which(mydata2[,8] == "Yes")
Not_Happy <- which(mydata2[,8] == "No")
# prevent vector recycling so find out how many values we are missing
to_pad <- length(HappyScore[Happy]) - length(HappyScore[Not_Happy])
# now use rep to pad out the Not Happy version of Happy index with the missing
# value NA
Happy_data <- cbind(HappyScore[Happy],c(HappyScore[Not_Happy],rep(NA, to_pad)))
# Fix rownames
colnames(Happy_data) <- c("Happy", "Not Happy")
# Finsihed matrix
Happy_data
# Summary
summary(Happy_data)

# Now to plot
boxplot(Happy_data, col = c("darkolivegreen1", "coral2"), ylim=c(-12,12), yaxt = "n", main = 'Happyness index for "Happy" and "Not happy" groups')
axis(2, at = seq(-12, 12, by = 2))

# Stay or leave boxplot
# Create indexes first
VL <- which(mydata2[,9] == "Very Likely")
L <- which(mydata2[,9] == "Likely")
N <- which(mydata2[,9] == "Not sure")
U <- which(mydata2[,9] == "Unlikely")
VU <- which(mydata2[,9] == "Very unlikely")

# prevent vector recycling so find out how many values we are missing
(pad_VL <- length(U) - length(VL))
(pad_L <- length(U) - length(L))
(pad_N <- length(U) - length(N))
(pad_U <- length(U) - length(U))
(pad_VU <- length(U) - length(VU))


# now use rep to pad out the Not Happy version of Happy index with the missing
# value NA
Happy_data2 <- cbind(c(HappyScore[VL], rep(NA, pad_VL)),
                     c(HappyScore[L], rep(NA, pad_L)),
                     c(HappyScore[N], rep(NA, pad_N)),
                     c(HappyScore[U],  rep(NA, pad_U)),
                     c(HappyScore[VU], rep(NA, pad_VU)))
                     
# Fix rownames
colnames(Happy_data2) <- c("Very likely", "likely", "Not sure", "Unlikely", "Very unlikely")
# Finsihed matrix
Happy_data2

# Now to plot
boxplot(Happy_data2, col = c("coral2","chocolate1","darkgoldenrod1","darkolivegreen1","chartreuse4"), ylim=c(-12,12), yaxt = "n")
axis(2, at = seq(-12, 12, by = 2))


# Are people who studied CS or related topic more or less happy
# Happy or not boxplot
# Create indexes first
CS <- which(mydata2[,18] == "Yes")
No_CS <- which(mydata2[,18] == "No")
length(CS)
length(No_CS)
# prevent vector recycling so find out how many values we are missing
to_pad <- length(HappyScore[CS]) - length(HappyScore[No_CS])
# now use rep to pad out the Not Happy version of Happy index with the missing
# value NA
Happy_data3 <- cbind(HappyScore[CS],c(HappyScore[No_CS],rep(NA, to_pad)))
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
Happy_data4 <- cbind(c(HappyScore[None], rep(NA, pad_None)),
                     c(HappyScore[GCSE], rep(NA, pad_GCSE)),
                     c(HappyScore[Alevel], rep(NA, pad_Alevel)),
                     c(HappyScore[Foun],  rep(NA, pad_Foun)),
                     c(HappyScore[Bdegree], rep(NA, pad_Bdegree)),
                     c(HappyScore[Mdegree], rep(NA, pad_Mdegree)),
                     c(HappyScore[Phd], rep(NA, pad_Phd)))


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
Happy_data5 <- cbind(c(HappyScore[lessthanone], rep(NA, pad_lessthanone)),
                     c(HappyScore[onetotwo], rep(NA, pad_onetotwo)),
                     c(HappyScore[twotofive], rep(NA, pad_twotofive)),
                     c(HappyScore[fivetoten],  rep(NA, pad_fivetoten)),
                     c(HappyScore[tentotwenty], rep(NA, pad_tentotwenty)),
                     c(HappyScore[twentyplus], rep(NA, pad_twentyplus)))


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
# Fix odering
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
# Mean of a logical vector * 100 gives percetage true
round(mean(want2test)* 100, digits = 1)
