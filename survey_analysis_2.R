# Survey response analysis
# 2016 Rosie Hamilton
# testingfuntime.blogspot.co.uk

# Set working dir
setwd ("/Dev/Git/tester_survey")
#setwd("~/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv",
                   header = TRUE, sep =",")

# Load treemap package
# install.packages("treemap")
library(treemap)


#####################################################

# Section 2 - A Snapshot of Software Testers in 2016

#####################################################


# Make an index which ignores people that have never worked in software testing
testers <- which(mydata[,3] != "No")

# Apply this index to only get data for all the testers
mydata <- mydata[testers,]

# Isolate all the industries that all the testers have tested in
industry <- mydata[,15]

# Check the structure of industry, currently a factor with 151 levels
str(industry)

# Convert industry to character which will allow each response to be split up
industry <- as.character(industry)

# Split all the industry data on ; to get a list of industries
industry <- strsplit(industry, split=";")

# Figure out which testers have n number of answers in industry
indy_counter <- function(x) {
  count <- length(industry[[x]])
  return(count)
}
# sapply() function to industry to get index
counts <- sapply(1:length(industry), indy_counter)

# Which testers have only worked in one industry
only_one_indy_index <- which(counts == 1 )

# Of these filter out those with free text responses for more than one word i.e. industry
# Grepl returns logical vector, Regex used matches on one or more white spaces between start and end
only_one_indy_index2 <- only_one_indy_index[grepl("^\\S+$", industry[only_one_indy_index])]

# Check that index lists all rows that only have 1 industry listed
mydata[only_one_indy_index2,15] 

# Observed that index missing single industry "social media" as it has a space in it
# Manually added the missing "social media" rows
only_one_indy_index2 <- c(only_one_indy_index2, 136, 173)
only_one_indy_index2 <-  sort(only_one_indy_index2)

# Check result by filtering the industries - good,  "social media" appears now
only_one_indy <- unlist(industry[only_one_indy_index2])

# Unlist the industries
industry <- unlist(industry)

# Write all these industries to a file so can manually clean up the "other" industry field
write(industry, file = "raw_industry.txt")

# Clean up done by splitting each industry listed in the "other" field onto a new line
# Clean up gave multiple industries with similar names the same name with the same case sensitivity
# E.g. "online gambling" and "gambling" and "betting" were renamed to "Gambling".

# Read the cleaned data back in
clean_industry <- scan(file = "clean_industry.txt", what = character(), sep = "\n")

# Tabulate industry frequencies
ind_tab <- table(clean_industry)

# As a data frame
ind_df <-data.frame(ind_tab)

# Find total number of testerss
total <- length(testers)

# Use total number of tester to calculate the relative frequency
relfreq <- ind_df[,2]/total

# cbind the relative frequency on to the industry data frame
ind_data <- cbind(ind_df, relfreq)

# Sort the data frame by relative frequency, largest first
ind_data <- ind_data[order(ind_data$relfreq, decreasing = TRUE) ,]

# Make new column
combined <- paste0(ind_data[,1], "\n", round(ind_data[,3] * 100, digits = 1), "%"  )

# Add to data frame
ind_data <- cbind(ind_data, combined)

# Treemap
png(filename = "treeplot.png", width = 600, height = 500, units = "px")
treemap(ind_data,
        index="combined",
        title="Tester Industry Experience",
        title.legend = "Total testers per industry",
        vSize="Freq",
        vColor = "Freq",
        type = "dens",
        aspRatio = NA,
        inflate.labels = FALSE,
        fontsize.labels = 14,
        fontsize.title = 18,
        lowerbound.cex.labels = 0,
        palette=rainbow(47, s = 1, start = 0, end = 0.8))
dev.off()

# Treemap HD
png(filename = "treeplotHD.png", width = 1920, height = 1080, units = "px")
treemap(ind_data,
        index="combined",
        title="Tester Industry Experience",
        title.legend = "Total testers per industry",
        vSize="Freq",
        vColor = "Freq",
        type = "dens",
        aspRatio = NA,
        inflate.labels = FALSE,
        fontsize.labels = 32,
        fontsize.title = 32,
        fontsize.legend = 32,
        lowerbound.cex.labels = 0,
        palette=rainbow(47, s = 1, start = 0, end = 0.8))
dev.off()

# Find all the testers that have only test in one industry
justone <- only_one_indy_index2

# Invert the just one index to get an index for testers which have tested in multiple industries
all <- c(1:186)
multi <- all [! all %in% justone]

# Apply these indexes to my data
one_industry <- mydata[justone,]
multi_industry <- mydata[multi,]

# Get number of industries
total_one <- nrow(mydata[justone,])
total_multi <- nrow(mydata[multi,])

perc <- c(round(total_one /186 * 100, digits = 1),round(total_multi /186 * 100, digits = 1) )
colnames <- c("Tested in only one Industry", "Tested in multiple industries")
colnames <- paste0(colnames, "\n", perc, "%"  )
ind_comp <- c(total_one, total_multi)

barplot(ind_comp,
        space = NULL,
        col = rainbow(10),
        names.arg = colnames,
        main="Industry Experience",
        ylim  = c(0,160),
        ylab="Number of testers"
        )

# Did testers have different job before becoming a tester, grouped by experience

# Indexes for experience groups
lessthanone <- which(mydata[,14] == "less than a year")
onetotwo <- which(mydata[,14] == "1 - 2 years")
twotofive <- which(mydata[,14] == "2 - 5 years")
fivetoten <- which(mydata[,14] == "5 - 10 years")
tentotwenty <- which(mydata[,14] == "10 - 20 years")
twentyplus <- which(mydata[,14] == "More than 20 years")

# Apply these indexes to mydata using column for did you have a different job or was test first job
lessthanone_job <- mydata[lessthanone,16]
onetotwo_job <- mydata[onetotwo ,16]
twotofive_job <- mydata[twotofive,16]
fivetoten_job <- mydata[fivetoten ,16]
tentotwenty_job <- mydata[tentotwenty,16]
twentyplus_job <- mydata[twentyplus,16]

# Checking levels show each of these vectors has factor w /3 levels 
str(lessthanone_job)
str(onetotwo_job)
str(twotofive_job)
str(fivetoten_job)
str(tentotwenty_job)
str(twentyplus_job)

# Drop the unused level "" from each group
lessthanone_job <- droplevels(lessthanone_job)
onetotwo_job <- droplevels(onetotwo_job)
twotofive_job <- droplevels(twotofive_job)
fivetoten_job <- droplevels(fivetoten_job)
tentotwenty_job <- droplevels(tentotwenty_job)
twentyplus_job <- droplevels(twentyplus_job)

# Store this data in tables
LTO <- table(lessthanone_job)
OTT <- table(onetotwo_job)
TTF <- table(twotofive_job)
FTT <- table(fivetoten_job)
TTT <- table(tentotwenty_job)
TP <- table(twentyplus_job)

# Up to two years is less than 1 plus one to two
UTT <- LTO + OTT

# Make a vector with all these tables combined
diff_job_before_tester <- c(UTT, TTF, FTT, TTT, TP)

# Populate a matrix with this data
matrix_jobs <- matrix(diff_job_before_tester, ncol=5, byrow= FALSE)

# Label the columns and rows of this matrix
rownames(matrix_jobs) <- c("No", "Yes")
colnames(matrix_jobs) <- c("< 2", "2 - 5", "5 - 10", "10 - 20", "20+")

# Use prop.table to convert numeric values to percentage values
matrix_jobs_perc <- prop.table(matrix_jobs, margin = 2)

# Multiple by 100 so the scale on the plot shows 0 - 100% instead of 0 - 1 %
matrix_jobs_perc <- matrix_jobs_perc * 100


barplot(matrix_jobs_perc,
        col = rainbow(3, start = 0.8),
        ylim = c(0,100),
        xlim = c(0,8),
        xlab="Years Testing",
        ylab="Percentage of Group",
        main="Did you have a different job before testing?"
)
legend(6.5,100,
       legend = rownames(matrix_jobs_perc), 
       title = "Response",
       fill = rainbow(3, start = 0.8),
       bty = "n")


# Did testers study computing, grouped by experience level

# Only view data for Testers which achieved a qualification
# Remove testers which state highest qualification is 'None' from index
# This is because testers responding 'None' have NA value in column 18

study_data_index <- which(mydata[,17] != "None")
study_data <- mydata[study_data_index,]

# Indexes for experience groups
lessthanone <- which(study_data[,14] == "less than a year")
onetotwo <- which(study_data[,14] == "1 - 2 years")
twotofive <- which(study_data[,14] == "2 - 5 years")
fivetoten <- which(study_data[,14] == "5 - 10 years")
tentotwenty <- which(study_data[,14] == "10 - 20 years")
twentyplus <- which(study_data[,14] == "More than 20 years")

# Apply these indexes to mydata using column for did you study computing
lessthanone_comp <- study_data[lessthanone,18]
onetotwo_comp <- study_data[onetotwo ,18]
twotofive_comp <- study_data[twotofive,18]
fivetoten_comp <- study_data[fivetoten ,18]
tentotwenty_comp <- study_data[tentotwenty,18]
twentyplus_comp <- study_data[twentyplus,18]

# Check levels
str(lessthanone_comp)
str(onetotwo_comp)
str(twotofive_comp)
str(fivetoten_comp)
str(tentotwenty_comp)
str(twentyplus_comp)

# Drop the unused level "" from each group to reduce to factor w/ 2 levels
lessthanone_comp <- droplevels(lessthanone_comp)
onetotwo_comp <- droplevels(onetotwo_comp)
twotofive_comp <- droplevels(twotofive_comp)
fivetoten_comp <- droplevels(fivetoten_comp)
tentotwenty_comp <- droplevels(tentotwenty_comp)
twentyplus_comp <- droplevels(twentyplus_comp)

# Add the No level back to the twentyplus_comp because it was accidently dropped
levels(twentyplus_comp)[2] <- "No"

# Swap the two levels of twentyplus_comp around so they match the other vectors
twentyplus_comp <- relevel(twentyplus_comp, "No", "Yes")

# Store this data in tables
LTO <- table(lessthanone_comp)
OTT <- table(onetotwo_comp)
TTF <- table(twotofive_comp)
FTT <- table(fivetoten_comp)
TTT <- table(tentotwenty_comp)
TP <- table(twentyplus_comp)

# Up to two years is less than 1 plus one to two
UTT <- LTO + OTT

# Make a vector with all these tables combined
study_comp <- c(UTT, TTF, FTT, TTT, TP)

# Get this data into a matrix
matrix_comp <- matrix(study_comp, ncol=5, byrow= FALSE)

# Label the columns and rows of this matrix
rownames(matrix_comp) <- levels(tentotwenty_comp)
colnames(matrix_comp) <- c("< 2", "2 - 5", "5 - 10", "10 - 20", "20+")

# Use prop.table to convert numeric values to percentage values
matrix_comp_perc <- prop.table(matrix_comp, margin = 2)

# Multiple by 100 so the scale on the plot shows 0 - 100% instead of 0 - 1 %
matrix_comp_perc <- matrix_comp_perc * 100


barplot(matrix_comp_perc,
        col = rainbow(8, start = 0.05),
        ylim = c(0,100),
        xlim = c(0,8),
        xlab="Years Testing",
        ylab="Percentage of Group",
        main="Did you study computing?"
)
legend(6.5,100,
       legend = rownames(matrix_comp_perc), 
       title = "Response",
       fill = rainbow(8, start = 0.05),
       bty = "n")

# Software Tester qualifications

# Make Indexes 
lessthanone <- which(mydata[,14] == "less than a year")
onetotwo <- which(mydata[,14] == "1 - 2 years")
twotofive <- which(mydata[,14] == "2 - 5 years")
fivetoten <- which(mydata[,14] == "5 - 10 years")
tentotwenty <- which(mydata[,14] == "10 - 20 years")
twentyplus <- which(mydata[,14] == "More than 20 years")

# Make indexes to mydata using split by experience group, grepping to get indexes for nongrads in each group
LTOnongrad <- grep("None|GCSE|A-Level|Foundation", mydata[lessthanone,17])
OTTnongrad <- grep("None|GCSE|A-Level|Foundation", mydata[onetotwo,17])
TTFnongrad <- grep("None|GCSE|A-Level|Foundation", mydata[twotofive,17])
FTTnongrad <- grep("None|GCSE|A-Level|Foundation", mydata[fivetoten,17])
TTTnongrad <- grep("None|GCSE|A-Level|Foundation", mydata[tentotwenty,17])
TPnongrad <- grep("None|GCSE|A-Level|Foundation", mydata[twentyplus,17])

# Make indexes to mydata using split by experience group, grepping to get indexes for grads in each group
LTOgrad <- grep("Bachelors degree|Masters degree|Doctorate", mydata[lessthanone,17])
OTTgrad <- grep("Bachelors degree|Masters degree|Doctorate", mydata[onetotwo,17])
TTFgrad <- grep("Bachelors degree|Masters degree|Doctorate", mydata[twotofive,17])
FTTgrad <- grep("Bachelors degree|Masters degree|Doctorate", mydata[fivetoten,17])
TTTgrad <- grep("Bachelors degree|Masters degree|Doctorate", mydata[tentotwenty,17])
TPgrad <- grep("Bachelors degree|Masters degree|Doctorate", mydata[twentyplus,17])

# Total the numbers of non-grad and grad and store them in a vector for each group
LTO <- c(length(LTOnongrad), length(LTOgrad))
OTT <- c(length(OTTnongrad), length(OTTgrad))
TTF <- c(length(TTFnongrad), length(TTFgrad))
FTT <- c(length(FTTnongrad), length(FTTgrad))
TTT <- c(length(TTTnongrad), length(TTTgrad))
TP <- c(length(TPnongrad), length(TPgrad))

# Up to two years is less than 1 plus one to two
UTT <- LTO + OTT

# Combine each of these vectors together
edu_exp <- c(UTT, TTF, FTT, TTT, TP)

# Convert to matrix
matrix_edu_exp <- matrix(edu_exp, ncol = 2, byrow = TRUE)


# Name rows and columns
colnames(matrix_edu_exp) <- c("Non-grad", "Grad")
rownames(matrix_edu_exp) <- c("< 2", "2 - 5", "5 - 10", "10 - 20", "20+")

matrix_edu_exp 

# Looks like this matrix is the wrong way around the columns need to be rows and rows need to be columns 
# So transpose it with t() 
matrix_edu_exp <- t(matrix_edu_exp)

barplot(matrix_edu_exp,
        col = rainbow(2, start = 0.78, end = 0.5),
        ylim = c(0,100),
        xlim = c(0,8),
        xlab="Years Testing",
        ylab="Number of testers",
        main="Graduates and Non-Graduates grouped by Experience"
)
legend(6.5,100,
       legend = rownames(matrix_edu_exp), 
       title = "Response",
       fill = rainbow(2, start = 0.78, end = 0.5),
       bty = "n")

# Plot education levels grouped by experience
None <-which(mydata[,17] == "None")
GCSE <- which(mydata[,17] == "GCSEs or equivalent")
AL <- which(mydata[,17] == "A-Levels or equivalent")
FC<- which(mydata[,17] == "Foundation course")
BD <- which(mydata[,17] == "Bachelors degree")
MD <- which(mydata[,17] == "Masters degree")
PHD <- which(mydata[,17] == "Doctorate")

edu <- table(mydata[,17])

N_sum <- length(None)
G_sum <- length(GCSE)
A_sum <- length(AL)
F_sum <- length(FC)
B_sum <- length(BD)
M_sum <- length(MD)
P_sum <- length(PHD)

total <- N_sum+G_sum+A_sum+F_sum+B_sum+M_sum+P_sum

plotcolnames <- c("None", "GCSE", "A-Level", "Foun.", "Bach.", "Masters", "PhD") 

barplot(c(N_sum, G_sum, A_sum, F_sum, B_sum, M_sum, P_sum), 
        space = NULL, 
        names.arg = plotcolnames,
        col = rainbow(10), 
        ylim = c(0,90),
        xlab="Highest Qualification", 
        ylab="Frequency",
        main="Education levels"
)
axis(2,at=seq(0,90,10))

phd_perc <- round((P_sum / total)*100, digits =2)
phd_perc

bd_perc <-  round((B_sum / total)*100, digits =2)
bd_perc

none_perc <- round((N_sum / total)*100, digits =2)
none_perc

no_degree <- (N_sum + G_sum + A_sum + F_sum)
no_degree
degree <- (B_sum + M_sum + P_sum)
degree

# Make a vector containing total numbers with and without degree
has_degree <- c(no_degree, degree) 

# Generate labels for pie chart containing % of degree and no degree
pielabels <- c("Without Degree", "With Degree")
percent <- round(has_degree/sum(has_degree)* 100, digits = 1)
pielabels <- paste(pielabels, percent)    
pielabels <- paste(pielabels, "%", sep="")

# Plot a pie chart of graduate and non-graduate
pie(has_degree, 
    labels = pielabels, 
    col = rainbow(3, start =0.3, end =0.6),
    radius = 1.5,
    main = "Are testers graduates?")

# Tester training - need some kind of snapshot of training courses

# Make an indexes of testers which say they have attended each training course

train20 <- which(mydata[,20] != "0")
train21 <- which(mydata[,21] != "0")
train22 <- which(mydata[,22] != "0")
train23 <- which(mydata[,23] != "0")
train24 <- which(mydata[,24] != "0")
train25 <- which(mydata[,25] != "0")
train26 <- which(mydata[,26] != "0")

# Group all the row numbers of testers which have been on a training course into a single vector
beenontraining <- c(train20, train21, train22, train23, train24, train25, train26)

# Make a vector to represent rows 1 - 186
all <- c(1:186) 

# Testers which have not done any training are all the rows which are not in beenontraining
notraining <- all [! all %in% beenontraining]

#test the notraining index to make sure all the training columns 20:26 contain 0
mydata[notraining, 20:26]

total_no_training <- length(notraining)
total_tester <- length(mydata[,20])

# Make a vector containing total numbers with and without training
train_vs_not <- c(total_no_training, total_tester - total_no_training)
 
# Generate labels for pie chart containing % with and without training
pielabels <- c("No training", "Attended training")
percent <- round(train_vs_not/sum(train_vs_not)* 100, digits = 1)
pielabels <- paste(pielabels, percent)    
pielabels <- paste(pielabels, "%", sep="")

# Plot a pie chart 
pie(train_vs_not, 
    labels = pielabels, 
    col = rainbow(3, start =0.75, end = 1),
    radius = 1.5,
    main = "Are testers attending training courses?")

# Are training courses a replacement for formal education?
# Make a stacked bar plot of education grouped by training and no training

#index of testers with no training
notraining

# make an index of testers with training which is all the rows that are not in no training
all <- c(1:186) 
training <- all [! all %in% notraining]

# apply the training and no training index to the column containing education level
notraining_edu  <- mydata[notraining,17]
training_edu <- mydata[training,17]

#check structure
str(notraining_edu)
str(training_edu)

# Drop the unused level
notraining_edu <- droplevels(notraining_edu)
training_edu <- droplevels(training_edu)

#Levels keep being sorted alphabetically which is no good for plots
levels(notraining_edu)  # shows the levels are "A-Levels or equivalent", "Bachelors degree", "Doctorate", "Foundation course", "GCSEs or equivalent", "Masters degree", "None"  

# To reorder the levels use factor() and specify a new order
notraining_edu <- factor(notraining_edu,levels(notraining_edu)[c(7,5,1,4,2,6,3)])
training_edu <- factor(training_edu, levels(training_edu)[c(7,5,1,4,2,6,3)])

#check levels now in right order
str(notraining_edu)
str(training_edu)

# store this data in tables
NTRAIN <- table(notraining_edu)
TRAIN <- table(training_edu)

#make a vector with all these tables combined
train_edu <- c(NTRAIN,TRAIN)

#convert this vector into a matrix
matrix_train_edu <- matrix(train_edu, ncol=7, byrow= TRUE)

#name rows and columns
rownames(matrix_train_edu) <- c("No Training", "Training")
colnames(matrix_train_edu) <- c("None", "GCSE", "A-Level", "Foun.", "Bach.", "Masters", "PhD")

# Use prop.table to convert numeric values to percentage values
matrix_train_edu_perc <- prop.table(matrix_train_edu, margin = 2)

# Multiple by 100 so the scale on the plot shows 0 - 100% instead of 0 - 1 %
matrix_train_edu_perc <- matrix_train_edu_perc * 100

barplot(matrix_train_edu,
        col = rainbow(2, start = 0.1, end = 0.6),
        ylim = c(0,100),
        xlim = c(0,8),
        xlab="Education Level",
        ylab="Number of Testers",
        main="Training by education level"
)
legend(6.4,100,
       legend = rownames(matrix_train_edu), 
       title = "Response",
       fill = rainbow(2, start = 0.1, end = 0.6),
       bty = "n")

matrix_train_edu

# Get data for each training course.
RST <- mydata[,20]
AST_F <- mydata[,21]
AST_B <- mydata[,22]
AST_T <- mydata[,23]
ISEB_F <- mydata[,24]
ISEB_A <- mydata[,25]
ISEB_E <- mydata[,26]

# Sum up the points
sumRST <- sum(RST)
sumAST_F <- sum(AST_F)
sumAST_B <- sum(AST_B)
sumAST_T <- sum(AST_T)
sumISEB_F <- sum(ISEB_F)
sumISEB_A <- sum(ISEB_A)
sumISEB_E <- sum(ISEB_E)

# Make a vector of training courses
training <- c(sumRST, sumISEB_F, sumAST_F, sumAST_B, sumISEB_A, sumAST_T, sumISEB_E)

#Add names to the vector
names(training) <- c("RST", "ISEB Foun.", "AST Foun.", "AST Bug.", "ISEB Adv.", "AST Test.", "ISEB Exp.")

# Change axis orientation
par(las = 2)

# Margins, bottom, left, top, right (default is  c(5.1, 4.1, 4.1, 2.1))
par(mar=c(6,4.1,4.1,2.1))

barplot(training,
        col = rainbow(7, start = 0.1, end = 0.6),
        ylim = c(0,250),
        xlim = c(0,8),
        ylab="Course rating",
        main="Training Courses by Rating")

# Make an index which excludes people that answered None to highest qualification 
# Because these people were not asked if they knew they wanted to be a tester while studying 
educated <- which(mydata[,17] != "None")

# Apply educated index to my data
edudf <- mydata[educated,]

# Make indexes for yes and no responses to did you know you wanted to be a tester question
yes <- which(edudf[,19] == "Yes")
no <- which(edudf[,19] == "No")

wanttotest <- c(length(yes), length(no))


# Generate labels for pie chart containing % with and without training
pielabels <- c("Yes", "No")
percent <- round(wanttotest/sum(wanttotest)* 100, digits = 1)
pielabels <- paste(pielabels, percent)    
pielabels <- paste(pielabels, "%", sep="")

pie(wanttotest, 
    labels = pielabels, 
    col = rainbow(3, start =0.52, end= 0.4),
    radius = 1.5,
    main = "While studying did you know you wanted to work in testing?")

# See if there is a trend for testing over the years of experience

# Make indexes for experience
lessthanone <- which(edudf[,14] == "less than a year")
onetotwo <- which(edudf[,14] == "1 - 2 years")
twotofive <- which(edudf[,14] == "2 - 5 years")
fivetoten <- which(edudf[,14] == "5 - 10 years")
tentotwenty <- which(edudf[,14] == "10 - 20 years")
twentyplus <- which(edudf[,14] == "More than 20 years")

# Apply indexes to edudf using column for if wanted to be a tester while in education 
lessthanone_t <- edudf[lessthanone,19]
onetotwo_t <- edudf[onetotwo ,19]
twotofive_t <- edudf[twotofive,19]
fivetoten_t <- edudf[fivetoten ,19]
tentotwenty_t <- edudf[tentotwenty,19]
twentyplus_t <- edudf[twentyplus,19]

# Drop levels to remove unused vectors
lessthanone_t <- droplevels(lessthanone_t)
onetotwo_t <- droplevels(onetotwo_t)
twotofive_t <- droplevels(twotofive_t)
fivetoten_t <- droplevels(fivetoten_t)
tentotwenty_t <- droplevels(tentotwenty_t)
twentyplus_t <- droplevels(twentyplus_t)

# Check vector to see number of levels
str(lessthanone_t)
str(onetotwo_t)
str(twotofive_t)
str(fivetoten_t)
str(tentotwenty_t)
str(twentyplus_t)

# Some valid levels got accidently dropped as they were zero value, so add them back in
levels(lessthanone_t)[2] <- "Yes"
levels(onetotwo_t)[2] <- "Yes"
levels(twentyplus_t)[2] <- "Yes"

# store this data in tables
LTO <- table(lessthanone_t)
OTT <- table(onetotwo_t)
TTF <- table(twotofive_t)
FTT <- table(fivetoten_t)
TTT <- table(tentotwenty_t)
TP <- table(twentyplus_t)

#up to two years is less than 1 plus one to two
UTT <- LTO + OTT

#make a vector with all these tables combined
test <- c(UTT, TTF, FTT, TTT, TP)

#convert this vector into a matrix
matrix_t <- matrix(test, ncol=5, byrow= FALSE)

#Label the columns and rows of this matrix
rownames(matrix_t) <- levels(lessthanone_t)
colnames(matrix_t) <- c("< 2", "2 - 5", "5 - 10", "10 - 20", "20+")

# Change axis orientation
par(las = 1)

#plot
barplot(matrix_t,
        col = rainbow(3, start= 0.4, end = 1),
        ylim = c(0,100),
        xlim = c(0,8),
        xlab="Years Testing",
        ylab="Number of Testers",
        main="While studying did you know you wanted to work in testing?"
)
legend(6.5,100,
       legend = rownames(matrix_t), 
       title = "Response",
       fill = rainbow(3, start= 0.4, end = 1),
       bty = "n")

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

# Set pars
# Change axis orientation
par(las = 2)

# Margins, bottom, left, top, right (default is  c(5.1, 4.1, 4.1, 2.1))
par(mar=c(19,4.1,1,2.1))

# Plot results
barplot(sorted_reasons,
        ylim = c(0,100),
        xlim = c(0,25),
        ylab="Responses",
        col = heat.colors(21)
)