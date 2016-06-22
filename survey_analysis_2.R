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


###################################################

# Section 2 - Tester Experience?

###################################################


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
# sapply function to industry to get index
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

# Make new col
combined <- paste0(ind_data[,1], "\n", round(ind_data[,3] * 100, digits = 1), "%"  )

# Add to data frame
ind_data <- cbind(ind_data, combined)

# Display all the RColourBrewer palettes
library(RColorBrewer)
display.brewer.all()

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



#are testers skills more likely to be valued the longer they have worked in testing

# Make indexes
lessthanone <- which(mydata[,14] == "less than a year")
onetotwo <- which(mydata[,14] == "1 - 2 years")
twotofive <- which(mydata[,14] == "2 - 5 years")
fivetoten <- which(mydata[,14] == "5 - 10 years")
tentotwenty <- which(mydata[,14] == "10 - 20 years")
twentyplus <- which(mydata[,14] == "More than 20 years")


# Apply indexes to mydata using column for are technical skills valued 
lessthanone_skills <- mydata[lessthanone,37]
onetotwo_skills <- mydata[onetotwo ,37]
twotofive_skills <- mydata[twotofive,37]
fivetoten_skills <- mydata[fivetoten ,37]
tentotwenty_skills <- mydata[tentotwenty,37]
twentyplus_skills <- mydata[twentyplus,37]

#All the vectors we just made have three factors so drop levels to make them have 2 factors
str(lessthanone_skills)

#so drop levels so that they are just two factors
lessthanone_skills <- droplevels(lessthanone_skills)
onetotwo_skills <- droplevels(onetotwo_skills)
twotofive_skills <- droplevels(twotofive_skills)
fivetoten_skills <- droplevels(fivetoten_skills)
tentotwenty_skills <- droplevels(tentotwenty_skills)
twentyplus_skills <- droplevels(twentyplus_skills)

# store this data in tables
LTO <- table(lessthanone_skills)
OTT <- table(onetotwo_skills)
TTF <- table(twotofive_skills)
FTT <- table(fivetoten_skills)
TTT <- table(tentotwenty_skills)
TP <- table(twentyplus_skills)

#up to two years is less than 1 plus one to two
UTT <- LTO + OTT


#make a vector with all these tables combined

valued <- c(UTT, TTF, FTT, TTT, TP)

#convert this vector into a matrix

matrix_valued = matrix(valued, ncol=5, byrow= FALSE)

levels(twentyplus_skills)

#Label the columns and rows of this matrix
rownames(matrix_valued)= levels(twentyplus_skills) # could use any of the skill vars as they all have same levels
colnames(matrix_valued)= c("> 2", "2 - 5", "5 - 10", "10 - 20", "20+")

# Use prop.table to convert numeric values to percentage values
matrix_valued_perc <- prop.table(matrix_valued, margin = 2)

# Multiple by 100 so the scale on the plot shows 0 - 100% instead of 0 - 1 %
matrix_valued_perc <- matrix_valued_perc * 100

barplot(matrix_valued_perc,
        col = rainbow(2, start = 0.6),
        ylim = c(0,100),
        xlim = c(0,8),
        xlab="Years Testing",
        ylab="Percentage of Group",
        main="Technical skills felt valued, grouped by experience"
        )
legend(6.5,100,
       legend = rownames(matrix_valued_perc), 
       title = "Response",
       fill = rainbow(2, start = 0.6),
       bty = "n")

#try apply the stacked % bar plot by group to the following columns:
# different job before becoming a tester - col 16
# did you study computing - col 18
# Do you think other testing jobs are better than yours - col 28
# can take on more responsibility col 41
# My judgement is trusted - col 44
# unpaid overtime - col 46
# devs superior - col 47
# Company support to attend training courses - col 49



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

# store this data in tables
LTO <- table(lessthanone_job)
OTT <- table(onetotwo_job)
TTF <- table(twotofive_job)
FTT <- table(fivetoten_job)
TTT <- table(tentotwenty_job)
TP <- table(twentyplus_job)

#up to two years is less than 1 plus one to two
UTT <- LTO + OTT


#make a vector with all these tables combined
diff_job_before_tester <- c(UTT, TTF, FTT, TTT, TP)

# Populate a matrix with this data

matrix_jobs = matrix(diff_job_before_tester, ncol=5, byrow= FALSE)

levels(twentyplus_job)

#Label the columns and rows of this matrix
rownames(matrix_jobs)= c("No", "Yes")
colnames(matrix_jobs)= c("> 2", "2 - 5", "5 - 10", "10 - 20", "20+")

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






#tester qualifications

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

plotcolnames <- c("None", "GCSE", "A-Level", "Foun.", "B.D", "M.D", "PHD") 


#I would really like to add % labels to the top of each column

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
bd_perc <-  round((B_sum / total)*100, digits =2)



no_degree <- (N_sum + G_sum + A_sum + F_sum)
degree <- (B_sum + M_sum + P_sum)

# Make a vector containing total numbers with and without degree
has_degree <- c(no_degree, degree) 
# Generate labels for pie chart containing % of Happy and Not Happy
pielabels <- c("Without Degree", "With Degree")
percent <- round(has_degree/sum(has_degree)* 100, digits = 1)
pielabels <- paste(pielabels, percent)    
pielabels <- paste(pielabels, "%", sep="")


# Plot a pie chart 
pie(has_degree, 
    labels = pielabels, 
    col = rainbow(7, start =0.35),
    main = "Are testers graduates?")





# Duration of testing career vs level of education 

# Make an index of non-graduates by grepping for None, GCSE, A-Level or foundation course

nongrad <- grep("None|GCSE|A-Level|Foundation", mydata[,17])

# Invert the non-graduate index to get the graduate index

all <- c(1:186)
grads <- all [! all %in% nongrad]

# Test the indexes to make sure they are correct
mydata[nongrad,17]
mydata[grads,17]

# Apply the nongrad index to the years experience column
nongradexp <- mydata[nongrad,14]

# Apply the grad index to the years experience column
gradexp <- mydata[grads,14]

# nongradexp and gradexp are factor w/ 7 levels, one of which is ""
str(nongradexp)
str(gradexp)

# Drop the unused "" levels to make them factors w/ 6 levels
nongradexp <- droplevels(nongradexp)
gradexp <- droplevels(gradexp)


# Check levels
levels(nongradexp)
levels(gradexp) # Gradexp is missing "More than 20 years" because it was accidently removed by droplevels

# Manually force the "More than 20 years" level back in
levels(gradexp)[6] <- "More than 20 years"

# Check graduate exp now has "More than 20 years" again
levels(gradexp)

# Reorder levels from shortest to longest
nongradexp <- relevel(nongradexp, "less than a year", "1 - 2 years", "2 - 5 years", "5 - 10 years", "10 - 20 years", "More than 20 years")
gradexp <- relevel(gradexp, "less than a year", "1 - 2 years", "2 - 5 years", "5 - 10 years", "10 - 20 years", "More than 20 years")


factor(nongradexp, levels = levels(nongradexp)[5,1,3,4,2,6])
?relevel

# Generate tables of experience for non grads and grads
nongradtab <- table(nongradexp)
gradtab <- table(gradexp)


# Group both tables together
groupdata <- c(nongradtab, gradtab)


# Get this data into a matrix
matrix_edu_exp = matrix(groupdata, ncol=6, byrow= TRUE)

#Label the columns and rows of this matrix
colnames(matrix_edu_exp)= levels(nongradexp) # could use either nongradexp or gradexp as they have the same levels
rownames(matrix_edu_exp)= c("Non-Graduates", "Graduates")

matrix_edu_exp
 
# use prop.table on it to convert numeric values to percentages

# Note to self:
# Leaving margin blank gives proportions of the whole table
#
# prop.table(x, margin=NULL)
#      [,1] [,2]
# [1,]  0.1  0.3
# [2,]  0.2  0.4
#
# Giving it 1 gives row proportions
#
# prop.table(x, 1)
#     [,1]      [,2]
# [1,] 0.2500000 0.7500000
# [2,] 0.3333333 0.6666667
#
# Giving it 2 gives column proportions
#
# prop.table(x, 2)
#     [,1]      [,2]
# [1,] 0.3333333 0.4285714
# [2,] 0.6666667 0.5714286

# For this matrix, the groups are rows nongrad and graduate, so need to use margin 1

matrix_edu_exp_perc <- prop.table(matrix_edu_exp, margin =1)

# Set some column names
plotcolnames <- c("> 1", "1 - 2", "10 - 20", "2 - 5", "5 - 10", "20+")

barplot(matrix_edu_exp_perc,
        width = 0.5, 
        space = NULL,
        names.arg = plotcolnames,
        col = rainbow(5, start = 0.15), 
        ylim = c(0,1),
       # xlim = c(0,5),
        xlab="Years Testing", 
        ylab="Percentage of Group",
        main="Experience levels of testers by education group"
      # legend.text = rownames(matrix_edu_exp),
        )
legend(2.5,1,
       legend = rownames(matrix_edu_exp), 
       title = "Groups",
       fill = rainbow(5, start = 0.15),
       bty = "n")


matrix_edu_exp_perc


#tester training - need some kind of snapshot of training courses



