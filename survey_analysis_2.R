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


lessthanone <- which(mydata[,14] == "less than a year")
onetotwo <- which(mydata[,14] == "1 - 2 years")
twotofive <- which(mydata[,14] == "2 - 5 years")
fivetoten <- which(mydata[,14] == "5 - 10 years")
tentotwenty <- which(mydata[,14] == "10 - 20 years")
twentyplus <- which(mydata[,14] == "More than 20 years")


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



LTO <- table(lessthanone_skills)
LTO_perc <- round(c(LTO[1] /(LTO[2]+ LTO[1]),LTO[2] /(LTO[2]+ LTO[1])), digits=2)

OTT <- table(onetotwo_skills)
OTT_perc <- round(c(OTT[1] /(OTT[2]+ OTT[1]),OTT[2] /(OTT[2]+ OTT[1])), digits=2)

TTF <- table(twotofive_skills)
TTF_perc <- round(c(TTF[1] /(TTF[2]+ TTF[1]),TTF[2] /(TTF[2]+ TTF[1])), digits=2)

FTT <- table(fivetoten_skills)
FTT_perc <- round(c(FTT[1] /(FTT[2]+ FTT[1]),FTT[2] /(FTT[2]+ FTT[1])), digits=2)

TTT <- table(tentotwenty_skills)
TTT_perc <- round(c(TTT[1] /(TTT[2]+ TTT[1]),TTT[2] /(TTT[2]+ TTT[1])), digits=2)

TP <- table(twentyplus_skills)
TP_perc <- round(c(TP[1] /(TP[2]+ TP[1]),TP[2] /(TP[2]+ TP[1])), digits=2)

LTO_perc
OTT_perc
TTF_perc
FTT_perc
TTT_perc 
TP_perc

#up to two years is less than 1 plus one to two
UTT <- LTO + OTT
UTT_perc <- c(UTT[1] /(UTT[2]+ UTT[1]),UTT[2] /(UTT[2]+ UTT[1]))


#these bar plots look a bit rubbish
barplot(c(UTT_perc, TTF_perc, FTT_perc, TTT_perc, TP_perc), col= rainbow(30))


barplot(c(UTT, TTF, FTT, TTT, TP), col= rainbow(30))

#ideally would like a percentage based stacked vertical barplot
#grouped by duration testing, showing % of true and false reponses


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


#Plot a pie chart 
pie(has_degree, 
    labels = pielabels, 
    col = rainbow(7, start =0.35),
    main = "Are testers graduates?")

#duration of testing career vs level of education - hmm how can I plot this?

#indexs for duration of career
#lessthanone <- which(mydata[,14] == "less than a year")
#onetotwo <- which(mydata[,14] == "1 - 2 years")
#twotofive <- which(mydata[,14] == "2 - 5 years")
#fivetoten <- which(mydata[,14] == "5 - 10 years")
#tentotwenty <- which(mydata[,14] == "10 - 20 years")
#twentyplus <- which(mydata[,14] == "More than 20 years")


#edu_lessthanone <- mydata[lessthanone,14:17]
#edu_onetotwo <- mydata[onetotwo,17]
#edu_twotofive <- mydata[twotofive,17]
#edu_fivetoten <- mydata[fivetoten,17]
#edu_tentotwenty <- mydata[tentotwenty,17]
#edu_twentyplus <- mydata[twentyplus,17]

exp_edu_cols <- c(14, 17)

exp_edu <- mydata[,exp_edu_cols]
 

#tester training - need some kind of snapshot of training courses



