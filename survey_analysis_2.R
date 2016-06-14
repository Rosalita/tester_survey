# Survey response analysis
# 2016 Rosie Hamilton
# testingfuntime.blogspot.co.uk

# Set working dir
#setwd ("/Dev/Git/tester_survey")
setwd("/git/tester_survey")

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
?treemap
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

# Make an index that includes row numbers of single industry only testers
justone <- c(4,6,11,17,21,28,29,30,31,43,44,45,52,60,
             70,71,77,78,83,88,94,103,108,110,115,118,
             128,136,138,144,145,162,168,172,173,175)

# Invert this index to get an index for testers which have tested in multiple industries
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


LTO <- table(lessthanone_skills)
LTO_perc <- c(LTO[2] /(LTO[3]+ LTO[2]),LTO[3] /(LTO[3]+ LTO[2]))

OTT <- table(onetotwo_skills)
OTT_perc <- c(OTT[2] /(OTT[3]+ OTT[2]),OTT[3] /(OTT[3]+ OTT[2]))

TTF <- table(twotofive_skills)
TTF_perc <- c(TTF[2] /(TTF[3]+ TTF[2]),TTF[3] /(TTF[3]+ TTF[2]))

FTT <- table(fivetoten_skills)
FTT_perc <- c(FTT[2] /(FTT[3]+ FTT[2]),FTT[3] /(FTT[3]+ FTT[2]))

TTT <- table(tentotwenty_skills)
TTT_perc <- c(TTT[2] /(TTT[3]+ TTT[2]),TTT[3] /(TTT[3]+ TTT[2]))

TP <- table(twentyplus_skills)
TP_perc <- c(TP[2] /(TP[3]+ TP[2]),TP[3] /(TP[3]+ TP[2]))

LTO_perc
OTT_perc
TTF_perc
FTT_perc
TTT_perc 
TP_perc

#up to two years is less than 1 plus one to two
UTT <- LTO + OTT
UTT_perc <- c(UTT[2] /(UTT[3]+ UTT[2]),UTT[3] /(UTT[3]+ UTT[2]))


barplot(c(UTT_perc, TTF_perc, FTT_perc, TTT_perc, TP_perc),
        col= rainbow(30))


