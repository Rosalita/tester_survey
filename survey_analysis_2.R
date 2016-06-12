# Survey response analysis
# 2016 Rosie Hamilton 
# testingfuntime.blogspot.co.uk

# Set working dir
# setwd ("/Dev/Git/tester_survey")
setwd("/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv", 
                   header = TRUE, sep =",")

# Load treemap package
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
ind_tab <- table(clean_industrys)

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
png(filename = "treeplot.png", width = 600, height = 400, units = "px")
treemap(ind_data,
        index="combined",
        title="Number of Testers in Industry",
        title.legend = "",
        vSize="Freq",
        vColor = "Freq",
        type = "dens",
        aspRatio = NA,
        inflate.labels = FALSE,
        
        palette=rainbow(47, s = 1, start = 0, end = 0.8)
        )
dev.off()