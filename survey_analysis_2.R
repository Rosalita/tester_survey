# Survey response analysis
# 2016 Rosie Hamilton 
# testingfuntime.blogspot.co.uk

# Set working dir
# setwd ("/Dev/Git/tester_survey")
setwd("/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv", 
                   header = TRUE, sep =",")

###################################################

# Section 2 - 

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
clean_industry <- scan(file = "industry.txt", what = character(), sep = "\n")





