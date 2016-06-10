# Survey response analysis
# 2016 Rosie Hamilton 
# testingfuntime.blogspot.co.uk

# Set working dir
 setwd ("/Dev/Git/tester_survey")
#setwd("/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv", 
                   header = TRUE, sep =",")

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


table(clean_industry)

#make a vector with the names of all the industries present in clean_industries
industry_names <- c("Aerospace", 
                    "Agriculture", 
                    "Automotive", 
                    "Business",       
                    "Car Hire",
                    "Charity", 
                    "Communications", 
                    "E-commerce", 
                    "Education", 
                    "Electronics",
                    "Energy",
                    "Entertainment", 
                    "Finance", 
                    "Gambling", 
                    "Games", 
                    "Geospatial", 
                    "Government", 
                    "Health", 
                    "Hotels", 
                    "Housing",
                    "HR", 
                    "Insurance", 
                    "Intelligence", 
                    "Legal", 
                    "Logistics", 
                    "Manufacturing", 
                    "Marketing", 
                    "Media", 
                    "Military",  
                    "Parking", 
                    "Pharmaceutical", 
                    "Public Sector", 
                    "Publishing", 
                    "Recycling", 
                    "Retail",
                    "Sales", 
                    "Scientific", 
                    "Security", 
                    "Social Media", 
                    "Software Tools", 
                    "Sports", 
                    "Television", 
                    "Tolling", 
                    "Tourism", 
                    "Transport", 
                    "Travel", 
                    "Web Services"
                  )


#make an index for each of the industry names against the cleaned up vector of industries
Aerospace       <- which(clean_industry =="Aerospace")
Agriculture     <- which(clean_industry =="Agriculture")
Automotive      <- which(clean_industry =="Automotive")
Business        <- which(clean_industry =="Business")
CarHire         <- which(clean_industry =="Car Hire")
Charity         <- which(clean_industry =="Charity")
Communications  <- which(clean_industry =="Communications")
Ecommerce      <- which(clean_industry =="E-commerce")
Education       <- which(clean_industry =="Education")
Electronics     <- which(clean_industry =="Electronics")
Energy          <- which(clean_industry =="Energy")
Entertainment   <- which(clean_industry =="Entertainment")
Finance         <- which(clean_industry =="Finance")
Gambling        <- which(clean_industry =="Gambling")
Games           <- which(clean_industry =="Games")
Geospatial      <- which(clean_industry =="Geospatial")
Government      <- which(clean_industry =="Government")
Health          <- which(clean_industry =="Health")
Hotels          <- which(clean_industry =="Hotels")
Housing         <- which(clean_industry =="Housing")
HR              <- which(clean_industry =="HR")
Insurance       <- which(clean_industry =="Insurance")
Intelligence    <- which(clean_industry =="Intelligence")
Legal           <- which(clean_industry =="Legal")
Logistics       <- which(clean_industry =="Logistics")
Manufacturing   <- which(clean_industry =="Manufacturing")
Marketing       <- which(clean_industry =="Marketing")
Media           <- which(clean_industry =="Media")
Military        <- which(clean_industry =="Military")
Parking         <- which(clean_industry =="Parking")
Pharmaceutical  <- which(clean_industry =="Pharmaceutical")
PublicSector    <- which(clean_industry =="Public Sector")
Publishing      <- which(clean_industry =="Publishing")
Recycling       <- which(clean_industry =="Recycling")
Retail          <- which(clean_industry =="Retail")
Sales           <- which(clean_industry =="Sales")
Scientific      <- which(clean_industry =="Scientific")
Security        <- which(clean_industry =="Security")
SocialMedia     <- which(clean_industry =="Social Media")
SoftwareTools   <- which(clean_industry =="Software Tools")
Sports          <- which(clean_industry =="Sports")
Television      <- which(clean_industry =="Television")
Tolling         <- which(clean_industry =="Tolling")
Tourism         <- which(clean_industry =="Tourism")
Transport       <- which(clean_industry =="Transport")
Travel          <- which(clean_industry =="Travel")
WebServices     <- which(clean_industry =="Web Services")


#use the indexes to create a vector of frequency for each industry
freq <- c(length(Aerospace), 
          length(Agriculture),
          length(Automotive), 
          length(Business),       
          length(CarHire),
          length(Charity), 
          length(Communications), 
          length(Ecommerce), 
          length(Education), 
          length(Electronics),
          length(Energy),
          length(Entertainment), 
          length(Finance), 
          length(Gambling), 
          length(Games), 
          length(Geospatial), 
          length(Government), 
          length(Health), 
          length(Hotels), 
          length(Housing),
          length(HR), 
          length(Insurance), 
          length(Intelligence), 
          length(Legal), 
          length(Logistics), 
          length(Manufacturing), 
          length(Marketing), 
          length(Media), 
          length(Military),  
          length(Parking), 
          length(Pharmaceutical), 
          length(PublicSector), 
          length(Publishing), 
          length(Recycling), 
          length(Retail),
          length(Sales), 
          length(Scientific), 
          length(Security), 
          length(SocialMedia), 
          length(SoftwareTools), 
          length(Sports), 
          length(Television), 
          length(Tolling), 
          length(Tourism), 
          length(Transport), 
          length(Travel), 
          length(WebServices)
          )

# Assemble a new data frame which lists each industry and frequency

ind_data <- data.frame(industry_names,freq)


# Find total number of tester 
total <- length(testers)

# Use total number of tester to calculate the relative frequency
relfreq <- round(ind_data[,2]/total, digits =4)

#bind the relative frequency on to the industry data frame
ind_data <- cbind(ind_data, relfreq)

#sort the dataframe by relative frequency, largest first
ind_data <- ind_data[order(ind_data$relfreq, decreasing = TRUE) ,] 

#todo find mean number of industries tested in
