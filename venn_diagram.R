
# Set working dir
setwd ("/Dev/Git/tester_survey")
#setwd("~/git/tester_survey")

# Read in data
mydata <- read.csv("survey_results_raw.csv",
                   header = TRUE, sep =",")

# People that currently do not work in testing have been excluded 
Current_testers <- which(mydata[,2] == "Yes")

#Apply this index to the data
mydata <- mydata[Current_testers,]

rst <- mydata[,20]

#replace the 2,3,4,5's with 1
rst <- replace(rst, rst==2, 1)
rst <- replace(rst, rst==3, 1)
rst <- replace(rst, rst==4, 1)
rst <- replace(rst, rst==5, 1)

ast_bbst_foundation <- mydata[,21]

#replace the 2,3,4,5's with 1
ast_bbst_foundation <- replace(ast_bbst_foundation, ast_bbst_foundation==2, 1)
ast_bbst_foundation <- replace(ast_bbst_foundation, ast_bbst_foundation==3, 1)
ast_bbst_foundation <- replace(ast_bbst_foundation, ast_bbst_foundation==4, 1)
ast_bbst_foundation <- replace(ast_bbst_foundation, ast_bbst_foundation==5, 1)

ast_bbst_bug_advocacy <- mydata[,22]

#replace the 2,3,4,5's with 1
ast_bbst_bug_advocacy <- replace(ast_bbst_bug_advocacy, ast_bbst_bug_advocacy==2, 1)
ast_bbst_bug_advocacy <- replace(ast_bbst_bug_advocacy, ast_bbst_bug_advocacy==3, 1)
ast_bbst_bug_advocacy <- replace(ast_bbst_bug_advocacy, ast_bbst_bug_advocacy==4, 1)
ast_bbst_bug_advocacy <- replace(ast_bbst_bug_advocacy, ast_bbst_bug_advocacy==5, 1)

ast_bbst_test_design <- mydata[,23]

#replace the 2,3,4,5's with 1
ast_bbst_test_design <- replace(ast_bbst_test_design, ast_bbst_test_design==2, 1)
ast_bbst_test_design <- replace(ast_bbst_test_design, ast_bbst_test_design==3, 1)
ast_bbst_test_design <- replace(ast_bbst_test_design, ast_bbst_test_design==4, 1)
ast_bbst_test_design <- replace(ast_bbst_test_design, ast_bbst_test_design==5, 1)

iseb_foundation <- mydata[,24]

#replace the 2,3,4,5's with 1
iseb_foundation<- replace(iseb_foundation, iseb_foundation==2, 1)
iseb_foundation<- replace(iseb_foundation, iseb_foundation==3, 1)
iseb_foundation<- replace(iseb_foundation, iseb_foundation==4, 1)
iseb_foundation<- replace(iseb_foundation, iseb_foundation==5, 1)

iseb_advanced <- mydata[,25]

#replace the 2,3,4,5's with 1
iseb_advanced  <- replace(iseb_advanced , iseb_advanced ==2, 1)
iseb_advanced  <- replace(iseb_advanced , iseb_advanced ==3, 1)
iseb_advanced  <- replace(iseb_advanced , iseb_advanced ==4, 1)
iseb_advanced  <- replace(iseb_advanced , iseb_advanced ==5, 1)


iseb_expert <- mydata[,26]

#replace the 2,3,4,5's with 1
iseb_expert  <- replace(iseb_expert , iseb_expert ==2, 1)
iseb_expert  <- replace(iseb_expert , iseb_expert ==3, 1)
iseb_expert  <- replace(iseb_expert , iseb_expert ==4, 1)
iseb_expert  <- replace(iseb_expert , iseb_expert ==5, 1)


training <- cbind(rst,iseb_foundation, iseb_advanced, iseb_expert)

vennDiagram(training,
            names = c("RST", "ISTQB Foundation", "ISTQB Advanced", "ISTQB Expert"),
            circle.col = c("skyblue", "lightgoldenrod1", "mediumorchid", "palegreen")
)
