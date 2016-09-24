# Capstone Project
# File: toTesting.R
# Set working directory to the testing directory - has test datasets

prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
download.dir <- "nlpData.dir"
testing.dir <- file.path(prj.dir,download.dir,"testing")
print(paste("Moving to",testing.dir))
setwd(testing.dir)
print(paste("Current directory: ",getwd()))
