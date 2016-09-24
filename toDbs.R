# Capstone Project
# File: toDbs.R
# Set working directory to the database directory - has test scores database.

prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
download.dir <- "nlpData.dir"
dbs.dir <- file.path(prj.dir,download.dir,"dbs")
print(paste("Moving to",dbs.dir))
setwd(dbs.dir)
print(paste("Current directory: ",getwd()))
