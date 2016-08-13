# Captone Project
# File: getRawCorpus.R
# Auxilliary script to download the raw data (raw corpus), uncompress it,
# and put it in directory with name: download.dir <- "nlpData.dir"
# Project Directory: $HOME/git/NLPCapstone

print("Started script: getRawData.R")

# Go to project directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory:",getwd()))

# Load data
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

tempName <- tempfile(tmpdir=getwd())   # get temporary file name
failed <- download.file(url,tempName,method="curl")
if (failed){
  stop("Error downloading the file!")
}

rawCorpus.downLoadDate <- date()
print(paste("Downloaded data on ",rawCorpus.downLoadDate))

download.dir <- "nlpData.dir"
if (dir.exists(download.dir)){
  unlink(download.dir,recursive=TRUE)   # deletes directory if present
}
unzip(tempName,exdir=download.dir)      # unzip into directory - nlpData.dir
unlink(tempName)                    # delete the unzipped file

files <- dir(download.dir)

print(paste("Raw corpus downloaded into directory: ",download.dir))
print(paste("Raw corpus directory contains: ",files))

print("Returning to main directory!")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))
print("Script, getRawCorpus.R, successfully finished!")

