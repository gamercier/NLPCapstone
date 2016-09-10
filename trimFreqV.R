# Capstone Project
# File: trimFreqV.R
#   Trims the Frequency vector by dropping elements thate occur less than a user
#   defined threshold

print("Started script: trimFreqV.R")
# Got to project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

# load resources
source("nlpTools.R")
print("Loaded tools.")

#Switch to processed corpus directory
source("toProcCorpusDir.R")
print(paste("Switched to diretory",getwd()))

# Loading document text matrices
load("freqs.r")
freqs <- ls(pattern="freqs")
print(paste("Loaded frequency vectors: ", paste(freqs)))

trimFreqsV <- function(freqsV,cut.levels=7){
  pull <- function(freq) freq[freq > cut.level]
  freqsV.trimmed <- lapply(freqsV,pull)
  names(freqsV.trimmed) <- names(freqsV)
  return(freqsV.trimmed)
}

