# Capstone Project
# File: makeFreqs.R
#   Computes the frequencies from the document text matrices
#   Loads document text matrices from specific directories.

print("Started script: makeFreqs.R")
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

###### Select document text matrix directory. THIS IS REQUIRED
dtms.dir <- c("../75.dir")
###### Select name for sample. THIS IS REQUIRED
sample.name <- "seventyfive.pct"
###### SET SAVE FILE #####  THIS IS REQUIRED
save.file <- "freqs.dense.r"

print(paste("Going to directory containing Document Text Matrix: ",dtms.dir))
setwd(dtms.dir)
print(paste("Current directory: ",getwd()))

###### Select document text matrix to load. THIS IS REQUIRED
dtms.file <- c("dtmsDense.r")

# Loading document text matrices
load(dtms.file)
print("Loaded document text matrices")

##### Set value of dtms ### REQUIRED
dtms <- dtms.dense

# Computing frequencies

# dtms is a list with one element per sample set. Each element has
# DTM for unigram ... quadgram (or higher depending on max.ngram)
print("Computing frequencies and sorting in decreasing order...")
freqs.db <- lapply(dtms,dtm2freq) # THIS RETURNS SORTED HIGHER FREQ FIRST

names(freqs.db) <- sample.name

print("Saving frequencies, etc...")
if(file.exists(save.file)){
  file.remove(save.file)
}

###### SET NAMES TO SAVE  ## REQUIRED
freqs.dense.db    <- freqs.db

save(freqs.dense.db,file=save.file)
print("Finished saving freqs.dense.db stuff.")

print("Completed makeFreqs.R")

print("Resetting to project directory.")
setwd(prj.dir)

