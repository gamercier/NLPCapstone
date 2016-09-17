# Capstone Project
# File: makeScoresSB.R
#   Computes the frequencies and scores for stupid backoff
#   Loads document text matrices

print("Started script: makeScoresSB.R")
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

#Select freqs directory. THIS IS REQUIRED
freqs.dir <- c("../XX.dir")
print(paste("Going to directory containing frequecy data: ",freqs.dir))
setwd(freqs.dir)

#Select frequency file. THIS IS REQUIRED
freqs.file <- "freqs.r"
print("Loading frequency data")
load("freqs.r")

freqs.var <- ls(pattern="freqs")
print(paste("Loaded document text matrices: ", paste(freqs.var)))

# Define SCORING FUNCTION. Based on Stupid Back Off
#    score(ngram) = counts(ngram)/counts(ngram with first word dropped) if n >1
#    score(1gram) = counts(1gram)/(total number of 1grams (not unique 1grams))
#For stupid back up implementation:
#http://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification

# NOTE: Computations here assume that for each sample,
#       the freqs.db is ordered as unigram, bigram, trigram, quadgram...
#

print("Computing ngram scores for stupid backoff scheme.")
scores.db <- lapply(freqs.db,score.sbackoff) # computes scores for each sample
print("Done computing scores.")

print("Saving scores database, etc... in *scoresSB.r*")
if(file.exists("scoresSB.r")){
  file.remove("scoresSB.r")
}
save(scores.db, ngrams.db, bases.db, file="scoresSB.r")
print("Finished saving scores database in *scoresSB.r*")

print("Completed makeScoresSB.R")

print("Resetting to project directory.")
setwd(prj.dir)

