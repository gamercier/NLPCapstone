# Capstone Project
# File: makeScoresSB.R
#   Computes the frequencies and scores for stupid backoff
#   Loads document frequency database for computations.

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
print(paste("Switched to directory",getwd()))

#Select freqs directory. THIS IS REQUIRED
freqs.dir <- c("../75.dir")
print(paste("Going to directory containing frequecy data: ",freqs.dir))
setwd(freqs.dir)
print(paste("Switched to directory",getwd()))

###### Select frequency file. THIS IS REQUIRED
freqs.file <- "freqs.trimmed.dense.r"
print(paste("Loading frequency data from file",freqs.file))
load(freqs.file)
print(paste("Loaded frequency data from file",freqs.file))

#### SELECT FILE FOR SAVING -- REQUIRED
save.file <- "scoresSB.trimmed.dense.r"

###### Set frequency database. THIS REQUIRED
freqs.db <- freqs.trimmed.dense.db

###### SEE BELOW TO Select save file for scores database -- THIS IS REQUIRED

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

ngrams.db <- lapply(scores.db,function(x) lapply(x,names))
bases.db  <- lapply( ngrams.db,function(x) { lapply( x[2:length(x)],
                    function(y) unlist(dropLastWord(y)) ) } )

#### SET NAMES FOR SAVING - REQUIRED
scores.trimmed.dense.db <- scores.db
ngrams.trimmed.dense.db <- ngrams.db
bases.trimmed.dense.db  <- bases.db

if(file.exists(save.file)){
  file.remove(save.file)
}
print(paste("Saving scores database in",save.file))
save(scores.trimmed.dense.db,
     ngrams.trimmed.dense.db,
     bases.trimmed.dense.db, file=save.file)
print(paste("Finished saving scores database in",save.file))

print("Completed makeScoresSB.R")

print("Resetting to project directory.")
setwd(prj.dir)


