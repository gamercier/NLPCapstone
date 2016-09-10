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

# Loading document text matrices
load("dtmsDense.r")
dtms.var <- ls(pattern="dtms")
print(paste("Loaded document text matrices: ", paste(dtms.var)))

print(paste("This document text matrix contains:",names(dtms.dense)))

# Computing frequencies

# dtms is a list with one element per sample set. Each element has
# DTM for unigram ... quadgram (or higher depending on max.ngram)
print("Computing frequencies and sorting in decreasing order...")
freqs.dense.db <- lapply(dtms.dense,dtm2freq)

# Generating additional data

#### NOTE ####
# Structure of object freqs.db is a list with one element per sample set
# For each sample set you have a list with frequencies for unigrams, bigrams, trigrams, and
# and quadgrams.
#    SORT - already done
#    COLLECT sorted ngrams
ngrams.dense.db <- lapply(freqs.dense.db, # over samples
                          function(x) lapply(x, # over n-grams of each sample
                                             names))
#    COLLECT the base, or the ngram minus the last word; nonsense for unigrams
bases.dense.db <- lapply(ngrams.dense.db,
                   function(x) { lapply(x[-1], function(x) unlist(dropLastWord(x))) } )

#     GET number of ngrams for each N, as a list, (includes repetitions)
N.ngrams.dense.db <- lapply(freqs.dense.db, function(x) lapply(x,sum))
# 
#     GET number of ngrams for each N, as a list, (no repetitions), i.e. the Vocabulary
V.ngrams.dense.db <- lapply(ngrams.dense.db,function(x) lapply(x,length))

print("Saving frequencies, etc...")
if(file.exists("freqs.dense.r")){
  file.remove("freqs.dense.r")
}
save(freqs.dense.db,ngrams.dense.db,bases.dense.db,N.ngrams.dense.db,
     V.ngrams.dense.db,file="freqs.dense.r")
print("Finished saving freqs.dense.db stuff.")

# Define SCORING FUNCTION. Based on Stupid Back Off
#    score(ngram) = counts(ngram)/counts(ngram with first word dropped) if n >1
#    score(1gram) = counts(1gram)/(total number of 1grams (not unique 1grams))
#For stupid back up implementation:
#http://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification

# NOTE: Computations here assume that for each sample,
#       the freqs.db is ordered as unigram, bigram, trigram, quadgram...
#

print("Computing ngram scores for stupid backoff scheme.")
scores.dense.db <- lapply(freqs.dense.db,score.sbackoff) # computes scores for each sample
print("Done computing scores.")

print("Saving scores database, etc... in *scoresSB.dense.r*")
if(file.exists("scoresSB.dense.r")){
  file.remove("scoresSB.dense.r")
}
save(scores.dense.db, ngrams.dense.db, bases.dense.db, file="scoresSB.dense.r")
print("Finished saving scores database in *scoresSB.dense.r*")

print("Completed makeScoresSB.R")

print("Resetting to project directory.")
setwd(prj.dir)

