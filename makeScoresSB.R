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
source("dbdtms.r")
corps <- ls(pattern="dtms")
print(paste("Loaded document text matrices: ", cat(corps)))

# Computing frequencies

# dtms is a list with one element per sample set. Each element has
# DTM for unigram ... quadgram (or higher depending on max.ngram)
print("Computing frequencies and sorting in decreasing order...")
freq.db <- lapply(dtms,dtm2freq)

# Generating additional data

#### NOTE ####
# Structure of object freq.db is a list that contains sublists, one per n-gram.
# Each sublist contains a subsublist with only one member, a named vector that contains
# the frequencies. The names are the n-grams.

#    SORT - already done
#    COLLECT sorted ngrams
ngrams.db <- lapply(freq.db,function(x) lapply(x,names))

# #    COLLECT the base, or the ngram minus the last word; nonsense for unigrams
# bases.db <- lapply(ngrams.db,
#                    function(x) { lapply(x[-1], function(x) unlist(dropLastWord(x))) } )
# 
# #    GET number of ngrams for each N, as a list, (includes repetitions)
# N.ngrams <- lapply(freq.db, function(x) lapply(x,sum))
# 
# #    GET number of ngrams for each N, as a list, (no repetitions)
# unique.N.ngrams <- lapply(ngrams.db,function(x) lapply(x,length))

print("Saving frequencies, etc...")
if(file.exists("dbfreq.r")){
  file.remove("dbfreq.r")
}
save(freq.db,ngrams.db,file="dbfreq.r")
print("Finished saving freq.db stuff.")

# Define SCORING FUNCTION. Based on Stupid Back Off
#    score(ngram) = counts(ngram)/counts(ngram with first word dropped) if n >1
#    score(1gram) = counts(1gram)/(total number of 1grams (not unique 1grams))
#For stupid back up implementation:
#http://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification

# NOTE: Computations here assume that for each sample,
#       the freq.db is ordered as unigram, bigram, trigram, quadgram...
#

print("Computing ngram scores for stupid backoff scheme.")
scores.db <- lapply(freq.db,score.sbackoff) # computes scores for each sample
print("Done computing scores.")

print("Saving scores database in *dbScoresSB.r*")
save(scores.db, ngrams.db, bases.db, file="dbScoresSB.r")
print("Finished saving scores database!")

print("Completed makeScoresSB.R")

print("Resetting to project directory.")
setwd(prj.dir)

