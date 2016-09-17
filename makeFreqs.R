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

#Select document text matrix directory. THIS IS REQUIRED
dtms.dir <- c("../XX.dir")
print(paste("Going to directory containing Document Text Matrix: ",dtms.dir))
setwd(dtms.dir)

#Select document text matrix to load. THIS IS REQUIRED
dtms.file <- c("dtms.r")

# Loading document text matrices
load("dtms.r")
dtms.var <- ls(pattern="dtms")
print(paste("Loaded document text matrices: ", paste(dtms.var)))

# Computing frequencies

# dtms is a list with one element per sample set. Each element has
# DTM for unigram ... quadgram (or higher depending on max.ngram)
print("Computing frequencies and sorting in decreasing order...")
freqs.db <- lapply(dtms,dtm2freq) # THIS RETURNS SORTED HIGHER FREQ FIRST

# Generating additional data

#### NOTE ####
# Structure of object freqs.db is a list with one element per sample set
# For each sample set you have a list with frequencies for unigrams, bigrams, trigrams, and
# and quadgrams.
#    SORT - already done
#    COLLECT sorted ngrams
ngrams.db <- lapply(freqs.db, # over samples
                          function(x) lapply(x, # over n-grams of each sample
                                             names))
#    COLLECT the base, or the ngram minus the last word; nonsense for unigrams
bases.db <- lapply(ngrams.db,
                         function(x) { lapply(x[-1], function(x) unlist(dropLastWord(x))) } )

#     GET number of ngrams for each N, as a list, (includes repetitions)
N.ngrams.db <- lapply(freqs.db, function(x) lapply(x,sum))
# 
#     GET number of ngrams for each N, as a list, (no repetitions), i.e. the Vocabulary
V.ngrams.db <- lapply(ngrams.db,function(x) lapply(x,length))

print("Saving frequencies, etc...")
if(file.exists("freqs.r")){
  file.remove("freqs.r")
}
save(freqs.db,ngrams.db,bases.db,N.ngrams.db,V.ngrams.db,file="freqs.r")
print("Finished saving freqs.db stuff.")

print("Completed makeFreqs.R")

print("Resetting to project directory.")
setwd(prj.dir)

