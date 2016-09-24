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

##### Set save file name and save variable names below ### REQUIRED

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
names(bases.db) <- names(ngrams.db)

#     GET number of ngrams for each N, as a list, (includes repetitions)
N.ngrams.db <- lapply(freqs.db, function(x) lapply(x,sum))
# 
#     GET number of ngrams for each N, as a list, (no repetitions), i.e. the Vocabulary
V.ngrams.db <- lapply(ngrams.db,function(x) lapply(x,length))

###### SET SAVE FILE ##### REQUIRE
save.file <- "freqs.dense.r"

print("Saving frequencies, etc...")
if(file.exists(save.file)){
  file.remove(save.file)
}

###### SET NAMES TO SAVE  ## REQUIRED
freqs.dense.db    <- freqs.db
ngrams.dense.db   <- ngrams.db
bases.dense.db    <- bases.db
N.ngrams.dense.db <- N.ngrams.db
V.ngrams.dense.db <- V.ngrams.db

save(freqs.dense.db,
     ngrams.dense.db,
     bases.dense.db,
     N.ngrams.dense.db,
     V.ngrams.dense.db,file=save.file)
print("Finished saving freqs.dense.db stuff.")

print("Completed makeFreqs.R")

print("Resetting to project directory.")
setwd(prj.dir)

