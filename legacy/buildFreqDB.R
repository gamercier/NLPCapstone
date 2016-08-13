# Capstone Project
# file: buildFreqDB.R
#   Takes the dtm matrices computed using makeDTM.R and saved in dbdtm.r
#   to compute the ngram frequencies. Saves these in dbfreq.rresults from doing text analysis with t

print ("Starting buildFreqDB.R...")
# Go to project directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory:",getwd()))

source("helpers.R")
load("dbdtms.r")

dtms <- list(unigram  = dtm.unigram,
             bigram   = dtm.bigram,
             trigram  = dtm.trigram,
             quadgram = dtm.quadgram)
freq.db <- list(unigram  = NULL,
              bigram   = NULL,
              trigram  = NULL,
              quadgram = NULL)

print("Computing frequencies and sorting in decreasing order...")
for(dtm in names(dtms)){
  freq.db[[dtm]] <- sort(colSums(as.matrix(dtms[[dtm]])),decreasing=TRUE)
}

# Generating additional data

#### NOTE ####
# Structure of object freq.db is a list that contains sublists, one per n-gram.
# Each sublist contains a subsublist with only one member, a named vector that contains
# the frequencies. The names are the n-grams.

# MAX_NGRAM = length(freq) # maximum N in NGRAMs

# PREPROCESSING
#    SORT - already done
#    COLLECT sorted ngrams
ngrams.db <- list(unigram  = names(freq.db$unigram),
               bigram   = names(freq.db$bigram),
               trigram  = names(freq.db$trigram),
               quadgram = names(freq.db$quadgram))

#    COLLECT the base, or the ngram minus the last word
bases.db = list(bigram = unlist(dropLastWord(ngrams.db$bigram)),
            trigram  = unlist(dropLastWord(ngrams.db$trigram)),
            quadgram = unlist(dropLastWord(ngrams.db$quadgram)))

#    GET number of ngrams for each N, as a list, (includes repetitions)
N.ngrams <- lapply(freq.db,sum)
#    GET number of ngrams for each N, as a list, (no repetitions)
unique.N.ngrams <- lapply(ngrams.db,length)

print("Saving frequencies, etc...")
if(file.exists("dbfreq.r")){
  file.remove("dbfreq.r")
}
save(freq.db,ngrams.db,bases.db,N.ngrams,unique.N.ngrams,file="dbfreq.r")
print("Finished buildFreqDB.R!")