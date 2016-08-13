# Capstone Project
# file buildScoresDB.R
# Computes ngram scores

# LOADING TOOLS AND FREQUENCY NGRAM DATA FROM TM processing
# Go to project directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory:",getwd()))

print("Loading tools ...")
source("helpers.R")
print("Finished loading tools!")

print("Loading database dbfreq.r...")
load("dbfreq.r") # loads freq.db object
print("Finished loading dbfreq.r database!")

# Define SCORING FUNCTION.
#    score(ngram) = counts(ngram)/counts(ngram with first word dropped) if n >1
#    score(1gram) = counts(1gram)/(total number of 1grams (not unique 1grams))
#For stupid back up implementation:
#http://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification
# Input: Vector of ngrams (NOT a list of ngrams)
dbScore.sb <- function(ngram,n){
  freq.db[[n]]/(freq.db[[(n-1)]][unlist(dropFirstWord(names(freq.db[[n]])))])
  # names remain those of the freq.db[[n]] vector
}

print("Computing ngram scores for the database... Starting with unigrams")
scores.db = list()
scores.db$unigram <- freq.db$unigram/N.db$unigram
# no need define the names for the vector. Carried over from freq.db$unigram

print("Moving to higher order ngrams... ")
for(n in seq(2,length(freq.db))){
  scores.db[[n]] <- dbScore.sb(freq.db[[n]],n)
}
names(scores.db) <- names(freq.db)

print("Saving scores database in *dbScores.r*")
save(scores.db, ngrams.db, base.db, file="dbScores.r")
print("Finished saving scores database!")

### Prune score database

#    GENERATE SHORT version
print("Shortening ngram database!")

# unigrams: the first 10K words make up 90% of the corpus.
# bigrams: first 50% just above 200K; 63% is 400K; 87% is 700K
# trigrams: every 25% is a bit less than 300K
# quadgrams: every 25% is 300K

# build short lists of ngrams and their frequencies
# The structure of these objects is parallels that of freq.sorted object:
ndict <- 8000
short.ngrams <- list(unigram = ngrams.sorted$unigram[1:ndict])

cut.off = 2
short.freq  <- list(unigram   = freq.sorted$unigram[short.ngrams$unigram],
                    bigram    = freq.sorted$bigram[freq.sorted$bigram > cut.off],
                    trigram   = freq.sorted$trigram[freq.sorted$trigram > cut.off],
                    quadgram  = freq.sorted$quadgram[freq.sorted$quadgram > cut.off])

short.ngrams$bigram <- names(short.freq$bigram)
short.ngrams$trigram <- names(short.freq$trigram)
short.ngrams$quadgram <- names(short.freq$quadgram)

short.N.ngram <- lapply(short.freq,sum)
short.unique.N.ngram <- lapply(short.ngrams,length)

# for speed, generate base of ngrams. The base is everything except the last word(token)
short.ngrams.base = list(bigram = unlist(dropLastWord(short.ngrams$bigram)),
                         trigram  = unlist(dropLastWord(short.ngrams$trigram)),
                         quadgram = unlist(dropLastWord(short.ngrams$quadgram)))

