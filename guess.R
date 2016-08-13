# Capstone Project
# file guess.R
# Given a trigram, guess the upcoming ngram - based on stupic back off

# Change directory to location of database
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir)
print(paste("Current directory: ",getwd()))

library(stringr)
# helper functions
toWords <- function(ngram) str_split(ngram,pattern=boundary("word"))
toStr <- function(words) lapply(words,str_c,collapse=" ")
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n
  toStr(lapply(words,function(s) s[2:n]))
}

# MAIN FUNCTION
guess.sb <- function(trigram,scores=scoresDB,ngrams=ngramsDB,bases=basesDB){
  ngram <- trigram
  n <- 3
  hits <- c((0.4^3)*TOPUNI.SCORES)
  while(n > 0) {
    if(ngram %in% ngrams[[n]]){
      hits <-
        c(hits,((0.4)^(3-n))*scores[[(n+1)]]
          [ ngrams[[(n+1)]][ ngram == bases[[(n)]] ] ]) # basesDB is offset down by 1
    }
    # back off
    ngram <- dropFirstWord(ngram)
    n <- n-1
  }
  scores.sorted <- sort(hits,decreasing=TRUE)
  scores.sorted <- scores.sorted[unique(names(scores.sorted))][1:3]
  words.sorted <- toWords(names(scores.sorted))
  guesses <- sapply(words.sorted, function(x) x[length(x)] )
  return(data.frame(guess=guesses,scores=scores.sorted))
}


# Using un-edited database
print("Loading score database ...")
load("dbScores.r")
print("Finished loading scores database!")

# Un-edited (not trimmed) database:
#   scores.db, ngrams.db, base.db
# These are lists with each element corresponding to a different
# sample of the corpus.
# Here we use the first sample.
scoresDB <- scores.db[[1]]; TOPUNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.db[[1]]
basesDB <- bases.db[[1]]

# Testing speed with long database
print("Testing with un-edited database.")
time <- proc.time()
guess.sb("book book book")
proc.time() - time

rm(scores.db,ngrams.db,bases.db)

# Using short database
print("Loading short score database ...")
load("dbShortScores.r")
print("Finished loading short scores database!")

# The strategy 1 short databases:
#     s.scoresDB, s.ngramsDB ,and s.basesDB
#     only apply to a single sample of the corpus.
scoresDB <- s.scoresDB; TOPUNI.SCORES <- s.scoresDB$unigram[1:3]
ngramsDB <- s.ngramsDB
basesDB <- s.basesDB

# Test using short database
print("Testing with short database.")
time <- proc.time()
guess.sb("book book book")
proc.time() - time

rm(s.scoresDB,s.ngramsDB,s.basesDB)

# Using minimal database
print("Loading minimal score database ...")
load("dbMinScores.r")
print("Finished loading short scores database!")

# The strategy 1 and 2 edited, minimal databases:
#     m.scores.db, m.ngrams.db ,and m.bases.db
#     only apply to a single sample of the corpus.
scoresDB <- m.scoresDB; TOPUNI.SCORES <- m.scoresDB$unigram[1:3]
ngramsDB <- m.ngramsDB
basesDB <- m.basesDB

# Test using shorter database
print("Testing with minimal database.")
time <- proc.time()
guess.sb("book book book")
proc.time() - time

rm(m.scoresDB,m.ngramsDB,,m.basesDB)