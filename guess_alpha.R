# Capstone Project
# file guess_alpha.R
# Here I develop different algorithms for the guess function.
# Right now I focus on stupid back off for scoring
# Given a trigram, guess the upcoming ngram - based on stupid back off

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

getLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { tail(s,1) }))
}
countWords <- function(ngram) lapply(toWords(ngram),length)

top3 <- function(v){
  n <- length(v)
  hits <- sort(v,partial=c(n,n-1,n-2))[n:(n-2)]
  idx <- unlist(sapply(hits, function(x) which(v == x)))
  return(v[idx])
}

############################### versions of score.sb #####################
######## VERSION UNRAVELING DATABASE LISTS
# NECESSARY GLOBALS
# scoresDB <- ___

# SCORES.UNI.V <- as.vector(scoresDB$unigram,mode="double")
# SCORES.BI.V <- as.vector(scoresDB$bigram,mode="double")
# SCORES.TRI.V <- as.vector(scoresDB$trigram,mode="double")
# SCORES.QUAD.V <- as.vector(scoresDB$quadgram,mode="double")
# NGRAMS.UNI.V <- as.vector(names(scoresDB$unigram),mode="character")
# NGRAMS.BI.V <- as.vector(names(scoresDB$bigram),mode="character")
# NGRAMS.TRI.V <- as.vector(names(scoresDB$trigram),mode="character")
# NGRAMS.QUAD.V <- as.vector(names(scoresDB$quadgram),mode="character")

# ALPHA <- 0.4

nolist_score.sb <- function(ngram){
  score.it <- function(ngram,words,scores.v,ngrams.v){
    idx <- which(ngram == ngrams.v)
    if(length(idx)){ # found
      return(scores.v[idx])
    } else{
      return(ALPHA*nolist_score.sb(unlist(toStr(list(words[-1])))))
    }
  }
  words <- unlist(toWords(ngram))
  n <- length(words)
  if(n == 4) return(score.it(ngram,words,SCORES.QUAD.V,NGRAMS.QUAD.V))
  if(n == 3) return(score.it(ngram,words,SCORES.TRI.V,NGRAMS.TRI.V))
  if(n == 2) return(score.it(ngram,words,SCORES.BI.V,NGRAMS.BI.V))
  if(n == 1) return(score.it(ngram,words,SCORES.UNI.V,NGRAMS.UNI.V))
}

############## END OF UNRAVELING DATABASE LISTS

############## VERSION WITH LISTS - WHILE LOOP
### scoresDB <- ___
### ngramsDB <- ___
### ALPHA <- 0.4

loop_score.sb <- function(ngram,n){
  found <- function(x,m) x %in% ngramsDB[[m]]
  while(n>0){
    words <- unlist(toWords(ngram))
    n <- length(words)
    if(found(ngram,n)){
      return((ALPHA^(4-n)) * scoresDB[[n]][ngram])
    }
    ngram <- unlist(toStr(list(words[-1])))
    n <- n-1
  }
}
############## END OF VERSION WITH LISTS - WHILE LOOP

############## VERSION WITH LISTS - RECURSSION
### scoresDB <- ___
### ngramsDB <- ___
### ALPHA <- 0.4

rec_score.sb <- function(ngram,n){
  # scores.lut <- scoresDB; ngrams.lut <- ngramsDB; alpha <- ALPHA
  words <- unlist(toWords(ngram))
  n <- length(words)
  if(n > 1){
    if(ngram %in% ngramsDB[[n]]){
      return(scoresDB[[n]][ngram])
    } else {
      return(ALPHA*rec_score.sb(unlist(toStr(list(words[-1])))))
    }
  } # n=1
  return(ALPHA*scoresDB[[n]][ngram])
}
############## END OF VERSION WITH LISTS - RECURSSION

############## VERSION WITH LISTS - VECTORIZED
### scoresDB <- ___
### ngramsDB <- ___
### TOP.UNI.SCORES <- ___
### ALPHA <- 0.4

vec_score.sb <- function(ngrams){
  # scores.lut <- scoresDB; ngrams.lut <- ngramsDB; alpha <- ALPHA
  divide <- function(v1,v2){
    t <- (v1 %in% v2)
    return(list(input=v1[t],output=v1[!t]))
  }
  n <- length(unlist(toWords(ngrams[1]))) # assumes all ngrams have the same number of words
  v <- ngrams
  # for unigrams we only return the top three; scores are presorted
  sc <- c((ALPHA^(n-1))*TOP.UNI.SCORES)
  while(n>1){
    d <- divide(v,ngramsDB[[n]])
    if(length(d$input)) sc <- c(sc,(ALPHA^(4-n))*scoresDB[[n]][d$input])
    if(length(d$output)) v <- unlist(dropFirstWord(d$output))
    n <- n-1
  }
  return(sc)
}

############################### END OF versions of score.sb #####################

############### VERSIONS of guess.sb ########################

############ NON-VECTORIZED ###########
### scoresDB <- ___
### ngramsDB <- ___
### ALPHA <- 0.4
### SCORE.F <- vec_score.sb (choices: loop_score.sb, rec_score.sb, vec_score.sb)
### DICT <- ngramsDB$unigram

###### PROBLEM HERE... names in vectors get appened like i am to -> i am to.to
######                 this leads to not detecting the matches!
guess.sb <- function(trigram){
  ### options for score.f
  if(identical(SCORE.F,vec_score.sb)) {
    scores <- SCORE.F(paste(trigram,DICT))
  } else {
    scores <- sapply(paste(trigram,DICT),SCORE.F,n=4)
  }
  out <- top3(scores)
  # guesses <- unlist(getLastWord(names(out)))
  # the score functions built here will append the matching ngram to the
  # trial quadgrams with the format: trial_quadgram.matching_ngram
  # the guessed word is the very last word in the matching_ngram
  # hence the need for the last split
  guesses <- unlist(lapply(str_split(names(out),pattern="[. ]"),tail,1))
  data.frame(guess=guesses[1:3],scores=out[1:3],row.names=c("1st","2nd","3rd"))
}
###########################################
############# VERSION VECTORIZED ##########
### NEEDS THESE DEFINED
### scoresDB <- ___
### ngramsDB <- ___
### DICT <- ___
### TOP.UNI.SCORES <- ___
### ALPHA <- 0.4

vec_guess.sb <- function(ngram){
  scores <- vec_score.sb(paste(ngram,DICT))
  out <- top3(scores)
  # guesses <- unlist(getLastWord(names(out)))
  # the score functions built here will append the matching ngram to the
  # trial quadgrams with the format: trial_quadgram.matching_ngram
  # the guessed word is the very last word in the matching_ngram
  # hence the need for the last split
  guesses <- unlist(lapply(str_split(names(out),pattern="[. ]"),tail,1))
  data.frame(guess=guesses[1:3],scores=out[1:3],row.names=c("1st","2nd","3rd"))
}
###########################################

############## VERSION SIMPLE WITH BASEDB #########
# scoresDB <- scores.dense.db$one.pct
# ngramsDB <- ngrams.dense.db$tone.pct
# basesDB  <-  bases.dense.db$one.pct 
# TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
# ALPHA <- 0.4

# simple_guess_sb was modified 2016-09-18 stringsAsFactors = FALSE
#### THIS only really need scoresDB and basesDB with ALPHA and TOP.UNI.SCORES
simple_guess.sb <- function(base_ngram){
  words <- unlist(toWords(base_ngram))
  n <- length(words)   # size of base_ngram. ngram size is n+1
  nMax <- n
  scores <- (ALPHA^nMax)*TOP.UNI.SCORES # ngram is unigram (base_ngram is ""), n=0
  while(n>0){
    base <- paste(words[(nMax-n+1):nMax],collapse=" ")
    hits <- base == basesDB[[n]]  # basesDB[[n]] has base for ngramsDB[[(n+1)]]
    if(sum(hits)){
      # scores <- c(scores,(ALPHA^(nMax-n))*scoresDB[[(n+1)]][ ngramsDB[[(n+1)]][hits] ])
      scores <- c(scores,(ALPHA^(nMax-n))*scoresDB[[(n+1)]][ hits ])
    }
    n <- n-1
  }
  guesses <- top3(scores)
  data.frame(guess=unlist(getLastWord(names(guesses)))[1:3],
             scores=guesses[1:3], sources=names(guesses),
             row.names=c("1st","2nd","3rd"),
             stringsAsFactors = FALSE)
}

#################END OF VERSIONS OF guess.sb ###################################
# Using un-edited database
setwd("try_02")
print("Loading score database ...")
load("dbScores.r")
print("Finished loading scores database!")

# Un-edited (not trimmed) database:
#   scores.db, ngrams.db
# These are lists with each element corresponding to a different
# sample of the corpus.
# Here we use the first sample.
scoresDB <- scores.db[[1]]
ngramsDB <- ngrams.db[[1]]

# Testing speed with long database
print("Testing with un-edited database.")
time <- proc.time()
guess.sb("book book book")
proc.time() - time

rm(scores.db,ngrams.db)

# Using short database
print("Loading short score database ...")
load("dbShortScores.r")
print("Finished loading short scores database!")

# The strategy 1 short databases:
#     s.scoresDB, s.ngramsDB
#     only apply to a single sample of the corpus.
scoresDB <- s.scoresDB
ngramsDB <- s.ngramsDB

# Test using short database
print("Testing with short database.")
time <- proc.time()
vec_guess.sb("book book book")
proc.time() - time

rm(s.scoresDB,s.ngramsDB)
setwd("..")

# Using dense document text matrix for all dbSp33Scores_All.r
setwd("try_04")
print("Loading dense score database ...")
load("dbSp33Scores_All.r")
print("Finished loading dense scores database!")

# Strategy here is to make sparsity 0% (actually set to threshold of 33% but yields 0%)
scoresDB <- scores.db.sp33[[1]]
ngramsDB <- ngrams.db.sp33[[1]]

TOP.UNI.SCORES <- scoresDB$unigram[1:3]

# Test using shorter database
print("Testing with minimal database.")
time <- proc.time()
vec_guess.sb("book book book")
proc.time() - time

rm(scores.db.sp33,ngrams.db.sp33,bases.db.sp33)
setwd("..")

# vec_guess.sb("date at the") gives an error
