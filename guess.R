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
getLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { n<-length(s); s[-(1:(n-1))] }))
}

top3 <- function(v){
  n <- length(v)
  hits <- sort(v,partial=c(n,n-1,n-2))[n:(n-2)]
  idx <- unlist(sapply(hits, function(x) which(v == x)))
  return(v[idx])
}

############################### versions of score.sb #####################
######## VERSION UNRAVELING DATABASE LISTS
# scoresDB <- ___

# scores.uni.v <- as.vector(scoresDB$unigram,mode="double")
# scores.bi.v <- as.vector(scoresDB$bigram,mode="double")
# scores.tri.v <- as.vector(scoresDB$trigram,mode="double")
# scores.quad.v <- as.vector(scoresDB$quadgram,mode="double")
# ngrams.uni.v <- as.vector(names(scoresDB$unigram),mode="character")
# ngrams.bi.v <- as.vector(names(scoresDB$bigram),mode="character")
# ngrams.tri.v <- as.vector(names(scoresDB$trigram),mode="character")
# ngrams.quad.v <- as.vector(names(scoresDB$quadgram),mode="character")

nolist_score.sb <- function(ngram,
                          scores.uni.lut=scores.uni.v,
                          scores.bi.lut=scores.bi.v,
                          scores.tri.lut=scores.tri.v,
                          scores.quad.lut=scores.quad.v,
                          ngrams.uni.lut=ngrams.uni.v,
                          ngrams.bi.lut=ngrams.bi.v,
                          ngrams.tri.lut=ngrams.tri.v,
                          ngrams.quad.lut=ngrams.quad.v,
                          alpha=0.4){
  score.it <- function(ngram,words,scores.v,ngrams.v){
    idx <- which(ngram == ngrams.v)
    if(length(idx)){ # found
      return(scores.v[idx])
    } else{
      return(alpha*score.sb(unlist(toStr(list(words[-1])))))
    }
  }
  words <- unlist(toWords(ngram))
  n <- length(words)
  if(n == 4) return(score.it(ngram,words,scores.quad.v,ngrams.quad.v))
  if(n == 3) return(score.it(ngram,words,scores.tri.v,ngrams.tri.v))
  if(n == 2) return(score.it(ngram,words,scores.bi.v,ngrams.bi.v))
  if(n == 1) return(score.it(ngram,words,scores.uni.v,ngrams.uni.v))
}

############## END OF UNRAVELING DATABASE LISTS

############## VERSION WITH LISTS - WHILE LOOP
### scoresDB <- ___
### ngramsDB <- ___

loop_score.sb <- function(ngram,scores.lut=scoresDB,ngrams.lut=ngramsDB,alpha=0.4){
  found <- function(x,m) x %in% ngrams.lut[[m]]
  while(n>0){
    words <- unlist(toWords(ngram))
    n <- length(words)
    if(found(ngram,n)){
      return((alpha^(4-n)) * scores.lut[[n]][ngram])
    }
    ngram <- unlist(toStr(list(words[-1])))
    n <- n-1
  }
}
############## END OF VERSION WITH LISTS - WHILE LOOP

############## VERSION WITH LISTS - RECURSSION
### scoresDB <- ___
### ngramsDB <- ___

rec_score.sb <- function(ngram,scores.lut=scoresDB,ngrams.lut=ngramsDB,alpha=0.4){
  words <- unlist(toWords(ngram))
  n <- length(words)
  if(n > 1){
    if(ngram %in% ngrams.lut[[n]]){
      return(scores.lut[[n]][ngram])
    } else {
      return(alpha*score.sb(unlist(toStr(list(words[-1])))))
    }
  } # n=1
  return(alpha*scores.lut[[n]][ngram])
}
############## END OF VERSION WITH LISTS - RECURSSION

############## VERSION WITH LISTS - VECTORIZED
### scoresDB <- ___
### ngramsDB <- ___
### TOP.UNI.SCORES <- ___

vec_score.sb <- function(ngrams,scores.lut=scoresDB,ngrams.lut=ngramsDB,alpha=0.4){
  divide <- function(v1,v2){
    t <- (v1 %in% v2)
    return(list(input=v1[t],output=v1[!t]))
  }
  n <- 4
  v <- ngrams
  sc <- c((alpha^3)*TOP.UNI.SCORES) #
  while(n>1){
    d <- divide(v,ngrams.lut[[n]])
    if(length(d$input)) sc <- c(sc,(alpha^(4-n))*scores.lut[[n]][d$input])
    if(length(d$output)) v <- unlist(dropFirstWord(d$output))
    n <- n-1
  }
  return(sc)
}

############### VERSIONS of guess.sb ########################
guess.sb <- function(trigram,dict=ngrams.uni.v,score.f=vec_score.sb){
  ### using loop_score.sb as default. may replace with nolist_score.sb or rec_score.sb
  if(identical(score.f,vec_score.sb)) {
    scores <- score.f(paste(trigram,dict))
  } else {
    scores <- sapply(paste(trigram,dict),score.f)
  }
  out <- top3(scores)
  guesses <- unlist(getLastWord(names(out)))
  data.frame(guess=guesses[1:3],scores=out[1:3],row.names=c("1st","2nd","3rd"))
}

vec_guess.sb <- function(trigram,dict=ngramsDB$unigram){
  scores <- vec_score.sb(paste(trigram,dict))
  out <- top3(scores)
  guesses <- unlist(getLastWord(names(out)))
  data.frame(guess=guesses[1:3],scores=out[1:3],row.names=c("1st","2nd","3rd"))
}

#################################################################################
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