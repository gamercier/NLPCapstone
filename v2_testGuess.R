# Capstone Project
# File: probeGuess.R
# Tests guessing function against a database of quadgrams in quad.r

# automating testing
testing <- function(test.s){
  hit <- function(x,a=a){
    guesses <- guess.sb(unlist(dropLastWord(x)))$guess
    if(unlist(getLastWord(x)) %in% guesses){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  test.f <- sapply(test.s,hit)
  return(test.f)
}

testing.answer <- function(test.s){
  hit <- function(x,a=a){
    guesses <- guess.sb(unlist(dropLastWord(x)))$guess
    if(unlist(getLastWord(x)) %in% guesses){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  test.f <- sapply(test.s,hit)
  # returns the actual test quadgram that got guessed correctly (within top 3)
  return(test.s[test.f])
}

# helper functions
library(stringr)
# helper functions
toWords <- function(ngram) str_split(ngram,pattern=boundary("word"))
toStr <- function(words) lapply(words,str_c,collapse=" ")
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n
  toStr(lapply(words,function(s) s[2:n]))
}
dropLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { n<-length(s); s[1:(n-1)] }))
}
getLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { tail(s,1) }))
}
countWords <- function(ngram) lapply(toWords(ngram),length)

toSpace <- function(x, pattern, ...){
  return(gsub(pattern," ", x, ...))
}
toNone <- function(x, pattern, ...){
  return(gsub(pattern,"", x, ...))
}

top3 <- function(v){
  n <- length(v)
  hits <- sort(v,partial=c(n,n-1,n-2))[n:(n-2)]
  idx <- unlist(sapply(hits, function(x) which(v == x)))
  return(v[idx])
}

########## Test Series A: input: test_quads_dev.r; method: stupid backoff; db: scores.dense.db
# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir); getwd()

print("Loading data for testing!")
if(file.exists(file.path(prj.dir,"quads_tst.r"))){
  load(file.path(prj.dir,"quads_tst.r"))
  print("Loaded (quads_tst.r) development quadgrams for testing.")
} else { # not present produce it
  load(file.path(prj.dir,"quads_dev.r"))
  set.seed(1974) # for reproducibility
  n.sample <- 1000
  test.sample <- quads[sample.int(length(quads),n.sample)]
  save(test.sample,file=file.path(prj.dir,"quads_tst.r"))
  print("Built and saved, quads_tst.r, new set of quadgrams from development set for testing")
}

#### set guessing function:
### NEEDS THESE DEFINED
### scoresDB <- ___
### ngramsDB <- ___
### DICT <- ___
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

# Change directory to corpus directory
print("Loading scoresSB.dense.r database ...")
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir); getwd()
load("scoresSB.dense.r") # loads variable short.dbs
print("Finished loading scoresSB.dense.r databases!")

##### Test Series A: scores.dense.db but using vec_guess.sb
#####   clean corpus from unix.xx.txt, but with purify options toASCII, collapseContractions,
#####   and collapseHyphens. DTM trimmed to sparsity 0%.
##### test data from test_quads_dev.r have purify with options toASCII, collapseContractions,
#####   and collapseHyphens OFF!

# For Test Series A
#### Test Series A: Test 1 db: scores.dense.db$one.pct
# loading scores db from 1 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$one.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$one.pct ; DICT <- ngramsDB$unigram
alphas <- c(0.2,0.4,0.6,0.8)
print("Test 1: input: sample_quads_dev.r; database: 1pct; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",alpha))
}

# NEW NEW NEW results Test Series A, Test 1 with updated version of vec_guess.sb
# [1] "Test 1: input: sample_quads_dev.r; database: 1pct; method: stupid backoff"
# [1] "Hits:  87  out of  1000 alpha= 0.2"
# [1] "Hits:  81  out of  1000 alpha= 0.4"
# [1] "Hits:  83  out of  1000 alpha= 0.6"
# [1] "Hits:  80  out of  1000 alpha= 0.8"

### When compared to simple_guess.sb I am missing results. See analysis below
### for example: bigram "i am" is not hit when using base trigram "by who i"

#### Test Series A: Test 2 db: scores.dense.db$five.pct
# loading scores db from 5 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$five.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$five.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test 1: input: sample_quads_dev.r; database: 5pct; method: stupid backoff")
for(alpha in alphas){
  results <- testing(test.sample,a=alpha)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",alpha))
}

# results Test Series A, Test 2:
#### Test Series A: Test 3 db: scores.dense.db$ten.pct
# loading scores db from 10 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$ten.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$ten.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test 1: input: sample_quads_dev.r; database: 10pct; method: stupid backoff")
for(alpha in alphas){
  results <- testing(test.sample,a=alpha)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",alpha))
}

# results Test Series A, Test 3:
# [1] "Test 1: input: sample_quads_dev.r; database: 10pct; method: stupid backoff"


###########################################################################

########## Test Series B: input: test_quads_dev.r; method: stupid backoff; db: scores.dense.db
############## VERSION SIMPLE WITH BASEDB #########
# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir); getwd()

print("Loading data for testing!")
if(file.exists(file.path(prj.dir,"test_quads_dev.r"))){
  load(file.path(prj.dir,"test_quads_dev.r"))
  print("Loaded (test_quads_dev.r) development quadgrams for testing.")
} else { # not present produce it
  load(file.path(prj.dir,"quads_dev.r"))
  set.seed(1974) # for reproducibility
  n.sample <- 1000
  test.sample <- quads[sample.int(length(quads),n.sample)]
  save(test.sample,file=file.path(prj.dir,"test_quads_dev.r"))
  print("Built and saved, test_quads_dev.r, new set of quadgrams from development set for testing")
}

# scoresDB <- scores.dense.db$one.pct
# ngramsDB <- ngrams.dense.db$tone.pct
# basesDB  <-  bases.dense.db$one.pct 
# TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
# ALPHA <- 0.4

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
             scores=guesses[1:3],row.names=c("1st","2nd","3rd"))
}

guess.sb <- simple_guess.sb

# Change directory to corpus directory
print("Loading scoresSB.dense.r database ...")
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir); getwd()
load("scoresSB.dense.r") # loads variable short.dbs
print("Finished loading scoresSB.dense.r databases!")

##### Test Series B: scores.dense.db but guess.sb <- simple_guess.sb
#####   clean corpus from unix.xx.txt, but with purify options toASCII, collapseContractions,
#####   and collapseHyphens. DTM trimmed to sparsity 0%.
##### test data from test_quads_dev.r have purify with options toASCII, collapseContractions,
#####   and collapseHyphens OFF!

#### Test Series B: Test 1 db: scores.dense.db$one.pct but guess.sb <- simple_guess.sb
# loading scores db from 1 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$one.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$one.pct ; basesDB <- bases.dense.db$one.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test 1: input: sample_quads_dev.r; database: 1pct; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",ALPHA))
}

# [1] "Test 1: input: sample_quads_dev.r; database: 1pct; method: stupid backoff"
# [1] "Hits:  89  out of  1000 alpha= 0.2"
# [1] "Hits:  84  out of  1000 alpha= 0.4"
# [1] "Hits:  85  out of  1000 alpha= 0.6"
# [1] "Hits:  83  out of  1000 alpha= 0.8"

#### Test Series B: Test 2 db: scores.dense.db$five.pct but guess.sb <- simple_guess.sb
# loading scores db from 5 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$five.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$five.pct ; basesDB <- bases.dense.db$five.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test 1: input: sample_quads_dev.r; database: 5pct; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",ALPHA))
}

# [1] "Test 1: input: sample_quads_dev.r; database: 5pct; method: stupid backoff"
# [1] "Hits:  85  out of  1000 alpha= 0.2"
# [1] "Hits:  85  out of  1000 alpha= 0.4"
# [1] "Hits:  83  out of  1000 alpha= 0.6"
# [1] "Hits:  78  out of  1000 alpha= 0.8"

#### Test Series B: Test 3 db: scores.dense.db$ten.pct but guess.sb <- simple_guess.sb
# loading scores db from 10 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$ten.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$ten.pct ; basesDB <- bases.dense.db$ten.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test 1: input: sample_quads_dev.r; database: 10pct; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",ALPHA))
}
# [1] "Test 1: input: sample_quads_dev.r; database: 10pct; method: stupid backoff"
# [1] "Hits:  79  out of  1000 alpha= 0.2"
# [1] "Hits:  80  out of  1000 alpha= 0.4"
# [1] "Hits:  76  out of  1000 alpha= 0.6"
# [1] "Hits:  76  out of  1000 alpha= 0.8"

########## Test Series C: input: test_quads_dev.r; method: stupid backoff; db: 50% corpus: scores.dense.db
############## VERSION SIMPLE WITH BASEDB #########
# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir); getwd()

print("Loading data for testing!")
if(file.exists(file.path(prj.dir,"test_quads_dev.r"))){
  load(file.path(prj.dir,"test_quads_dev.r"))
  print("Loaded (test_quads_dev.r) development quadgrams for testing.")
} else { # not present produce it
  load(file.path(prj.dir,"quads_dev.r"))
  set.seed(1974) # for reproducibility
  n.sample <- 1000
  test.sample <- quads[sample.int(length(quads),n.sample)]
  save(test.sample,file=file.path(prj.dir,"test_quads_dev.r"))
  print("Built and saved, test_quads_dev.r, new set of quadgrams from development set for testing")
}

# scoresDB <- scores.dense.db$fifty.pct
# ngramsDB <- ngrams.dense.db$fifty.pct
# basesDB  <-  bases.dense.db$fifty.pct 
# TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
# ALPHA <- 0.4

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
             scores=guesses[1:3],row.names=c("1st","2nd","3rd"))
}

guess.sb <- simple_guess.sb

# Change directory to corpus directory
print("Loading scoresSB.dense.r database ...")
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir); getwd()
load("scoresSB.dense.r") # loads variable short.dbs
print("Finished loading scoresSB.dense.r databases!")

##### Test Series C: 50% corpus scores.dense.db but guess.sb <- simple_guess.sb
#####   clean corpus from unix.xx.txt, but with purify options toASCII, collapseContractions,
#####   and collapseHyphens. DTM trimmed to sparsity 0%. (actually 33% but goes to 0%)
##### test data from test_quads_dev.r have purify with options toASCII, collapseContractions,
#####   and collapseHyphens OFF! NOT MATCHING scores.dense.db

#### Test Series C: Test 1 db: scores.dense.db$50.pct but guess.sb <- simple_guess.sb
# loading scores db from 50 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$fifty.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$fifty.pct ; basesDB <- bases.dense.db$fifty.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test Series C Test 1: input: quads_dev.r; database: 50pct; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",ALPHA))
}

# [1] "Test Series C Test 1: input: sample_quads_dev.r; database: 50pct; method: stupid backoff"
# [1] "Hits:  16  out of  1000 alpha= 0.2"
# [1] "Hits:  17  out of  1000 alpha= 0.4"
# [1] "Hits:  19  out of  1000 alpha= 0.6"
# [1] "Hits:  19  out of  1000 alpha= 0.8"

# Looks like a larger corpus (50% sample) introduces a lot of noise and things get worse.
# There are 224,186 unique quadrams (length(ngramsDB$quadgram))
# However, 10,611 of these only appear once!
# sum(scoresDB$quadgram[scoresDB$quadgram == 1])

# Let's look at the cases with 1%, 5%, and 10%.
# 1%:       120/862    = 13.9%
# 5%:       960/10545  = 9.1%
# 10%:     2067/27812  = 7.4%
# 50%:    10611/224186 = 4.7%

# Conclusion:
# Increasing the sample of the corpus, increases the number of 1 time quadgrams
# by a factor of 1.8/% increase in sample, or 80% increase/% increase in sample.
# However, the fraction of these quadgrams, decreases much slower, -20%/% increase in sample
###############################################################################

########## TEST D repeats TEST C but input has been cleaned to match scoresDB
##########
########## Test Series D: input: test_quads_dev.r; method: stupid backoff; db: 50% corpus: scores.dense.db
############## VERSION SIMPLE WITH BASEDB #########
# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir); getwd()

print("Loading data for testing!")
if(file.exists(file.path(prj.dir,"test_quads_dev.r"))){
  load(file.path(prj.dir,"test_quads_dev.r"))
  print("Loaded (test_quads_dev.r) development quadgrams for testing.")
} else { # not present produce it
  load(file.path(prj.dir,"quads_dev.r"))
  set.seed(1974) # for reproducibility
  n.sample <- 1000
  test.sample <- quads[sample.int(length(quads),n.sample)]
  save(test.sample,file=file.path(prj.dir,"test_quads_dev.r"))
  print("Built and saved, test_quads_dev.r, new set of quadgrams from development set for testing")
}

# scoresDB <- scores.dense.db$fifty.pct
# ngramsDB <- ngrams.dense.db$fifty.pct
# basesDB  <-  bases.dense.db$fifty.pct 
# TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
# ALPHA <- 0.4

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
             scores=guesses[1:3],row.names=c("1st","2nd","3rd"))
}

guess.sb <- simple_guess.sb

# Change directory to corpus directory
print("Loading scoresSB.dense.r database ...")
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir); getwd()
load("scoresSB.dense.r") # loads variable short.dbs
print("Finished loading scoresSB.dense.r databases!")

##### Test Series D: 50% corpus scores.dense.db but guess.sb <- simple_guess.sb
#####   clean corpus from unix.xx.txt, but with purify options toASCII, collapseContractions,
#####   and collapseHyphens. DTM trimmed to sparsity 0%. (actually 33% but goes to 0%)
##### test data from test_quads_dev.r have purify options MATCHING scores.dense.db

#### Test Series D: Test 1 db: scores.dense.db$50.pct but guess.sb <- simple_guess.sb
# loading scores db from 50 pct corpus with sparsity = 0
scoresDB <- scores.dense.db$fifty.pct ; TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.dense.db$fifty.pct ; basesDB <- bases.dense.db$fifty.pct
alphas <- c(0.2,0.4,0.6,0.8)
print("Test Series C Test 1: input: quads_dev.r; database: 50pct; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",ALPHA))
}

# [1] "Hits:  21  out of  1000 alpha= 0.2"
# [1] "Hits:  25  out of  1000 alpha= 0.4"
# [1] "Hits:  25  out of  1000 alpha= 0.6"
# [1] "Hits:  25  out of  1000 alpha= 0.8"

# Conclusion: Preprocessing test quadgrams to match the training set
# preprocessing lead to more hits. Slightly larger value of alpha lead
# to better results.

########## TEST E repeats TEST D but uses a trimmed version of the scores.db
##########
########## Test Series E: input: test_quads_dev.r; method: stupid backoff;
#                         db: 50% corpus: scores.trimmed.dense.db
############## VERSION SIMPLE WITH BASEDB #########
# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir); getwd()

print("Loading data for testing!")
if(file.exists(file.path(prj.dir,"test_quads_dev.r"))){
  load(file.path(prj.dir,"test_quads_dev.r"))
  print("Loaded (test_quads_dev.r) development quadgrams for testing.")
} else { # not present produce it
  load(file.path(prj.dir,"quads_dev.r"))
  set.seed(1974) # for reproducibility
  n.sample <- 1000
  test.sample <- quads[sample.int(length(quads),n.sample)]
  save(test.sample,file=file.path(prj.dir,"test_quads_dev.r"))
  print("Built and saved, test_quads_dev.r, new set of quadgrams from development set for testing")
}

# scoresDB <- scores.trimmed.dense.db$fifty.pct
# ngramsDB <- ngrams.trimmed.dense.db$fifty.pct
# basesDB  <-  bases.trimmed.dense.db$fifty.pct 
# TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
# ALPHA <- 0.4

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
             scores=guesses[1:3],row.names=c("1st","2nd","3rd"))
}

guess.sb <- simple_guess.sb

# Change directory to corpus directory
print("Loading scoresSB.dense.r database ...")
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir); getwd()
load("scoresSB.trimmed.dense.r") 
print("Finished loading scoresSB.trimmed.dense.r databases!")

##### Test Series E: 50% corpus scores.trimmed.dense.db
#####                but guess.sb <- simple_guess.sb
#####   clean corpus from unix.xx.txt, but with purify options toASCII, collapseContractions,
#####   and collapseHyphens. DTM sparsity 0%. (actually 33% but goes to 0%)
##### test data from test_quads_dev.r have purify options MATCHING scores.dense.db

#### Test Series E: Test 1
# db: scores.trimmed.dense.db$50.pct but guess.sb <- simple_guess.sb
# loading scores db from 50 pct corpus with sparsity = 0, but trimmed
scoresDB <- scores.trimmed.dense.db$fifty.pct
TOP.UNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.trimmed.dense.db$fifty.pct
basesDB <- bases.trimmed.dense.db$fifty.pct

alphas <- c(0.2,0.4,0.6,0.8)
print("Test Series E Test 1: input: test_quads_dev.r; database: 50pct, trimmed; method: stupid backoff")
for(alpha in alphas){
  ALPHA <- alpha
  results <- testing(test.sample)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",ALPHA))
}

# [1] "Hits:  34  out of  1000 alpha= 0.2"
# [1] "Hits:  34  out of  1000 alpha= 0.4"
# [1] "Hits:  32  out of  1000 alpha= 0.6"
# [1] "Hits:  33  out of  1000 alpha= 0.8"

#Conclusion: trimming the database improve the hits but now
#  the alpha is almost irrelevant.
# Improvement is about 50%.

# TO DO NEXT
# REPEAT USING Database from 1 to 10% of corpus.
# Expect a hit rate of around 90-100



### ================================= Evaluating vec_guess.sb vs simple_guess.sb
# results.simple_guess alpha 0.2  using scoresDB$one.pct
# [1] "a renoir show to"                   
# [2] "to decide relation to"              
# [3] "tuesday feb york times"             
# [4] "eyebrow when yet another"           
# [5] "the spin rather than"               
# [6] "on satellite television and"        
# [7] "to combat terrorism the"            
# [8] "george mcgovern said he"            
# [9] "hospitals avoid teaching the"       
# [10] "in american dollars and"            
# [11] "are in hot water"                   
# [12] "a period when he"                   
# [13] "courted patriot movement and"       
# [14] "useful to say about"                
# [15] "gore did not know"                  
# [16] "benefits for employees and"         
# [17] "by who i am"                        
# [18] "in other words the"                 
# [19] "what the academics and"             
# [20] "who have already been"              
# [21] "its more interesting to"            
# [22] "impeachment proceedings against the"
# [23] "is whats killing the"               
# [24] "on a walk through"                  
# [25] "he delivers goodies to"             
# [26] "answer that matters and"            
# [27] "place is not an"                    
# [28] "go away far away"                   
# [29] "adjusted his style to"              
# [30] "to let you know"                    
# [31] "to a cabinet to"                    
# [32] "thing prescribing lots of"          
# [33] "to rent listings and"               
# [34] "why im going to"                    
# [35] "on do anyone else"                  
# [36] "post doesnt tell us"                
# [37] "reform plan three years"            
# [38] "casting himself eye to"             
# [39] "minds away from the"                
# [40] "policy nuances tend to"             
# [41] "americans are not the"              
# [42] "si bon croons the"                  
# [43] "if there was a"                     
# [44] "when people scrutinize and"         
# [45] "acting out ought to"                
# [46] "desist and let the"                 
# [47] "to various presidents and"          
# [48] "differ satanic pornographers and"   
# [49] "stripped of clutter and"            
# [50] "appeared on meet the"               
# [51] "this is meant to"                   
# [52] "hes al gore and"                    
# [53] "isnt paying attention to"           
# [54] "of flytraps key players"            
# [55] "by the cold war"                    
# [56] "widely believed i am"               
# [57] "its usat reefer the"                
# [58] "and friends control over"           
# [59] "of they were in"                    
# [60] "nor that friday night"              
# [61] "with it white house"                
# [62] "a child born to"                    
# [63] "provided spiritual counseling to"   
# [64] "juenger a world war"                
# [65] "discuss the meaning of"             
# [66] "a massive conflict the"             
# [67] "magazines radio stations and"       
# [68] "i dont think they"                  
# [69] "warren beatty would be"             
# [70] "both were forced to"                
# [71] "prejudices youll be able"           
# [72] "jeans real choice and"              
# [73] "brewed faster warmer and"           
# [74] "sick of flytrap and"                
# [75] "a imprudently trimmed the"          
# [76] "religion and of course"             
# [77] "for other governments to"           
# [78] "and an online store"                
# [79] "women like jones and"               
# [80] "cash flow toward the"               
# [81] "is their inability to"              
# [82] "high seems like a"                  
# [83] "its head within the"                
# [84] "one to wonder if"                   
# [85] "security unlike activex and"        
# [86] "claustrophobic small town where"    
# [87] "indices filed last week"            
# [88] "much as berio and"                  
# [89] "to previous prefromc the"  

# # results.simple_guess using alpha 0.8 scoresDB$one.pct
# [1] "a renoir show to"                   
# [2] "to decide relation to"              
# [3] "tuesday feb york times"             
# [4] "eyebrow when yet another"           
# [5] "the spin rather than"               
# [6] "on satellite television and"        
# [7] "to combat terrorism the"            
# [8] "george mcgovern said he"            
# [9] "hospitals avoid teaching the"       
# [10] "in american dollars and"            
# [11] "courted patriot movement and"       
# [12] "useful to say about"                
# [13] "gore did not know"                  
# [14] "benefits for employees and"         
# [15] "plodding but hard to"               
# [16] "by who i am"                                        # extra found                     
# [17] "in other words the"                 
# [18] "what the academics and"             
# [19] "its more interesting to"            
# [20] "impeachment proceedings against the"
# [21] "his subjects after the"             
# [22] "is whats killing the"               
# [23] "he delivers goodies to"             
# [24] "answer that matters and"            
# [25] "place is not an"                    
# [26] "money and people to"                
# [27] "go away far away"                   
# [28] "assistant whos doing the"           
# [29] "adjusted his style to"              
# [30] "girder holding him to"              
# [31] "to let you know"                    
# [32] "to a cabinet to"                    
# [33] "to rent listings and"               
# [34] "why im going to"                    
# [35] "on do anyone else"                  
# [36] "reform plan three years"            
# [37] "casting himself eye to"             
# [38] "the bosnia where the"               
# [39] "policy nuances tend to"             
# [40] "si bon croons the"                  
# [41] "when people scrutinize and"         
# [42] "acting out ought to"                
# [43] "desist and let the"                 
# [44] "to various presidents and"          
# [45] "differ satanic pornographers and"   
# [46] "stripped of clutter and"            
# [47] "a happy man to"                     
# [48] "appeared on meet the"               
# [49] "this is meant to"                   
# [50] "hes al gore and"                    
# [51] "isnt paying attention to"           
# [52] "of flytraps key players"            
# [53] "views about how many"               
# [54] "by the cold war"                    
# [55] "widely believed i am"               
# [56] "its usat reefer the"                
# [57] "of they were in"                    
# [58] "nor that friday night"              
# [59] "with it white house"                
# [60] "a child born to"                    
# [61] "provided spiritual counseling to"   
# [62] "juenger a world war"                
# [63] "discuss the meaning of"             
# [64] "a massive conflict the"             
# [65] "magazines radio stations and"       
# [66] "i dont think they"                  
# [67] "warren beatty would be"             
# [68] "both were forced to"                
# [69] "prejudices youll be able"                         # extra found          
# [70] "jeans real choice and"              
# [71] "brewed faster warmer and"           
# [72] "sick of flytrap and"                
# [73] "a imprudently trimmed the"          
# [74] "religion and of course"                           # extra found
# [75] "for other governments to"           
# [76] "women like jones and"               
# [77] "cash flow toward the"               
# [78] "is their inability to"              
# [79] "its head within the"                
# [80] "security unlike activex and"        
# [81] "indices filed last week"            
# [82] "much as berio and"                  
# [83] "to previous prefromc the" 

# results.vec_guess.sb with alpha 0.8 using scoresDB$one.pct
# [1] "a renoir show to"                   
# [2] "to decide relation to"              
# [3] "tuesday feb york times"             
# [4] "eyebrow when yet another"           
# [5] "the spin rather than"               
# [6] "on satellite television and"        
# [7] "to combat terrorism the"            
# [8] "george mcgovern said he"            
# [9] "hospitals avoid teaching the"       
# [10] "in american dollars and"            
# [11] "courted patriot movement and"       
# [12] "useful to say about"                
# [13] "gore did not know"                  
# [14] "benefits for employees and"         
# [15] "plodding but hard to"               
# [16] "in other words the"                 
# [17] "what the academics and"             
# [18] "its more interesting to"            
# [19] "impeachment proceedings against the"
# [20] "his subjects after the"             
# [21] "is whats killing the"               
# [22] "he delivers goodies to"             
# [23] "answer that matters and"            
# [24] "place is not an"                    
# [25] "money and people to"                
# [26] "go away far away"                   
# [27] "assistant whos doing the"           
# [28] "adjusted his style to"              
# [29] "girder holding him to"              
# [30] "to let you know"                    
# [31] "to a cabinet to"                    
# [32] "to rent listings and"               
# [33] "why im going to"                    
# [34] "on do anyone else"                  
# [35] "reform plan three years"            
# [36] "casting himself eye to"             
# [37] "the bosnia where the"               
# [38] "policy nuances tend to"             
# [39] "si bon croons the"                  
# [40] "when people scrutinize and"         
# [41] "acting out ought to"                
# [42] "desist and let the"                 
# [43] "to various presidents and"          
# [44] "differ satanic pornographers and"   
# [45] "stripped of clutter and"            
# [46] "a happy man to"                     
# [47] "appeared on meet the"               
# [48] "this is meant to"                   
# [49] "hes al gore and"                    
# [50] "isnt paying attention to"           
# [51] "of flytraps key players"            
# [52] "views about how many"               
# [53] "by the cold war"                    
# [54] "widely believed i am"               
# [55] "its usat reefer the"                
# [56] "of they were in"                    
# [57] "nor that friday night"              
# [58] "with it white house"                
# [59] "a child born to"                    
# [60] "provided spiritual counseling to"   
# [61] "juenger a world war"                
# [62] "discuss the meaning of"             
# [63] "a massive conflict the"             
# [64] "magazines radio stations and"       
# [65] "i dont think they"                  
# [66] "warren beatty would be"             
# [67] "both were forced to"                
# [68] "jeans real choice and"              
# [69] "brewed faster warmer and"           
# [70] "sick of flytrap and"                
# [71] "a imprudently trimmed the"          
# [72] "for other governments to"           
# [73] "women like jones and"               
# [74] "cash flow toward the"               
# [75] "is their inability to"              
# [76] "its head within the"                
# [77] "security unlike activex and"        
# [78] "indices filed last week"            
# [79] "much as berio and"                  
# [80] "to previous prefromc the"           

# Evaluation of issue:
# > head(vec.scores)
# i guess i suppose    i hate  i forgot     i bet    i wish 
# 0.4058537 0.3873684 0.3429665 0.3352381 0.3235955 0.3024473 
# > head(simple.scores)
# i am   i guess i suppose    i hate  i forgot     i bet 
# 0.4274389 0.4058537 0.3873684 0.3429665 0.3352381 0.3235955 
