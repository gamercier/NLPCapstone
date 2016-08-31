# Capstone Project
# File: testGuess.R
# Tests guessing function against a database of quadgrams in quad.r

# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")

setwd(prj.dir)
getwd()
print("Loading tools!")
source("nlpTools.R")

print("Data for testing!")
# Load data for testing
if(file.exists(file.path(prj.dir,"test_quads.r"))){
  load(file.path(prj.dir,"test_quads.r"))
  print("Loaded quadgrams for testing.")
} else {
  load(file.path(prj.dir,"quads.r"))
  n.sample <- 1000
  test.sample <- quads[sample.int(length(quads),n.sample)]
  save(test.sample,file=file.path(prj.dir,"test_quads.r"))
  print("Built and saved new set of quadgrams for testing")
}

# MAIN FUNCTION
guess.sb <- function(trigram,scores=scoresDB,ngrams=ngramsDB,bases=basesDB,alpha=0.4){
  ngram <- trigram
  n <- 3
  hits <- c((alpha^3)*TOPUNI.SCORES)
  while(n > 0) {
    if(ngram %in% ngrams[[n]]){
      hits <-
        c(hits,((alpha)^(3-n))*scores[[(n+1)]]
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

# automating testing
testing <- function(test.s,a=0.4){
  hit <- function(x){
    guesses <- guess.sb(unlist(dropLastWord(x)),alpha=a)$guess
    if(unlist(getLastWord(x)) %in% guesses){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  test.f <- sapply(test.s,hit)
  return(test.f)
}

# Change directory to corpus directory
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)
setwd(proc.corpus.dir)
getwd()

################ try_01 database testing ####################
# Change directory to version of database we want
try.dir <- file.path(proc.corpus.dir,"try_01")  # doing second try
setwd(try.dir)
print("try_01")
getwd()

#######  Using minimal database
print("Loading minimal score database ...")
load("dbMinScores.r")
print("Finished loading short scores database!")

# The strategy 1 and 2 edited, minimal databases:
#     m.scoresDB, m.ngramsDB ,and m.basesDB
#     only apply to a single sample of the corpus.
scoresDB <- m.scoresDB; TOPUNI.SCORES <- m.scoresDB$unigram[1:3]
ngramsDB <- m.ngramsDB
basesDB <- m.basesDB

results.minDB <- testing(test.sample)
print(paste("Hits: ",sum(results.minDB)," out of ",length(results.minDB)))

######  Using short database
print("Loading short score database ...")
load("dbShortScores.r")
print("Finished loading short scores database!")

# The strategy 1 edited, short databases:
#     s.scoresDB, s.ngramsDB ,and s.basesDB
#     only apply to a single sample of the corpus.
scoresDB <- s.scoresDB; TOPUNI.SCORES <- s.scoresDB$unigram[1:3]
ngramsDB <- s.ngramsDB
basesDB <- s.basesDB

results.shortDB <- testing(test.sample)
print(paste("Hits: ",sum(results.shortDB)," out of ",length(results.shortDB)))

########## Using full database
print("Loading full score database ...")
load(file.path("try_01","dbScores.r"))
print("Finished loading full scores database!")

# The unedited full databases:
#     scores.db, ngrams.db ,and bases.db
#     only apply to a single sample of the corpus.
scoresDB <- scores.db[[1]]; TOPUNI.SCORES <- scores.db[[1]]$unigram[1:3]
ngramsDB <- ngrams.db[[1]]
basesDB <- bases.db[[1]]

results.fullDB <- testing(test.sample)
print(paste("Hits: ",sum(results.fullDB)," out of ",length(results.fullDB)))

# With try_01 database and test_quads.r we get the following results:
# Minimal database: 62/1000 short database but keep top 3 scores.
# Short database:   87/1000 cuts off frequency of 1 in bigram and above
# Full database:    44/1000 no cuts 
################ END OF  try_01 database testing ####################
#

################ try_02 database testing ####################
# Change directory to version of database we want
try.dir <- file.path(proc.corpus.dir,"try_02")  # doing second try
setwd(try.dir)
print("try_02")
getwd()

######  Using minimal database
print("Loading minimal score database ...")
load("dbMinScores.r")
print("Finished loading short scores database!")

# The strategy 1 and 2 edited, minimal databases:
#     m.scoresDB, m.ngramsDB ,and m.basesDB
#     only apply to a single sample of the corpus.
scoresDB <- m.scoresDB; TOPUNI.SCORES <- m.scoresDB$unigram[1:3]
ngramsDB <- m.ngramsDB
basesDB <- m.basesDB

results.minDB <- testing(test.sample)
print(paste("Hits: ",sum(results.minDB)," out of ",length(results.minDB)))

#######  Using short database; cut off of 1
print("Loading short score database ...")
load("dbShortScores.r")
print("Finished loading short scores database!")

# The strategy 1 edited, short databases:
#     s.scoresDB, s.ngramsDB ,and s.basesDB
#     only apply to a single sample of the corpus.
scoresDB <- s.scoresDB; TOPUNI.SCORES <- s.scoresDB$unigram[1:3]
ngramsDB <- s.ngramsDB
basesDB <- s.basesDB

results.shortDB <- testing(test.sample)
print(paste("Hits: ",sum(results.shortDB)," out of ",length(results.shortDB)))

########  Using full database
print("Loading full score database ...")
load(file.path("try_02","dbScores.r"))
print("Finished loading full scores database!")

# The unedited full databases:
#     scores.db, ngrams.db ,and bases.db
#     only apply to a single sample of the corpus.
scoresDB <- scores.db[[1]]; TOPUNI.SCORES <- scores.db[[1]]$unigram[1:3]
ngramsDB <- ngrams.db[[1]]
basesDB <- bases.db[[1]]

results.fullDB <- testing(test.sample)
print(paste("Hits: ",sum(results.fullDB)," out of ",length(results.fullDB)))

######## Testing with different cut offs
# Here we load dbShortDBS.r
print("Loading dbShortDBS.r database ...")
load("dbShortDBS.r") # loads variable short.dbs
# This is a list.
# One entry per cut off value.
#    Each entry is a list(scores,ngrams,bases)
#
print("Finished loading dbShortdBS databases, dbShortDBS.r!")

cuts <- names(short.dbs)
for(k in cuts){
  scoresDB <- short.dbs[[k]]$scores; TOPUNI.SCORES <- scoresDB$unigram[1:3]
  ngramsDB <- short.dbs[[k]]$ngrams
  basesDB <- short.dbs[[k]]$bases
  
  results <- testing(test.sample)
  print(paste("Testing with cut off: ",k))
  print(paste("Hits: ",sum(results)," out of ",length(results)))
}

# With try_02 database and test_quads.r we get the following results:
# Minimal database:       62/1000 short database but keep top 3 scores.
# Short database cut 20:  93/1000
# Short database cut 10:  98/1000
# Short database cut  7: 104/1000
# Short database cut  3: 102/1000
# Short database cut  1: 85/1000 cuts off frequency of 1 in bigram and above
# Full database:         47/1000 no cuts

# So using a cut off 7 is pretty good with a 5% sampling of the database.

##### Testing with sparse database Sp33
# Here we load dbSp33Scores.r
print("Loading dbSp33Scores.r database ...")
load("dbSp33Scores.r") # loads variable short.dbs
print("Finished loading dbSp33Scores databases!")

scoresDB <- scores.db.sp33[[1]] ; TOPUNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.db.sp33[[1]]
basesDB <- bases.db.sp33[[1]]

results <- testing(test.sample)
print(paste("Hits: ",sum(results)," out of ",length(results)))

# With Sparsity of 33% got 104/1000; using sparsity 33% resulted in sparsity of 0%
# for all except the unigram, which had about 50% sparsity

# testing with variable alpha parameter in the guess function.
alphas <- c(0.0)
for(alpha in alphas){
  results <- testing(test.sample,a=alpha)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",alpha))
}

# "Hits:  91  out of  1000 alpha= 0"
# "Hits:  107  out of  1000 alpha= 0.05"
# "Hits:  105  out of  1000 alpha= 0.1"
# "Hits:  104  out of  1000 alpha= 0.15"
# "Hits:  105  out of  1000 alpha= 0.2"
# "Hits:  105  out of  1000 alpha= 0.3"
# "Hits:  104  out of  1000 alpha= 0.4"
# "Hits:  102  out of  1000 alpha= 0.6"
# "Hits:  101  out of  1000 alpha= 0.9"
################ END OF  try_02 database testing ####################

################ try_03 database testing ####################
# Change directory to version of database we want
try.dir <- file.path(proc.corpus.dir,"try_03")  # doing second try
setwd(try.dir)
print("try_03")
getwd()

######## Testing with different cut offs
# Here we load dbShortDBS.r
print("Loading dbShortDBS.r database ...")
load("dbShortDBS.r") # loads variable short.dbs
# This is a list.
# One entry per cut off value.
#    Each entry is a list(scores,ngrams,bases)
#
print("Finished loading dbShortdBS databases, dbShortDBS.r!")

cuts <- names(short.dbs)
for(k in cuts){
  scoresDB <- short.dbs[[k]]$scores; TOPUNI.SCORES <- scoresDB$unigram[1:3]
  ngramsDB <- short.dbs[[k]]$ngrams
  basesDB <- short.dbs[[k]]$bases
  
  results <- testing(test.sample)
  print(paste("Testing with cut off: ",k))
  print(paste("Hits: ",sum(results)," out of ",length(results)))
}

# Using 10% sample of corpus the best cut off is 7
# "Testing with cut off:  1"
# "Hits:  94  out of  1000"
# "Testing with cut off:  3"
# "Hits:  98  out of  1000"
# "Testing with cut off:  7"
# "Hits:  106  out of  1000"
# "Testing with cut off:  10"
# "Hits:  98  out of  1000"
# "Testing with cut off:  20"
# "Hits:  97  out of  1000"

##### Testing with sparse database Sp33
# Here we load dbSp33Scores.r
print("Loading dbSp33Scores.r database ...")
load("dbSp33Scores.r") # loads variable short.dbs
print("Finished loading dbSp33Scores databases!")

scoresDB <- scores.db.sp33[[1]] ; TOPUNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.db.sp33[[1]]
basesDB <- bases.db.sp33[[1]]

results <- testing(test.sample)
print(paste("Hits: ",sum(results)," out of ",length(results)))

# "Hits:  106  out of  1000"

# testing with variable alpha parameter in the guess function.
alphas <- c(0.0,0.05,0.1,0.15,0.2,0.3,0.6,0.9)
for(alpha in alphas){
  results <- testing(test.sample,a=alpha)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",alpha))
}

# "Hits:  98  out of  1000 alpha= 0"
# "Hits:  115  out of  1000 alpha= 0.05"
# "Hits:  113  out of  1000 alpha= 0.1"
# "Hits:  112  out of  1000 alpha= 0.15"
# "Hits:  110  out of  1000 alpha= 0.2"
# "Hits:  108  out of  1000 alpha= 0.3"
# "Hits:  106  out of  1000 alpha= 0.4"
# "Hits:  108  out of  1000 alpha= 0.6"
# "Hits:  105  out of  1000 alpha= 0.9"

### Now doing with all DTMS with sparsity 33% (actually 0%)
# Here we load dbSp33Scores_All.r
print("Loading dbSp33Scores_All.r database ...")
load("dbSp33Scores_All.r") # loads variable short.dbs
print("Finished loading dbSp33Scores_All databases!")

scoresDB <- scores.db.sp33[[1]] ; TOPUNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.db.sp33[[1]]
basesDB <- bases.db.sp33[[1]]

results <- testing(test.sample,a=0.05)
print(paste("Hits: ",sum(results)," out of ",length(results)))

# "Hits:  106  out of  1000 alpha=0.4"
# "Hits:  115  out of  1000 alpha=0.05"

################ END OF  try_03 database testing ####################

################ try_04 database testing ####################
# Change directory to version of database we want
try.dir <- file.path(proc.corpus.dir,"try_04")  # doing second try
setwd(try.dir)
print("try_04")
getwd()

######## Testing with different cut offs
# Here we load dbShortDBS.r
print("Loading dbShortDBS.r database ...")
load("dbShortDBS.r") # loads variable short.dbs
# This is a list.
# One entry per cut off value.
#    Each entry is a list(scores,ngrams,bases)
#
print("Finished loading dbShortdBS databases, dbShortDBS.r!")

cuts <- names(short.dbs)
for(k in cuts){
  scoresDB <- short.dbs[[k]]$scores; TOPUNI.SCORES <- scoresDB$unigram[1:3]
  ngramsDB <- short.dbs[[k]]$ngrams
  basesDB <- short.dbs[[k]]$bases
  
  results <- testing(test.sample)
  print(paste("Testing with cut off: ",k))
  print(paste("Hits: ",sum(results)," out of ",length(results)))
}

# Using 10% sample of corpus the best cut off is 7
# "Testing with cut off:  1"
# "Hits:  94  out of  1000"
# "Testing with cut off:  3"
# "Hits:  98  out of  1000"
# "Testing with cut off:  7"
# "Hits:  106  out of  1000"
# "Testing with cut off:  10"
# "Hits:  98  out of  1000"
# "Testing with cut off:  20"
# "Hits:  97  out of  1000"

##### Testing with sparse database Sp33 - All

### Now doing with all DTMS with sparsity 33% (actually 0%)
# Here we load dbSp33Scores_All.r
print("Loading dbSp33Scores_All.r database ...")
load("dbSp33Scores_All.r") # loads variable short.dbs
print("Finished loading dbSp33Scores_All databases!")

scoresDB <- scores.db.sp33[[1]] ; TOPUNI.SCORES <- scoresDB$unigram[1:3]
ngramsDB <- ngrams.db.sp33[[1]]
basesDB <- bases.db.sp33[[1]]

alphas <- c(0.0,0.05,0.1,0.15,0.2,0.3,0.4,0.6,0.9)
for(alpha in alphas){
  results <- testing(test.sample,a=alpha)
  print(paste("Hits: ",sum(results)," out of ",length(results),"alpha=",alpha))
}

# "Hits:  112  out of  1000 alpha= 0"
# "Hits:  103  out of  1000 alpha= 0.05"
# "Hits:  102  out of  1000 alpha= 0.1"
# "Hits:  101  out of  1000 alpha= 0.15"
# "Hits:  100  out of  1000 alpha= 0.2"
# "Hits:  104  out of  1000 alpha= 0.3"
# "Hits:  99  out of  1000 alpha= 0.4"
# "Hits:  98  out of  1000 alpha= 0.6"
# "Hits:  101  out of  1000 alpha= 0.9"
################ END OF  try_04 database testing ####################
