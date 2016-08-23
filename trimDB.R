# Capstone Project
# file trimDB.R
# Takes the scores database (with associated ngrams and bases) and trims it
# Strategy 1: Drops cases where ngram frequency is 1 (except for unigrams)
#     This generate the short list
# Strategy 2: Keeps the 3 top scoring ngrams that have the same
#             (n-1)gram at a begining
#     This generates the minimal list
# Strategy 3: keep unigrams that cover 90% of the corpus. NOT GOOD COMMENTED OUT

# Change directory to location of project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")

setwd(prj.dir)
source("nlpTools.R")

# Change directory to corpus directory
download.dir <- "nlpData.dir"; sub.dir <- "final"; proc.corpus.dir <- "proc"
proc.corpus.dir <- file.path(prj.dir,download.dir,sub.dir,proc.corpus.dir)

setwd(proc.corpus.dir)

# Change directory to version of database we want
try.dir <- file.path(proc.corpus.dir,"try_02")  # doing second try
setwd(try.dir)

print(paste("Current directory: ",getwd()))

# Prunning database
# Start with the un-edited database.
#    freq.db, scores.db, ngrams.db, and bases.db are lists with each
#    element containing the information generated from a given sample of the corpus
#
# STRATEGY -1 - keep all ngrams that have n > cut value
#               cut = c(1,3,7,10,20)
load("dbfreq.r")
freqDB <- freq.db[[1]] # take the first sample

load("dbScores.r")
scoresDB <- scores.db[[1]] # take the first sample from full score
basesDB <- bases.db[[1]]
ngramsDB <- ngrams.db[[1]]

buildShortDB <- function(scores,ngrams,bases,freq,cut.levels=c(1)){
  short.dbs <- list()
  for(i in cut.levels){
    
    c.scores <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
    c.ngrams <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
    c.bases  <- list(bigram=NULL,trigram=NULL,quadgram=NULL)
    
    c.scores[2:4] <- mapply(cut.score,scores[2:4],freq[2:4],
                            MoreArgs = list(cut.level=i))
    c.scores$unigram <- sort(scoresDB$unigram,decreasing=TRUE) # scoresDB should be sorted
    c.ngrams <- lapply(c.scores,names)
    c.bases <- lapply(c.ngrams[2:4],function(x) unlist(dropLastWord(x)))
    db <- list(scores=c.scores,ngrams=c.ngrams,bases=c.bases)
    short.dbs[[as.character(i)]] <- db
  }
  return(short.dbs)
}

short.dbs <- buildShortDB(scoresDB,ngramsDB,basesDB,freqDB,c(1,3,7,10,20))

if(file.exists("dbShortDBS.r")){
  file.remove(("dbShortDBS.r"))
}
save(short.dbs,file="dbShortDBS.r")

### END STRATEGY 1 #####

# STRATEGY -2:
# keep only top scoring p ngrams that start with the same n-1 words
# Start from the short database
load("dbShortScores.r")

# Notice we do not have to pick a sample. Once dbShortScores.r we have chosen!

# selection function
minScores <- function(scores,bases,pMax=3){
  m.scores <- c()
  bases.with.dup <- bases
  scores.with.dup <- scores
  for(i in seq(1,pMax)){
    # works because duplicated returns FALSE for first instance of duplicated entry
    is.dup <- duplicated(bases.with.dup)
    m.scores <- c(m.scores,scores.with.dup[!is.dup])
    scores.with.dup <- scores.with.dup[is.dup]
    bases.with.dup <- bases.with.dup[is.dup]
  }
  return(sort(m.scores,decreasing=TRUE))
}

m.scoresDB <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
m.ngramsDB <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
m.basesDB <- list(bigram=NULL,trigram=NULL,quadgram=NULL)

for(i in seq(2,4)){
  m.scoresDB[[i]] <- minScores(s.scoresDB[[i]],s.basesDB[[(i-1)]]) # is sorted
  m.ngramsDB[[i]] <- names(m.scoresDB[[i]])
  m.basesDB[[(i-1)]] <- unlist(dropLastWord(m.ngramsDB[[i]]))
}
m.scoresDB[[1]] <- s.scoresDB[[1]] # scoresDB should be sorted
m.ngramsDB[[1]] <- names(s.scoresDB[[1]])

if(file.exists("dbMinScores.r")){
  file.remove(("dbMinScores.r"))
}
save(m.scoresDB,m.ngramsDB,m.basesDB,file="dbMinScores.r")
### END STRATEGY 2 #####

# # pruning database for speed and reduced memory
# # STRATEGY- 3 - keep unigrams that cover 90% of corpus unigrams, and associated ngrams.
# s.ngrams.db$unigram <- names(freq.db$unigram[1:8000])
# ngramInDict <- function(words,dict){
#   words <- words[[1]]
#   for(w in words){
#     if(!(w %in% dict)) return(FALSE)
#   }
#   return(TRUE)
# }
# selectToKeep <- function(ngrams){
#   asWords <- toWords(ngrams) # returns a list, one entry per ngram
#   sapply(asWords,ngramInDict,s.ngrams.db$unigram) # returns Boolean
# }

# pruning the dictionary is not the best strategy. This keeps too many higher order
# ngrams. For example, for quadgrams it kept 979,657 out of 1,118,610 or 88%! This
# is not surprising because the dictionary includes enough words to account for
# 90% of the corpus!
# END STRATEGY 3

#### Strategy 4 - Do pruning in DTM instead
####
load("dbdtms.r")
library(tm)
dtmDB <- dtms[[1]] # take the first sample

#sparsity
# unigram 50%, bigram 60%, trigram 64%, quadgram 66%

dtmDB.sp33 <- list()
dtmDB.sp33$unigram <- dtmDB$unigram
dtmDB.sp33[2:4] <- lapply(dtmDB[2:4],removeSparseTerms,0.33)
names(dtmDB.sp33) <- c("unigram","bigram","trigram","quadgram")

freqDB <- dtm2freq(dtmDB.sp33)
scoresDB <- score.sbackoff(freqDB)
ngramsDB <- lapply(scoresDB,names)
basesDB <- lapply(ngramsDB[2:4],function(x) unlist(dropLastWord(x)))

freq.db.sp33 <- list(freqDB)
scores.db.sp33 <- list(scoresDB)
ngrams.db.sp33 <- list(ngramsDB)
bases.db.sp33 <- list(basesDB)

save(freq.db.sp33,file="dbfreq_Sp33.r")
save(scores.db.sp33,ngrams.db.sp33,bases.db.sp33,file="dbSp33Scores.r")

############### Looking at the frequencies ####
load("dbfreq.r")
ffDB<- lapply(freq.db[[1]],freq2ff)
lapply(ffDB,head,n=5)
