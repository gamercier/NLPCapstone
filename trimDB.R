# Capstone Project
# file trimDB.R
# Takes the scores database (with associated ngrams and bases) and trims it
# Strategy 1: Drops cases where ngram frequency is 1 (except for unigrams)
#     This generate the short list
# Strategy 2: Keeps the 3 top scoring ngrams that have the same
#             (n-1)gram at a begining
#     This generates the minimal list

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
dropLastWord <- function(ngram){
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n and n > 1
  toStr(lapply(words,function(s) s[1:(n-1)]))
}

# Prunning database
# Start with the un-edited database.
#    freq.db, scores.db, ngrams.db, and bases.db are lists with each
#    element containing the information generated from a given sample of the corpus
#
# STRATEGY -1 - keep all ngrams witn n > that have freq > 1
load("dbfreq.r")
load("dbScores.r")

# pick a sample
freqDB <- freq.db[[1]] # sorted already

scoresDB <- scores.db[[1]] # picking the first sample
basesDB <- bases.db[[1]]
ngramsDB <- ngrams.db[[1]]

# selection function
selectToKeep <- function(freq,cutoff=1){
  return(names(freq[freq > cutoff]))
}
toKeep <- lapply(freqDB,selectToKeep)

s.scoresDB <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
s.ngramsDB <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
s.basesDB <- list(bigram=NULL,trigram=NULL,quadgram=NULL)

# notice the need to re-sort the data
for(i in seq(2,4)){
  s.scoresDB[[i]] <- sort(scoresDB[[i]][toKeep[[i]]],decreasing=TRUE)
  s.ngramsDB[[i]] <- names(s.scoresDB[[i]])
  s.basesDB[[(i-1)]] <- unlist(dropLastWord(s.ngramsDB[[i]]))
}
s.scoresDB[[1]] <- sort(scoresDB[[1]],decreasing=TRUE) # scoresDB should be sorted
s.ngramsDB[[1]] <- names(s.scoresDB[[1]])

if(file.exists("dbShortScores.r")){
  file.remove(("dbShortScores.r"))
}
save(s.scoresDB,s.ngramsDB,s.basesDB,file="dbShortScores.r")
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
