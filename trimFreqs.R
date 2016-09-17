# Capstone Project
# File: trimFreqs.R
#   Trims the Frequency vector by dropping elements thate occur less than a user
#   defined threshold.
#   Scheme 1 - only one implemented
#      Selects a given cut off for the cumulative frequency of unigram.
#      Deletes unigrams that fall out of the 0-cum.freq.cut.off
#      Gets the maximum frequency of unigrams cut out and uses it as a hard
#      cut off to delete higher order unigrams that occur with lower frequency.

print("Started script: trimFreqs.R")
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

#Select freqs directory. THIS IS REQUIRED
freqs.dir <- c("../50.dir")
print(paste("Going to directory containing frequecy data: ",freqs.dir))
setwd(freqs.dir)
print(paste("Switched to diretory",getwd()))

#Select frequency file to load. THIS IS REQUIRED
freqs.file <- "freqs.dense.r"
print("Loading frequency data")
load(freqs.file)
print(paste("Loaded frequency data from",freqs.file))

#### FIX database labels ---- MUST DO MANUALLY NOT AS SOURCED SCRIPT!
####        THESE NAMES SHOULD BE A FIXED IN THE CREATION OF freqs.file!
names(freqs.dense.db) <- "fifty.pct"
names(ngrams.dense.db) <- "fifty.pct"
names(N.ngrams.dense.db) <- "fifty.pct"
names(V.ngrams.dense.db) <- "fifty.pct"

file.remove(freqs.file) # intention is to replace this one.

save(freqs.dense.db,
     ngrams.dense.db,
     bases.dense.db,
     N.ngrams.dense.db,
     V.ngrams.dense.db, file=freqs.file) # replacing.
#### FIX FINISHED

### ALL THIS PRESUMES THE FREQUENCY DATA IS SORTED, MOST FREQUENT FIRST!!!!!! ####
### IF USED THE makeFreqs.R script will do this automatically ####
print("****** WARNING: PRESUMES FREQUENCY DATA IS SORTED, MOST FREQUENT FIRST *****")

# set frequecies to trim. ########## THIS IS REQUIRED
db.element <- "fifty.pct"

freqs <- freqs.dense.db[[db.element]]   # freqs for one sample.
ngrams <- ngrams.dense.db[[db.element]] # ngrams for one sample, the ngrams with frequency in freqs
N.ngrams <- N.ngrams.dense.db[[db.element]]
V.ngrams <- V.ngrams.dense.db[[db.element]]

print("**** Statistics on the Frequency Data ****")
print("******************************************")

print("----- Totals -----")
total.ngrams <- as.vector(unlist(N.ngrams))
names(total.ngrams) <- names(N.ngrams)
voc.ngrams <- as.vector(unlist(V.ngrams.dense.db))
names(voc.ngrams) <- names(V.ngrams)
totals.df <- data.frame(n.voc=voc.ngrams,n.ngrams=total.ngrams)
print(totals.df)

for(ngram in names(freqs)){
  print(paste("Minimum frequency in ",ngram," is ",min(freqs[[ngram]])))
}

# print("Computing and Plotting Frequency of Frequencies!")
# ff <- list(unigram=NULL,bigram=NULL,trigram=NULL,quadgram=NULL)
# for(ngram in names(freqs)){
#   ff[[ngram]] <- freq2ff(freqs[[ngram]])
# }
# #plotting histograms
# for(ngram in names(ff)){
#   plot(ff[[ngram]][1:200,],main=paste("Frequency of Frequencies - ",ngram))
# }

print("Computing Fractional Cummulative Frequency")
frac.cum <- lapply(freqs, function(x) cumsum(x)/sum(x))

# for(ngram in names(frac.cum)){
#   plot(x=1:1000,y=frac.cum[[ngram]][1:1000],
#        main=paste("Fractional Cummulative Frequency - ",ngram),
#        xlab="index",ylab="cummulative frequency")
# }

##### REQUIRED: SELECT SCHEME TO USE #### ONLY ONE ON AT A TIME #####
scheme1 <- TRUE
scheme2 <- FALSE

#### SELECT OUTPUT FILE NAME ### REQUIRED
save.file <- "freqs.trimmed.dense.r"

############################ scheme one:
#   keep words that make up 99% of the total number of unigrams = dict.trimmed
#   Find the maximum count (frequency) seen in the words that are dropped.
#   Drop all bigrams, trigrams, and quadgrams with frequencies that are less than
#   that count.

findIndex <- function(freq.unigram, pct=0.99){
  # at and below this index you have 0.9 of the total number of unigrams
  fracCum.unigram <- cumsum(freq.unigram)/sum(freq.unigram)
  return(sum((fracCum.unigram <= pct)))
}
divideWords <- function(freq.unigram, pct=0.99){
  idx <- findIndex(freq.unigram,pct)
  words <- names(freq.unigram)
  return(list(words.to.keep=words[1:idx],
              words.to.cut =words[(idx+1):length(words)],index=idx))
}
trimFreqV <- function(freqV,cut.level=7){  # acts on freq vector for a unigram
  return(freqV[freqV > cut.level])
}

if(scheme1 == TRUE){
  print("Trimming using scheme one")
  pct.to.keep <- 0.99 #### SELECT percentage of unigrams to keep
  word.partition <- divideWords(freqs$unigram,pct.to.keep)
  print(paste("Index for word (unigram) covering",pct.to.keep*100,"% is",
              word.partition$index))
  print(paste("Confirmation from fractional cummulative frequency:",
              round(frac.cum$unigram[word.partition$index],3)*100,"%"))
  cut.index <- word.partition$index
  bad.words <- word.partition$words.to.cut
  bad.word.max.count <- max(freqs$unigram[bad.words])
  
  print(paste("Maximum frequency of words to cut is",bad.word.max.count ,"."))
  print("Dropping all ngrams with frequency equal or less than this value.")
  # notice the maximum frequency in the bad words (words to cut)
  # can be the same as the minimum frequency of word to keep.
  # This is because numerous words may have the same frequency, and we
  # pick the cut off point to reach certain cummulative frequency. Which words
  # are kept depends on the way the sort function works in R.
  
  freqs.trimmed <- vector(mode="list",length=4)
  names(freqs.trimmed) <- names(freqs)
  freqs.trimmed$unigram <- freqs$unigram[1:word.partition$index]
  for(n in seq(2,length(freqs.trimmed))){ # now do bigrams and up
    freqs.trimmed[[n]] <- freqs[[n]][freqs[[n]] > bad.word.max.count ]
  }
  
  # build a database
  ngrams.trimmed <- lapply(freqs.trimmed,names)
  bases.trimmed <- vector(mode="list",length=3)
  bases.trimmed <- lapply(ngrams.trimmed[2:length(ngrams.trimmed)],
                          function(x) unlist(dropLastWord(x)))
  N.ngrams.trimmed <- lapply(freqs.trimmed,sum)
  V.ngrams.trimmed <- lapply(ngrams.trimmed,length)
  
  # checking that all words in the bi,tri, and quadgrams are in the vocabulary
  # generated by the unigrams.
  
  quad.words <- unique(unlist(toWords(ngrams.trimmed$quadgram)))
  tri.words <- unique(unlist(toWords(ngrams.trimmed$trigram)))
  bi.words <- unique(unlist(toWords(ngrams.trimmed$bigram)))
  
  no.bad.quads.words <- sum(!(quad.words %in% ngrams.trimmed$unigram))
  no.bad.tris.words <- sum(!(tri.words %in% ngrams.trimmed$unigram))
  no.bad.bis.words <- sum(!(bi.words %in% ngrams.trimmed$unigram))
  
  print(paste("The number of missing words in ngram=",c(2,3,4),"is",
              c(no.bad.bis.words,no.bad.tris.words,no.bad.quads.words)))
  
  ### must check this change in definition of dbs - made 2016-09-17
  freqs.trimmed.dense.db    <- list()
  ngrams.trimmed.dense.db   <- list()
  bases.trimmed.dense.db    <- list()
  N.ngrams.trimmed.dense.db <- list()
  V.ngrams.trimmed.dense.db <- list()
  
  freqs.trimmed.dense.db[[db.element]]    <- freqs.trimmed
  ngrams.trimmed.dense.db[[db.element]]   <- ngrams.trimmed
  bases.trimmed.dense.db[[db.element]]    <- bases.trimmed
  N.ngrams.trimmed.dense.db[[db.element]] <- N.ngrams.trimmed
  V.ngrams.trimmed.dense.db[[db.element]] <- V.ngrams.trimmed
  
  print("Saving frequencies, etc...")
  if(file.exists(save.file)){
    file.remove(save.file)
  }
  
  print(paste("Saving trimmed frequency database in file ",save.file))
  
  save(freqs.trimmed.dense.db,
       ngrams.trimmed.dense.db,
       bases.trimmed.dense.db,
       N.ngrams.trimmed.dense.db,
       V.ngrams.trimmed.dense.db, file=save.file)
  
  print("Finished trimming and saving database with scheme one.")
}

if(scheme2 == TRUE) print("Scheme 2 is not active. Sorry...")

print("Returning to top directory")
setwd(prj.dir)
print(getwd())


################ BELOW IS NOT TESTED!
#### We can  refine the scheme one by actually dropping all
#### bigrams, trigrams, and quadgram that still have words in the bad word
#### list. This is INSTEAD of USING THE HARD CUT FOUND ABOVE.

# partNgrams <- function(freq.ngram,bad.words){
#   ngrams <- names(freq.ngram)
#   pat <- function(ngrams){ # "\\b(word)\\b|\\b(word)| ... |(word)\\b"
#     right.end <- paste(ngrams,"\\b",sep="")
#     both.ends <- paste("\\b",right.end,sep="")
#     return(str_replace(both.ends," ","\\b|\\b"))
#   }
#   bywords <- toWords(ngrams)
#   hit <- function(words,bad.words){
#     for(word in words){
#       if(word %in% bad.words) return(TRUE)
#     }
#     return(FALSE)
#   }
#   to.cut <- sapply(bywords.pat,grepl,bad.words)
#   return(list(to.keep=ngrams[!to.cut],to.cut=ngrams[to.cut]))
# }
# 
# #Developing function to retain ngrams with only good words
# #  Check if it has bad words. If so cut it. Otherwise, keep it.
# 
# ngramsToDrop <- function(ngrams,bad.words){
#   # pat <- paste(bad.words,collapse="|")
#   bad.sets <- slice(bad.words,1000)
#   bad.pats <- sapply(bad.sets,function(x) paste(x,collapse="|"))
#   answers <- lapply(bad.pats,function(x) str_detect(ngrams,x))
#   t <- as.matrix(answers)
#   r <- colSums(t)
#   return(as.logical(r))
#   answers <- lapply(bad.words,function(x) str_detect(ngrams,x))
#   
#   return(answer)
# }
# 
# trimByDict <- function(freqsL,pct=0.99){
#   print("Using scheme 1 to trim frequency data.")
#   pct.to.keep <- 0.99
#   print(paste("The threshold is",pct.to.keep))
#   wrds.to.drop <- wordsToDrop(freqsL$unigram,pct=pct.to.keep)
#   print(paste("Vocabulary words with this frequency or less will be dropped:",
#               max(freqsL$unigram[wrds.to.drop])))
#   freqs.trimmed <- vector(mode="list",length(freqsL))
#   names(freqs.trimmed) <- names(freqsL)
#   for(ngram in names(freqs)){
#     freqs.trimmed[[ngram]] <-
#       freqs[[ngram]][ !ngramsToDrop(names(freqsL[[ngram]]),wrds.to.drop) ]
#   }
#   return(freqs.trimmed)
# }
# 
# # scheme two:
# #   Keep the whole vocabulary formed by the unigrams
# #   Cut quadgrams with frequency less that a cut.level
# 
# freq.quad.trimmed <- trimFreqV(freqs$quadgram,cut.levels=7)
# 
# 
