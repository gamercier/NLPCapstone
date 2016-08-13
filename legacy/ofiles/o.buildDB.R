# Capstone Project
# Generate database for model
# buildDB.R

# LOADING PACKAGES AND FREQUENCY NGRAM DATA FROM TM processing
# Go to project directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory:",getwd()))

print("Loading tools and helper functions...")
source("helpers.R")
print("Finish loading tools and helper functions!")
print("Loading frequencies from DTM")
load("freq.r")
print("Finished loading!")

#### NOTE ####
# Structure of object freq loaded with freq.r.
#        freq is a list with
#          each member a list (these sublists are labeled as ngram)
#          within these lists there is a list (a subsublist) with one member.
#             The member in the subsublist is a vector
#             with each item being a ngram.
MAX_NGRAM = length(freq) # maximum N in NGRAMs

# PREPROCESSING
#    SORT most frequent ngram within each N goes first
freq.sorted <- lapply(freq,function(x) sort(x,decreasing=TRUE) )

#    COLLECT sorted ngrams
ngrams.sorted <- list(unigram  = names(freq.sorted$unigram),
                      bigram   = names(freq.sorted$bigram),
                      trigram  = names(freq.sorted$trigram),
                      quadgram = names(freq.sorted$quadgram))
# for speed
ngrams.base = list(bigram = unlist(dropLastWord(ngrams.sorted$bigram)),
                   trigram  = unlist(dropLastWord(ngrams.sorted$trigram)),
                   quadgram = unlist(dropLastWord(ngrams.sorted$quadgram)))

#    GET number of ngrams for each N, as a list, (includes repetitions)
N.ngram <- lapply(freq.sorted,sum)
#    GET number of ngrams for each N, as a list, (no repetitions)
unique.N.ngram <- lapply(ngrams.sorted,length)

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

# LINK TO DATABASE HOOK
freq.db <- short.freq
ngrams.db <- short.ngrams
base.db <- short.ngrams.base
N.db <- N.ngram

print("Saving short database to *dbShort.R*")
# save(freq.db,ngrams.db,base.db,N.db,
#      short.freq,short.ngrams,short.ngrams.base,N.ngram,
#      file="db.R")
save(freq.db,ngrams.db,base.db,N.db,
     file="dbShort.r")
print("Database saved!")

freq.db <- freq.sorted
ngrams.db <- ngrams.sorted
base.db <- ngrams.base
N.db <- N.ngram

print("Saving long database to *db.R*")
# save(freq.db,ngrams.db,base.db,N.db,
#      freq.sorted,ngrams.sorted,ngrams.base,N.ngram,
#      file="db.R")
save(freq.db,ngrams.db,base.db,N.db,
     file="db.r")
print("Database saved!")