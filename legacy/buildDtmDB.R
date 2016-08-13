# Capstone Project
# File: buildDtmDB.R
#   Loads Corpus -
#     en_US/en_US.blogs_XX.txt,en_US.twitter_XX.txt,en_US.news_XX.txt
#   Cleans Corpus
#   Generates Document Term Matrix for Uni,Bi,Tri, and Quadgrams
#   Saves DTMS in dbdtms.r

print("Started script: buildDtmDB.R")
# Got to project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

source("helpers.R")
#Switch to corpus directory
source("toCorpusDir.R")

text.files <- dir(corpus.dir) # corpus.dir set in toCorpusDir.R
#print(paste("Corpus Directory Files: ",paste0(text.files,collapse=", ")))

# building corpus sample files with 2% of the original:
#    en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt
corpus <- list(); 
k = 1
corpus.files <- text.files[grep(sprintf("%02i",k),text.files)]
print(paste("Corpus files: ",paste0(corpus.files,collapse=", ")))

# for individual files must use URISource.
# Will use readLines reader uses UTF-8, default local encoding.
# Also load the files.
uri <- URISource(paste("file://",corpus.files,sep=""),encoding="UTF-8",mode='text')
corpus[[k]] <- VCorpus(uri,
  readerControl = list(reader=readPlain,language="en",load=TRUE))

# Structure of corpus:
# A list with one member, the corpus. This member has 3 lists (sublists), one per document.
# Each of the sublists has two elements: content and meta.
# For example, to access the text of document 2 use corpus[[k]][2]$content in our case
# k=1 because we have only one corpus source.
#
# Notice that for our data the entries in the content subsublist is a character vector where
# each element contains either phrases, full sentences, or sets of sentences/phrases.
corpus[[k]] <- clean.corpus(corpus[[k]])

# Unigram Document Term matrices
print("Generating DTM for unigrams.")
# for all words
dtm.unigram <- DocumentTermMatrix(corpus[[k]])

# for more descriptive words pick length from 5 to 12
dtm.unigram.512 <- DocumentTermMatrix(corpus[[k]],
                                  control=list(wordLengths=c(5,12)))

# bigram and trigram tokenizers
BigramTokenizer  <- function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)
TrigramTokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "),
         use.names = FALSE)

QuadgramTokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 4), paste, collapse = " "),
         use.names = FALSE)

# Bigram, Trigram, and Quadgram Document Term matrices
print("Generating DTM for bigrams.")
dtm.bigram  <- DocumentTermMatrix(corpus[[k]],
                  control=list(tokenize = BigramTokenizer))

print("Generating DTM for trigrams.")
dtm.trigram <- DocumentTermMatrix(corpus[[k]],
                  control=list(tokenize = TrigramTokenizer))

print("Generating DTM for quadgrams")
dtm.quadgram <- DocumentTermMatrix(corpus[[k]],
                  control=list(tokenize = QuadgramTokenizer))

print("Returning to main directory!")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

# Save the DTM's
print("Saving the dbdtms")
if(file.exists("dbdtms.r")){
  file.remove("dbdtms.r")
}
save(dtm.unigram.512,dtm.unigram,dtm.bigram,dtm.trigram,dtm.quadgram,
     file="dbdtms.r")
print("Completed buildDtmDB.R")
