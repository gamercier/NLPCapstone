# Capstone Project
# file: buildFreqDB.R
#   Takes the dtm matrices computed using makeDTM.R and saved in dbdtm.r
#   to compute the ngram frequencies. Saves these in dbfreq.rresults from doing text analysis with t

print ("Starting ngramFreq.R...")

# Go to project directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory:",getwd()))

source("helpers.R")
load("dbdtms.r")

dtms <- list(unigram  = dtm.unigram,
             bigram   = dtm.bigram,
             trigram  = dtm.trigram,
             quadgram = dtm.quadgram)
freq <- list(unigram  = NULL,
              bigram   = NULL,
              trigram  = NULL,
              quadgram = NULL)

print("Computing frequencies and sorting in decreasing order...")
for(dtm in names(dtms)){
  freq[[dtm]] <- sort(colSums(as.matrix(dtms[[dtm]])),decreasing=TRUE)
}

print("Saving frequencies...")
if(file.exists("dbfreq.r")){
  file.remove("dbfreq.r")
}
# sort each ngram freq vector: highest frequency comes first (decreasing)
save(freq,file="dbfreq.r")

print("Finished ngramFreq.R!")