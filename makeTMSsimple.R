# Capstone Project
# File: makeTMSsimple.R
#   Builds the document text matrices (DTMS) with small memory requirement
#   Do this for each n-gram at a time. Save each time you build one DTM

print("Started script: makeTMSsimple.R")
# Got to project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

library(tm)
print("Loaded tm library.")

#Switch to processed corpus directory
source("toProcCorpusDir.R")
print(paste("Switched to diretory",getwd()))

#Select corpus directory. THIS IS REQUIRED
corpus.dir <- c("../100.dir")
print(paste("Going to directory containing corpus: ",corpus.dir))
setwd(corpus.dir)

# Loading corpus
load("corpus.r")
corps <- paste(dir(pattern="corpus"))
print(paste("Loaded corpus: ", corps))

print("releasing non clean corpus.")
rm(corpus)

buildDTM <- function(corpus,min.ngram=1,max.ngram=4){
  tokenizers = list()
  makeTokenizer <- function(n) {
    n;
    function(x){
      unlist(lapply(ngrams(words(x), n), paste,
                    collapse = " "),use.names = FALSE)
    }
  }
  for(i in seq(min.ngram,max.ngram)){
    tokenizers[[i]] <- makeTokenizer(i)
  }
  return(
    lapply(tokenizers,
           function(tk) {
             DocumentTermMatrix(corpus,control=list(tokenize = tk,
                                                    wordLengths=c(1,Inf)))
           }
    )
  )
}

print("***Generating Document Text Matrices***")
print("Generating DTMS for clean corpus.")
n.grams.names <- c("unigram","bigram","trigram","quadgram")
for(n.choice in seq(1,4)){
  dtms <- lapply(clean.corpus,buildDTM,min.ngram=n.choice,max.ngram=n.choice)
  for(k in seq_along(clean.corpus)){
    names(dtms[[k]])<- n.grams.names[n.choice]
  }
  
  out.file <- paste("dtms_",n.choice,".r",sep="")
  print(paste("Saving DTMS for",n.grams.names[n.choice]))
  if(file.exists(out.file)){
    file.remove(out.file)
  }
  save(dtms,file=out.file)
  print(paste("Finished saving DTMS in file",out.file))
  print("Releasing memory...")
  rm(dtms)
  print("Memory free!")
}

print("Completed makeTMSsimple.R")

print("Resetting to project directory.")
setwd(prj.dir)


