# Capstone Project
# File: nlpTools.R
#   Functions useful for natural language processing and used
#   in building Document Text Matrix (DTM), freqDB (frequency database),
#   and scoreDB (scores for stupid backoff)

print("Started script: nlpTools.R")
# Got to project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

library(tm)
library(stringr)

print("Loaded libraries: tm, string_r")

# helper functions
toWords <- function(ngram) str_split(ngram,pattern=boundary("word"))
toStr <- function(words) lapply(words,str_c,collapse=" ")

dropLastWord <- function(ngram){
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n and n > 1
  toStr(lapply(words,function(s) s[1:(n-1)]))
}
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n
  toStr(lapply(words,function(s) s[2:n]))
}

# read text file - Needs this early
read.txt <- function(fname){
  con <- file(fname,open="r")
  txt <- readLines(con,skipNul=TRUE)
  close(con)
  return(txt)
}

helper.f <- c("toWords","toStr","dropFirstWord","dropLastWord","read.txt")
print(paste("Created helper functions: ",paste(helper.f)))

# global variables
# Twitter Slang adapted from
#  http://www.socialmediatoday.com/social-networks/sarah-snow/2015-07-08/
#       get-out-your-twittonary-twitter-abbreviations-you-must-know
TWITTERSLANG <- read.txt(file.path(prj.dir,"twitterSlang.txt"))
# Profanity from
# http://www.banned.wordlist.com
PROFANITY <- read.txt(file.path(prj.dir,"profanity.txt"))

# regexs
# source: adapted from http://rpubs.com/sgeletta/95577
URL.PAT <- "(file|ftp|http)(s?)://.*\\b"
# source: adapted from http://www.regular-expressions.info/email.html
MAIL.PAT <- "\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}\\b"
# source: my own after web review of multiple options
TWHASH.PAT <- "\\b#[a-zA-Z0-9]+\\b"
# my own after web review of multiple options
TWUSER.PAT <- "\\b@([a-zA-Z0-9_]{1,15})\\b"
# contractions
CNT.PAT <-"(?<=[a-zA-Z0-9])'(?=[a-zA-Z0-9])|(?<=[a-zA-Z0-9])\"(?=[a-zA-Z0-9])"
# bad characters.
BAD.PAT <- "[^[:alnum:][:punct:][:space:]]"

regex.p <- c("TWITTERSLANG","PROFANITY","URL.PAT",
             "MAIL.PAT","TWHASH.PAT","TWUSER.PAT","CNT.PAT","BAD.PAT")
print(paste("Created regex patterns: ",paste(regex.p)))

# global substitutions
toSpace <- function(x, pattern, ...){
  return(gsub(pattern," ", x, ...))
}
toNone <- function(x, pattern, ...){
  return(gsub(pattern,"", x, ...))
}

killProfane <- function(doc,profane.words){
  killInPara <- function(paragraph,profane.words){
    # Works on a paragraph, a one element character vector with sentences.
    sents <- unlist(str_split(paragraph,pattern="[.?!]"))
    isProfane <- function(sent){
      words <- unlist(str_split(sent,boundary("word")))
      for(w in words){
        if(w %in% profane.words) return(TRUE)
      }
      return(FALSE)
    }
    profane <- sapply(sents,isProfane)
    return(paste(sents[!profane],collapse="."))
  }
  # Works on a doc, a character vector with each element a paragraph
  return(unlist(lapply(doc,killInPara,profane.words)))
}

# Cleans the corpus. Different from legacy one in helpers.R
purify.corpus <- function(corpus,toASCII=FALSE,collapseContractions=FALSE,
                         removeStopWords=FALSE,stemWords=FALSE){
  #   Difference 1
  #   Assumes: text is US-ASCII and line terminations are fixed
  #     Can do this through OS X (with homebrew dos2unix)
  #     iconv --from-code=UTF-8 --to-code==US-ASCII input > output
  #     dos2unix -f -n inputfile output file
  #
  #   Difference 2
  #   Sets to lower case up front, not later
  #
  #   Difference 3
  #   Removes profanity - takes out the sentences with profanity, not word
  #
  #   Same except sets it to "", not blank space " "
  #   Removes special stuff:
  #       urls, emails, twitter hash tags, and user names
  #   Removes chat and twitter slang
  #
  #   Difference 4
  #   Does not collapse contractions like don't to dont
  #
  #   Same
  #   Removes Punctuation
  #   Removes Numbers
  #
  #   Difference 5
  #   Does not remove Stop words
  #
  #   Difference 6
  #   Does not do Stemming
  #
  #   Difference 7
  #   Strips whitespaces - done last
  
  # requires tm package
  # if(!require("tm")){ stop("Requires tm package!") }
  
  # helper functions
  toNone.tm <- content_transformer(toNone)
  killProfane.tm <- content_transformer(killProfane)
  tolower.tm <- content_transformer(tolower)
  
  # 1) Make ASCII. Already in US-ASCII with proper line termination.
  #                See result of checkRawData.R
  if(toASCII){
    print("Cleaning: Special characters")
    corpus <- tm_map(corpus,toSpace,BAD.PAT)
  }
  
  # 2) Set to lower case
  print("Going to lower case!")
  corpus <- tm_map(corpus,tolower.tm)
  
  # 3) Remove lines with profanity
  print("Killing profanity!")
  corpus <- tm_map(corpus,killProfane.tm, PROFANITY)
  
  # 2) Null url like patterns
  print("Nulling URL")
  corpus <- tm_map(corpus,toNone.tm, URL.PAT)
  
  # 3) Null email like patterns
  print("Nulling mail")
  corpus <- tm_map(corpus,toNone.tm, MAIL.PAT)
  
  # 4) Null twitter hash tags and user names
  print("Nulling twitter hash tags and user names")
  corpus <- tm_map(corpus,toNone.tm, TWHASH.PAT)
  corpus <- tm_map(corpus,toNone.tm, TWUSER.PAT)
  
  # 5) Null twitter slang
  print("Nulling twitter slang")
  corpus <- tm_map(corpus,removeWords,TWITTERSLANG)
  
  # 6) Collapse contractions - will not do for guess database
  if(collapseContractions){
    print("Cleaning: Collapsing contractions.")
    corpus <- tm_map(corpus,toNone.tm, CNT.PAT,perl=TRUE)
  }
  
  # 7) Remove punctuation and numbers
  print("Removing punctuation and numbers")
  corpus <- tm_map(corpus,removePunctuation) # may need to keep "[.!?]"
  corpus <- tm_map(corpus,removeNumbers)
  
  # 8) Remove stopwords - will not do for guess database
  if(removeStopWords){
    print("Cleaning: removing english stop words.")
    corpus <- tm_map(corpus,removeWords,stopwords("english"))
  }
  
  # 9) Remove white spaces
  print("Stripping whitespace")
  corpus <- tm_map(corpus,stripWhitespace)
  
  # 10) Stemming - will not do for guess database
  if(stemWords){
    corpus <- tm_map(corpus,stemDocument)
  }
  
  return(corpus)
}

helper.f2 <- c("toSpace","toNone","killProfane","purify.corpus")
print(paste("Created helper functions: ",paste(helper.f2)))

# sampleText <- function(text.files,n.samples=1,percent=0.05){
#   for(f in text.files){
#     # read entire file, sample lines, and output lines
#     print(paste("Reading file: ",f))
#     txt <- read.txt(f)
#     fparts <- unlist(strsplit(f,split="[.]")) # process file name for write
#     fbase <- paste0(fparts[1],".",fparts[2])
#     print(paste("Sampling ",round(percent*100),"%"," of the file."))
#     for(k in 1:(n.samples)){
#       txt.kept <- txt[sample.int(length(txt),round(percent*length(txt)))]
#       outf <- sprintf("%s_%02i.txt",fbase,k)
#       if(file.exists(outf)){
#         file.remove(outf)
#       }
#       writeLines(txt.kept,con=outf)
#       print(paste("Wrote sample to file: ",outf))
#     }
#   }
# }

# buildCorpus <- function(n.samples=1,path="."){
#   corpus <- list()          # one entry per sample
#   files <- dir(path)   # default is current directory
#   for(k in seq(n.samples)){
#     sample.files <- files[grep(sprintf("%02i",k),files)]
#     print(paste("Creating corpus from: ",paste0(sample.files,collapse=", ")))
#     # for individual files must use URISource.
#     # Local encoding is UTF-8, but files should be already processed to US-ASCII
#     # Also loads the files. Requires tm library
#     uri <- URISource(paste("file://",sample.files,sep=""),
#                      encoding="US-ASCII",mode='text')
#     corpus[[k]] <- VCorpus(uri,
#                            readerControl = list(reader=readPlain,language="en",load=TRUE))
#   }
#   return(corpus)
# }

sampleText <- function(percent,text.files){
  # takes filenames in a vector and returns a vector with
  # the text of each file.
  getSample <- function(f.in){
    txt <- read.txt(f.in)
    return( txt[sample.int(length(txt),round(percent*length(txt)))] )
  }
  return(sapply(text.files,getSample))
}

buildCorpus <- function(doc.list){
  return(VCorpus(VectorSource(doc.list),
          readerControl = list(reader=readPlain,language="en")))
}

buildDTM <- function(corpus,max.ngram=4){
  tokenizers = list()
  makeTokenizer <- function(n) {
                     n;
                     function(x){
                      unlist(lapply(ngrams(words(x), n), paste,
                                    collapse = " "),use.names = FALSE)
                     }
                   }
  for(i in seq(max.ngram)){
    tokenizers[[i]] <- makeTokenizer(i)
  }
  return(
    lapply(tokenizers,
       function(tk) {
         DocumentTermMatrix(corpus,control=list(tokenize = tk))
       }
    )
  )
}

# dtm is list with Document Text Matrix for ngrams where n=1, 2, ...
# here we loop over the ngrams and sort the results
dtm2freq <- function(dtm){
  return(lapply(dtm,function(x){sort(colSums(as.matrix(x)),decreasing=TRUE)}))
}

score.sbackoff <- function(freqs){
  freqAB2score <- function(freqAB){
    return(freqAB[[1]]/freqAB[[2]][unlist(dropFirstWord(names(freqAB[[1]])))])
  }
  thread <- function(seq1,seq2) {
    mapply(function(x,y) list(x,y), seq1,seq2, SIMPLIFY=FALSE)
  }
  #
  scores <- vector("list",length(freqs))
  scores[[1]] <- freqs[[1]]/sum(freqs[[1]]) # assumes unigram comes first
  otherScores <- lapply(thread(freqs[-1],freqs[-length(freqs)]),freqAB2score)
  for(k in seq(2,length(freqs))){
    scores[[k]] <- otherScores[[(k-1)]]
  }
  names(scores) <- names(freqs)
  scores <- lapply(scores,sort,decreasing=TRUE) # sorts the scores
  return(scores)
}

helper.f3 <- c("sampleText","buildCorpus","buildDTM","dtm2freq","score.sbackoff")
print(paste("Created helper functions: ",paste(helper.f3)))

print("End of nlpTools.R")