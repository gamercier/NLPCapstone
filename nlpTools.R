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
  toStr(lapply(words,function(s) { n<-length(s); s[1:(n-1)] }))
}

getLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { tail(s,1) }))
}

dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { n<-length(s); s[2:n] }))
}
countWords <- function(ngram) lapply(toWords(ngram),length)

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
# hyphens (-), en-dash(--), em-dash(---)
HYPHEN.PAT <- "(?<=[a-zA-Z0-9 ])-(?=[a-zA-Z0-9 ])"
ENDASH.PAT <- "(?<=[a-zA-Z0-9 ])--(?=[a-zA-Z0-9 ])" # true endash is outside ASCII code
EMDASH.PAT <- "(?<=[a-zA-Z0-9 ])---(?=[a-zA-Z0-9 ])" # true emdash is outside ASCII code
# bad characters.
BAD.PAT <- "[^[:alnum:][:punct:][:space:]]"

regex.p <- c("TWITTERSLANG","PROFANITY","URL.PAT",
             "MAIL.PAT","TWHASH.PAT","TWUSER.PAT","CNT.PAT","HYPHEN.PAT","ENDASH.PAT",
             "EMDASH.PAT","BAD.PAT")
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
                          collapseHyphens=FALSE,removeStopWords=FALSE,stemWords=FALSE){
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
  #   Difference 5
  #   Options to collapse hyphens (to null), endash (to space), and emdashes (to space)
  #
  #   Same
  #   Removes Punctuation
  #   Removes Numbers
  #
  #   Difference 6
  #   Does not remove Stop words
  #
  #   Difference 7
  #   Does not do Stemming
  #
  #   Difference 8
  #   Strips whitespaces - done last
  
  # requires tm package
  # if(!require("tm")){ stop("Requires tm package!") }
  
  # helper functions
  toNone.tm <- content_transformer(toNone)
  toSpace.tm <- content_transformer(toSpace)
  killProfane.tm <- content_transformer(killProfane)
  tolower.tm <- content_transformer(tolower)
  
  #### 
  # 1) Make ASCII. Already in US-ASCII with proper line termination.
  #                See result of checkRawData.R
  if(toASCII){
    print("Cleaning: Special characters")
    corpus <- tm_map(corpus,toSpace.tm,BAD.PAT, mc.cores=1)
  }
  
  # 2) Set to lower case
  print("Going to lower case!")
  corpus <- tm_map(corpus,tolower.tm, mc.cores=1)
  
  # 3) Remove lines with profanity
  print("Killing profanity!")
  corpus <- tm_map(corpus,killProfane.tm, PROFANITY, mc.cores=1)
  
  # 2) Null url like patterns
  print("Nulling URL")
  corpus <- tm_map(corpus,toNone.tm, URL.PAT, mc.cores=1)
  
  # 3) Null email like patterns
  print("Nulling mail")
  corpus <- tm_map(corpus,toNone.tm, MAIL.PAT, mc.cores=1)
  
  # 4) Null twitter hash tags and user names
  print("Nulling twitter hash tags and user names")
  corpus <- tm_map(corpus,toNone.tm, TWHASH.PAT, mc.cores=1)
  corpus <- tm_map(corpus,toNone.tm, TWUSER.PAT, mc.cores=1)
  
  # 5) Null twitter slang
  print("Nulling twitter slang")
  corpus <- tm_map(corpus,removeWords,TWITTERSLANG, mc.cores=1)
  
  # 6) Collapse contractions - will do
  if(collapseContractions){
    print("Cleaning: Collapsing contractions.")
    corpus <- tm_map(corpus,toNone.tm,CNT.PAT,perl=TRUE, mc.cores=1)
  }
  
  # 7) Collapse hyphens, and set to blanks endash and emdash
  if(collapseHyphens){
    print("Nulling hyphens")
    corpus <- tm_map(corpus,toNone.tm,HYPHEN.PAT,perl=TRUE, mc.cores=1)
    print("ENdashes and EMdashes to blanks.")
    corpus <- tm_map(corpus,toSpace.tm,ENDASH.PAT,perl=TRUE, mc.cores=1)
    corpus <- tm_map(corpus,toSpace.tm,EMDASH.PAT,perl=TRUE, mc.cores=1)
  }
  
  # 8) Remove punctuation and numbers
  # punctuations in English:
  #   periods, question mark, exclamation point
  #   commas, dash, colon, semicolon
  #   ellipsis, hyphen, quotation mark, apostrophe, parenthesis
  print("Removing punctuation and numbers")
  corpus <- tm_map(corpus,removePunctuation, mc.cores=1) # may need to keep "[.!?]"
  corpus <- tm_map(corpus,removeNumbers, mc.cores=1)
  
  # 9) Remove stopwords - will not do for guess database
  if(removeStopWords){
    print("Cleaning: removing english stop words.")
    corpus <- tm_map(corpus,removeWords,stopwords("english"), mc.cores=1)
  }
  
  # 10) Remove extra white spaces
  print("Stripping whitespace")
  corpus <- tm_map(corpus,stripWhitespace, mc.cores=1)
  
  # 11) Stemming - will not do for guess database
  if(stemWords){
    corpus <- tm_map(corpus,stemDocument, mc.cores=1)
  }
  return(corpus)
}

simple.purify <- function(ngram){
  # takes ngram and return a "cleaned version"
  
  # regex
  ## contraction pattern
  CNT.PAT <-"(?<=[a-zA-Z0-9])'(?=[a-zA-Z0-9])|(?<=[a-zA-Z0-9])\"(?=[a-zA-Z0-9])"
  ## hyphens (-), en-dash(--), em-dash(---)
  HYPHEN.PAT <- "(?<=[a-zA-Z0-9 ])-(?=[a-zA-Z0-9 ])"
  ENDASH.PAT <- "(?<=[a-zA-Z0-9 ])--(?=[a-zA-Z0-9 ])" # true endash is outside ASCII code
  EMDASH.PAT <- "(?<=[a-zA-Z0-9 ])---(?=[a-zA-Z0-9 ])" # true emdash is outside ASCII code
  # bad characters.
  BAD.PAT <- "[^[:alnum:][:punct:][:space:]]"
  PUNC.PAT <- "[[:punct:]]"
  NOTALPHA <- "[^[:alpha:][:space:]]"
  MULTISPACE <- "[[:space:]]+"
  
  ngram <- toNone(ngram,pattern=NOTALPHA) # gets rid of punctuation and numbers
  ngram <- toNone(ngram,pattern=CNT.PAT,perl=TRUE) # collapse contractions
  ngram <- toNone(ngram,pattern=HYPHEN.PAT,perl=TRUE) # collapse hyphens
  ngram <- toSpace(ngram,pattern=ENDASH.PAT,perl=TRUE)
  ngram <- toSpace(ngram,pattern=EMDASH.PAT,perl=TRUE)
  ngram <- toSpace(ngram,pattern=MULTISPACE) #replaces multi space with on space
  
  return(str_trim(tolower(ngram)))
}

helper.f2 <- c("toSpace","toNone","killProfane","purify.corpus","simple.purify")
print(paste("Created helper functions: ",paste(helper.f2)))

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
         DocumentTermMatrix(corpus,control=list(tokenize = tk,
                                                wordLengths=c(1,Inf)))
       }
    )
  )
}

buildTDM <- function(corpus,max.ngram=4){
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
             TermDocumentMatrix(corpus,control=list(tokenize = tk,
                                                    wordLengths=c(1,Inf)))
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

helper.f3 <- c("sampleText","buildCorpus","buildDTM","buildTDM",
               "dtm2freq","score.sbackoff")
print(paste("Created helper functions: ",paste(helper.f3)))

# for frequency of frequencies as data.frame, looping over ngrams
freq2ff <- function(freq){
  ff.table <- table(freq)
  table2df <- function(t.in){
    return(data.frame(freq=as.numeric(unlist(dimnames(t.in))),freq.of.freq=as.numeric(t.in)))
  }
  return(table2df(ff.table))
}

# selection function
selectToKeep <- function(freq,cutoff=1){
  return(names(freq[freq > cutoff]))
}

cut.score <- function(score,freq,cut.level=3){
  to.keep <- selectToKeep(freq,cut.level)
  return(sort(score[to.keep],decreasing=TRUE))
}

helper.f4 <- c("freq2ff","selectToKeep","cut.score")
print(paste("Created helper functions: ",paste(helper.f4)))

print("End of nlpTools.R")

### JUNK

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

# old_guess.sb <- function(trigram,scores=scoresDB,ngrams=ngramsDB,bases=basesDB){
#   ngram <- trigram
#   n <- 3
#   hits <- c((0.4^3)*TOPUNI.SCORES)
#   while(n > 0) {
#     if(ngram %in% ngrams[[n]]){
#       hits <-
#         c(hits,((0.4)^(3-n))*scores[[(n+1)]]
#           [ ngrams[[(n+1)]][ ngram == bases[[(n)]] ] ]) # basesDB is offset down by 1
#     }
#     # back off
#     ngram <- dropFirstWord(ngram)
#     n <- n-1
#   }
#   scores.sorted <- sort(hits,decreasing=TRUE)
#   scores.sorted <- scores.sorted[unique(names(scores.sorted))][1:3]
#   words.sorted <- toWords(names(scores.sorted))
#   guesses <- sapply(words.sorted, function(x) x[length(x)] )
#   return(data.frame(guess=guesses,scores=scores.sorted))
# }
