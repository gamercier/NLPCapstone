# Capstone Project
# file: helpers.R
# also loads tools

prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

print("Loading packages...")

# library(RWeka) # probably loaded with tm
library(stringi) # for stri_extract_all_words, stri_extract_first/last_words, stri_count_words
library(stringr) # using stringr, a wrapper over stringi
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# wordnet is special
# source: https://wordnet.princeton.edu/
# Version 3.1 installed using homebrew package manager for Mac OSX
# wnhome <- "/usr/local/Cellar/wordnet/3.1/dict"
# options(warn=-1) # turn off warning fixed with last two statements
# library(wordnet)
# options(warn=0)

# setDict(wnhome)
# initDict(wnhome)
print("Finish loading packages!")

# read text file - Needs this early
read.txt <- function(fname){
  con <- file(fname,open="r")
  txt <- readLines(con)
  close(con)
  return(txt)
}
print("Setting global variables...")
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

print("Finished setting global variables...")

print("Loading helper functions...")

# Think in these terms. In a corpus the content is a set of documents.
# tm_map applies the content_transformer to each document content.
# Document content consist of a character vector with each element
# corresponding to a paragraph. Each paragraph may contain one or more sentences.
#
# str_split sentences returns a list.
#   So when applied to the paragraph returns a list, one element per paragraph.
#   each element contains a character vector containing individual sentences
#
# lapply(str_split_words)
#   Goes over each element of the list and splits each sentence into words.
#   The result is a list of lists. The top list goes over paragraphs, the next
#   list goes over sentences in the paragraph and the lowest level contains
#   a character vector with each element containing a word.

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

# clean string
#NEEDS TESTING
clean.str <- function(string){
  # requires tm package
  # if(!require("tm")){ stop("Needs tm package!") }

  # 1) Get rid of non ASCII characters by setting to space.
  cleaned.str <- toSpace(string,BAD.PAT) 
  
  # 2) Fix url like patterns
  cleaned.str <- toSpace(cleaned.str,URL.PAT)
  
  # 3) Fix email like patterns
  cleaned.str <- toSpace(cleaned.str,MAIL.PAT)
  
  # 4) Fix twitter hash tags and user names
  cleaned.str <- toSpace(cleaned.str,TWHASH.PAT)
  cleaned.str <- toSpace(cleaned.str,TWUSER.PAT)
  
  # 5) Fix twitter slang
  cleaned.str <- removeWords(cleaned.str,TWITTERSLANG)
  
  # 6) Collapse contractions
  cleaned.str <- toNone(cleaned.str,CNT.PAT,perl=TRUE)
  
  # 7) Do common fixes
  # removeWords,removeNumbers,removePunctuation - from tm package
  # tolower is base package.
  cleaned.str <- removePunctuation(cleaned.str)
  cleaned.str <- removeNumbers(cleaned.str)
  cleaned.str <- tolower(cleaned.str)
  cleaned.str <- removeWords(cleaned.str,stopwords("english"))
  
  # 8) Remove profanity
  cleaned.str <- removeWords(cleaned.str,PROFANITY)

  # 9) Remove white spaces and stem the words
  # stripWhiteSpace, and wordStem are from tm package.
  cleaned.str <- stripWhitespace(cleaned.str)
  cleaned.str <- wordStem(cleaned.str) # from SnowballC, but loaded with tm
  
  return(cleaned.str)
}

clean.corpus <- function(corpus){
  #   Filters non Ascii
  #   Removes special stuff:
  #       urls, emails, twitter hash tags, and user names
  #   Removes chat and twitter slang
  #   Collapse contractions like don't to dont
  #   Removes Punctuation
  #   Removes Numbers
  #   Goes to lower
  #   Remove Stop words
  #   Remove profanity
  #   Strips white spaces
  #   Does Stemming
  
  # requires tm package
  # if(!require("tm")){ stop("Requires tm package!") }
  
  # helper functions
  toSpace.tm <- content_transformer(toSpace)
  toNone.tm <- content_transformer(toNone)

  # 1) Get rid of non ASCII characters by setting to space.
  corpus <- tm_map(corpus,toSpace.tm,BAD.PAT)
  
  # 2) Fix url like patterns
  corpus <- tm_map(corpus,toSpace.tm, URL.PAT)
  
  # 3) Fix email like patterns
  corpus <- tm_map(corpus,toSpace.tm, MAIL.PAT)
  
  # 4) Fix twitter hash tags and user names
  corpus <- tm_map(corpus,toSpace.tm, TWHASH.PAT)
  corpus <- tm_map(corpus,toSpace.tm, TWUSER.PAT)
  
  # 5) Fix twitter slang
  corpus <- tm_map(corpus,removeWords,TWITTERSLANG)
  
  # 6) Collapse contractions
  corpus <- tm_map(corpus,toNone.tm,CNT.PAT,perl=TRUE)
  
  # 7) Do common fixes
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  
  # 8) Remove profanity
  corpus <- tm_map(corpus,removeWords,PROFANITY)
  
  # 9) Remove white spaces and stem the words
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,stemDocument)
  
  return(corpus)
}

# Manual tokenization helper functions
# These require stringr package
getLines <- function(fname){
  con <- file(fname,open="r")
  txt <- readLines(con)
  close(con)
  return(txt)
}

setLower <- function(txt){
  return(tolower(txt))
}

kill <- function(txt,regx){
  # returns everything that DOES NOT match regx
  # if using base package
  #     lines <- gsub(regx," ",txt)
  #     words <- unlist(strsplit(lines,split=" "))
  # if using stringr
  #     lines <- str_replace_all(txt,regx," ") # replace it with a blank
  #     words <- unlist(str_split(lines," "))
  #
  # if(!require("stringr")) { stop("Requires stringr package!) }
  lines <- str_replace_all(txt,regx," ") # replace it with a blank
  words <- unlist(str_split(lines," "))
  # get rid of ""
  indeces <- which(words == "") # generated when two or more blanks in a row
  if(length(indeces) > 0){
    words <- words[-indeces]
  }
  return(words)
}

pull <- function(txt,regx){
  # returns everything that DOES match regx
  return(unlist(str_extract_all(txt,regx)))
}

getWords <- function (txt){
  regx <- "[^a-zA-Z\\s]" # matches anything that is not a word
  return(kill(txt,regx))
}

getNumbers <- function(txt){
  regx <- "([+-]?([0-9]*)[.,]*([0-9]+))" # matches floating and integers
  return(pull(txt,regx))
}

getPunctuation <- function(txt){
  regx <- "[[:punct:]]+"
  return(pull(txt,regx))
}

# ngram Helper functions
#   Each ngram is represented as a string with each word(token) separated by " "
#   (for unigrams we have only one word)
#   Input:
#      vector of ngrams
#      OR
#      list of ngrams
#
#   Output:
#      A list with one entry per ngram.
#          
#  Example:
#   To split the quadgrams in short.ngrams into words (token)
#      toWords(short.ngrams$quadgram)
#   To get this back to a list of the original ngrams in short.ngrams$quadgram
#      toStr(toWords(short.ngrams#quadgram))
#
# toWords <- function(ngram) stri_extract_all_words(ngram)
toSentences <- function(paragraph) str_split(paragraph,pattern=boundary("sentence",skip_word_none = FALSE))
toWords <- function(ngram) str_split(ngram,pattern=boundary("word"))
toStr <- function(words) lapply(words,str_c,collapse=" ")
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n
  toStr(lapply(words,function(s) s[2:n]))
}
dropLastWord <- function(ngram){
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n and n > 1
  toStr(lapply(words,function(s) s[1:(n-1)]))
}
appendWord <- function(ngram,last.word){
  ngram.words <- toWords(ngram)
  words <- lapply(ngram.words,function(g) c(g,last.word))
  toStr(words)
}

ngramsInDB <- function(ngram,db){
  if(sum(sapply(ngram, function(x) x %in% db))){
    return(TRUE)
  }
  return(FALSE)
}

# This may be a faster function due to short circuit
# ngramsInDict <- function(words,dict){
#   words <- words[[1]]
#   for(w in words){
#     if(!(w %in% dict)) return(FALSE)
#   }
#   return(TRUE)
# }

print("Finished loading helper functions!")
