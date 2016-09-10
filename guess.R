# Capstone Project
# file guess.R
# The final version of the guess function for export to shiny app
# Scoring: Stupid back off
# Given a n-gram (n=1-3), guess the upcoming ngram - based on stupic back off

# helper functions
library(stringr)
# helper functions
toWords <- function(ngram) str_split(ngram,pattern=boundary("word"))
toStr <- function(words) lapply(words,str_c,collapse=" ")
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  n <- length(words[[1]]) # no checks! Assumes all ngrams have same n
  toStr(lapply(words,function(s) s[2:n]))
}

getLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { tail(s,1) }))
}
countWords <- function(ngram) lapply(toWords(ngram),length)

toSpace <- function(x, pattern, ...){
  return(gsub(pattern," ", x, ...))
}
toNone <- function(x, pattern, ...){
  return(gsub(pattern,"", x, ...))
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

top3 <- function(v){
  n <- length(v)
  hits <- sort(v,partial=c(n,n-1,n-2))[n:(n-2)]
  idx <- unlist(sapply(hits, function(x) which(v == x)))
  return(v[idx])
}

simple_guess.sb <- function(base_ngram){
  # NEEDS scoresDB, ngramsDB, basesDB, ALPHA
  words <- unlist(toWords(base_ngram))
  n <- length(words)   # size of base_ngram. ngram size is n+1
  nMax <- n
  scores <- (ALPHA^nMax)*TOP.UNI.SCORES # ngram is unigram (base_ngram is ""), n=0
  while(n>0){
    base <- paste(words[(nMax-n+1):nMax],collapse=" ")
    hits <- base == basesDB[[n]]  # basesDB[[n]] has base for ngramsDB[[(n+1)]]
    if(sum(hits)){
      # scores <- c(scores,(ALPHA^(nMax-n))*scoresDB[[(n+1)]][ ngramsDB[[(n+1)]][hits] ])
      scores <- c(scores,(ALPHA^(nMax-n))*scoresDB[[(n+1)]][ hits ])
    }
    n <- n-1
  }
  guesses <- top3(scores)
  data.frame(guess=unlist(getLastWord(names(guesses))),
             scores=guesses,row.names=c("1st","2nd","3rd"))
}

guess.sb <- simple_guess.sb
print("guess.sb <- simple_guess.sb")
