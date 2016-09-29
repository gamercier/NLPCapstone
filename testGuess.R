# Capstone Project
# File: testGuess.R
# Tests guessing function against a database of quadgrams in nlpData.dir/testing

# Move to testing directory and load the test dataset
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
print(paste("Moving to",prj.dir)) 
setwd(prj.dir)
print(paste("Current directory to",getwd()))

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
dropLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { n<-length(s); s[1:(n-1)] }))
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

top3 <- function(v){
  n <- length(v)
  hits <- sort(v,partial=c(n,n-1,n-2))[n:(n-2)]
  idx <- unlist(sapply(hits, function(x) which(v == x)))
  return(v[idx])
}

# simple_guess_sb was modified 2016-09-18 stringsAsFactors = FALSE

simple_guess.sb <- function(base_ngram){
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
  data.frame(guess=unlist(getLastWord(names(guesses)))[1:3],
             scores=guesses[1:3], sources=names(guesses),
             row.names=c("1st","2nd","3rd"),
             stringsAsFactors = FALSE)
}

guess.sb <- simple_guess.sb

##########  automating testing
testing <- function(test.s){
  hit <- function(x){
    output <- guess.sb(unlist(dropLastWord(x)))
    if(unlist(getLastWord(x)) %in% output$guess){
      return(list(hit=TRUE,answer=output))
    } else {
      return(list(hit=FALSE,answer=output))
    }
  }
  test.f <- lapply(test.s,hit)
  names(test.f) <- as.character(1:length(test.s))
  return(test.f)
}

# Load the testing data
#  test_quads_dev.r -- test data for development, 1000 quads news text, non web
#     source: 12dicts-6.0.2_bench_1.zip
#     http://www.anc.org/OANC/OANC_GrAF.zip
#     This is from the American National Corpus: http://www.anc.org/
#  test_web_dev.r -- test data for development, 1000 quads from HCcorpora
#  test_web_test.r -- test data for final testing, 1000 quads from HCcorpora
source("toTesting.R")

##### Set test data set - REQUIRED
test.file <- "test_web_dev.r"

##### Set: scores database (wrk) directory, 
#####      scores file
#####      save file
#####      database element - all REQUIRED ###########
wrk.dir <- "100.dir"
db.element <- "onehundred"
scores.file <- "scoresSB.trimmed.dense.r"
save.file <- "dev_test_web.r"

###
test.file <- file.path(getwd(),test.file)
load(test.file)
print(paste("Loading test data set file",test.file))

if(tail(unlist(strsplit(test.file,"/")),1) == "test_web_dev.r"){
  test.sample <- test.quads.dev
}
print(paste("Loaded test data:",ls(pattern="sample")))

setwd(prj.dir)
print(paste("Returning directory to",getwd()))

##### Load the scores database and set the global variables
source("toDbs.R")
dir()

scores.dir <- file.path(dbs.dir,wrk.dir)
setwd(scores.dir) ; dir()

load(scores.file)
print(paste("Loading scores file",scores.file))
print(paste("Loaded scores database:",
            ls(pattern="^(bases|ngrams|scores)\\.[a-zA-Z0-9\\.]*db")))

scoresDB <-  scores.trimmed.dense.db[[db.element]]
ngramsDB <-  ngrams.trimmed.dense.db[[db.element]]
basesDB  <-  bases.trimmed.dense.db[[db.element]] 
TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
ALPHA <- 0.4

# Do the test
alphas <- c(0.2,0.4,0.6,0.8)
print(paste("Test:",tail(unlist(strsplit(test.file,"/")),1)
            ,"database:",db.element,"method: stupid backoff"))
print(paste("Scores database directory:",scores.dir))
print(paste("Scores file:",scores.file))

results <- list()
n.test <- length(test.sample)
for(alpha in alphas){
  ALPHA <- alpha
  results[[as.character(alpha)]] <- testing(test.sample)
  n.hits <- sum(sapply(results[[as.character(alpha)]],function(x) x$hit))
  print(paste("Hits: ",sum(n.hits)," out of ",n.test,"alpha=",alpha))
}

if(file.exists(save.file)){
  file.remove(save.file)
}
save(results,file=save.file)
print(paste("Results saved in",save.file))

print("Finished testing.")
setwd(prj.dir)
print(paste("Returning directory to",getwd()))

###### AC ####

# Sample 10% trimmed 99%
# [1] "Test: test_quads_dev.r; database: 10pct; method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/10.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  95  out of  1000 alpha= 0.2"
# [1] "Hits:  95  out of  1000 alpha= 0.4"
# [1] "Hits:  100  out of  1000 alpha= 0.6"
# [1] "Hits:  99  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"

# Sample 25% trimmed 99%
# [1] "Test: test_quads_dev.r; database: 25pct; method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/25.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  99  out of  1000 alpha= 0.2"
# [1] "Hits:  98  out of  1000 alpha= 0.4"
# [1] "Hits:  98  out of  1000 alpha= 0.6"
# [1] "Hits:  96  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"

# Sample 50% trimmed 99%
# [1] "Test: test_quads_dev.r database: fifty.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/50.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  101  out of  1000 alpha= 0.2"
# [1] "Hits:  97  out of  1000 alpha= 0.4"
# [1] "Hits:  100  out of  1000 alpha= 0.6"
# [1] "Hits:  98  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"


###### WEB ####

# Sample 10% trimmed 99%
# [1] "Test: test_web_dev.r database: ten.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/10.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  301  out of  1000 alpha= 0.2"
# [1] "Hits:  277  out of  1000 alpha= 0.4"
# [1] "Hits:  252  out of  1000 alpha= 0.6"
# [1] "Hits:  236  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

# Sample 25% trimmed 99%
# [1] "Test: test_web_dev.r database: twentyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/25.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  337  out of  1000 alpha= 0.2"
# [1] "Hits:  310  out of  1000 alpha= 0.4"
# [1] "Hits:  282  out of  1000 alpha= 0.6"
# [1] "Hits:  261  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

# Sample 50% trimmed 99%
# [1] "Test: test_web_dev.r database: fifty.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/50.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  362  out of  1000 alpha= 0.2"
# [1] "Hits:  330  out of  1000 alpha= 0.4"
# [1] "Hits:  295  out of  1000 alpha= 0.6"
# [1] "Hits:  260  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

####################################### 75% Sample Size 
##### 75% sample size
##### AC

# trimming 80%
# [1] "Test: test_quads_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  118  out of  1000 alpha= 0.2"
# [1] "Hits:  118  out of  1000 alpha= 0.4"
# [1] "Hits:  116  out of  1000 alpha= 0.6"
# [1] "Hits:  119  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"

# trimming 90%
# [1] "Test: test_quads_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  105  out of  1000 alpha= 0.2"
# [1] "Hits:  106  out of  1000 alpha= 0.4"
# [1] "Hits:  107  out of  1000 alpha= 0.6"
# [1] "Hits:  109  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"

# trimming 95%
# [1] "Test: test_quads_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  95  out of  1000 alpha= 0.2"
# [1] "Hits:  99  out of  1000 alpha= 0.4"
# [1] "Hits:  99  out of  1000 alpha= 0.6"
# [1] "Hits:  103  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"

# trimming 99%
# [1] "Test: test_quads_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  98  out of  1000 alpha= 0.2"
# [1] "Hits:  95  out of  1000 alpha= 0.4"
# [1] "Hits:  96  out of  1000 alpha= 0.6"
# [1] "Hits:  94  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_ac.r"

##### WEB

# trimming 80%
# [1] "Test: test_web_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  234  out of  1000 alpha= 0.2"
# [1] "Hits:  209  out of  1000 alpha= 0.4"
# [1] "Hits:  204  out of  1000 alpha= 0.6"
# [1] "Hits:  205  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

# trimmed 90%
# [1] "Test: test_web_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  244  out of  1000 alpha= 0.2"
# [1] "Hits:  240  out of  1000 alpha= 0.4"
# [1] "Hits:  226  out of  1000 alpha= 0.6"
# [1] "Hits:  222  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

# trimmed 95%
# [1] "Test: test_web_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  265  out of  1000 alpha= 0.2"
# [1] "Hits:  244  out of  1000 alpha= 0.4"
# [1] "Hits:  228  out of  1000 alpha= 0.6"
# [1] "Hits:  222  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

# trimming 99%
# [1] "Test: test_web_dev.r database: seventyfive.pct method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/75.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  389  out of  1000 alpha= 0.2"
# [1] "Hits:  348  out of  1000 alpha= 0.4"
# [1] "Hits:  312  out of  1000 alpha= 0.6"
# [1] "Hits:  279  out of  1000 alpha= 0.8"
# [1] "Results saved in dev_test_web.r"

####################################### 100% Sample Size 
##### WEB

## trimming 95%
# [1] "Test: test_web_dev.r database: onehundred method: stupid backoff"
# [1] "Scores database directory: /Users/gamercier/git/NLPCapstone/nlpData.dir/dbs/100.dir"
# [1] "Scores file: scoresSB.trimmed.dense.r"
# [1] "Hits:  262  out of  1000 alpha= 0.2"
# [1] "Hits:  245  out of  1000 alpha= 0.4"
# [1] "Hits:  230  out of  1000 alpha= 0.6"
# [1] "Hits:  223  out of  1000 alpha= 0.8"

# NOTICE NO IMPROVEMENT FROM 75%/95% trimmed to 100%/95% trimmed.

#######################################
# Analysis of 75% sample with 90% trim Web sample
# hits:
#   quads:
#      [1] "cant wait to see" "at the top of" 
#   tris:
#     [1] "at least one"         "in the meantime"     
#   [3] "a lot of"             "the beginning of"    
#   [5] "the fact that"        "a lot to"            
#   [7] "the importance of"    "the back of"         
#   [9] "the first time"       "a couple of"         
#   [11] "time of year"         "a good idea"         
#   [13] "when it comes"        "the edge of"         
#   [15] "for more information" "looking forward to"  
#   [17] "have you been"        "i think i"           
#   [19] "the idea of"          "a bit of"            
#   [21] "the hunger games"     "the city of"         
#   [23] "i thought i"          "to see if"           
#   [25] "of the year"          "a picture of"        
#   [27] "i thought it"         "the best of"         
#   [29] "a member of"          "a chance to"         
#   [31] "friends and family"   "to keep the"         
#   [33] "the life of"          "the top of"          
#   [35] "to win the"           "to work with"        
#   [37] "i feel like"          "the history of"      
#   [39] "to get back"          "to get out"          
#   [41] "at a time"            "to make the"         
#   [43] "for a while"          "the only one"        
#   [45] "there are some"       "the state of"        
#   [47] "to make sure"         "so much to"          
#   [49] "at the end"           "the number of"       
#   [51] "to use the"           "to take the"         
#   [53] "if you want"          "i wanted to"         
#   [55] "i was going"          "a very good"         
#   [57] "i decided to"         "i need a"            
#   [59] "a number of"          "go to bed"           
#   [61] "will be at"           "if you dont"         
#   [63] "of thousands of"      "what is your"        
#   [65] "it was a"             "i can get"           
#   [67] "of my life"           "more and more"       
#   [69] "do not have"          "whats going on"      
#   [71] "i had the"            "i would have"        
#   [73] "have been a"          "the new york"        
#   [75] "i like it"            "you have to"         
#   [77] "people who are"       "to think of"         
#   [79] "give it a"            "im going to"         
#   [81] "you need to"          "a part of"           
#   [83] "it could be"          "to have the"         
#   [85] "to me and"            "we dont have"        
#   [87] "dont want to"         "is one of"           
#   [89] "to have a"            "we are not"          
#   [91] "are looking for"      "can do it"           
#   [93] "we had a"             "just wanted to"      
#   [95] "what you want"        "are going to"        
#   [97] "wait to see"          "you get a"           
#   [99] "was going to"         "that they are"       
#   [101] "learn how to"         "im trying to"        
#   [103] "for all the"          "was one of"          
#   [105] "we want to"           "that i am"           
#   [107] "look like a"          "you had a"           
#   [109] "is like a"            "away from the"       
#   [111] "youre going to"       "is a lot"            
#   [113] "really want to"       "be one of"           
#   [115] "not want to"          "do with the"  
#   bi:
#     [1] "new york"         "san francisco"    "the meantime"    
#   [4] "a lot"            "at least"         "the same"        
#   [7] "i am"             "no longer"        "the end"         
#   [10] "to see"           "what happens"     "more than"       
#   [13] "super bowl"       "to bed"           "it comes"        
#   [16] "as well"          "has been"         "you can"         
#   [19] "make sure"        "you want"         "my life"         
#   [22] "has changed"      "more information" "go ahead"        
#   [25] "a while"          "best friend"      "facebook page"   
#   [28] "you dont"         "the year"         "they were"       
#   [31] "weeks ago"        "too much"         "do anything"     
#   [34] "been able"        "good idea"        "if you"          
#   [37] "hunger games"     "they are"         "anything else"   
#   [40] "three months"     "was going"        "less than"       
#   [43] "know what"        "first time"       "those who"       
#   [46] "and family"       "feel like"        "and more"        
#   [49] "would have"       "been doing"       "can get"         
#   [52] "a time"           "are not"          "one of"          
#   [55] "get back"         "best friends"     "could be"        
#   [58] "have a"           "going to"         "dont have"       
#   [61] "out of"           "look at"          "only one"        
#   [64] "time since"       "who are"          "was a"           
#   [67] "are some"         "very good"        "want to"         
#   [70] "with the"         "have to"          "is your"         
#   [73] "get out"          "from the"         "make it"         
#   [76] "see if"           "do it"            "part of"         
#   [79] "had a"            "need to"          "lot of"          
#   [82] "not have"         "came out"         "of year"         
#   [85] "along with"       "looking for"      "like a"          
#   [88] "by the"           "felt like"        "kind of"         
#   [91] "like an"          "you been"         "going on"        
#   [94] "trying to"        "lot more"         "all the"         
#   [97] "exactly what"     "able to"          "like it"         
#   [100] "fact that"        "be at"            "end of"          
#   [103] "say that"         "get a"            "least one"       
#   [106] "work with"        "how to"           "wanted to"       
#   [109] "already been"     "me and"           "number of"       
#   [112] "think i"          "been a"           "couple of"       
#   [115] "forward to"       "thought it"       "ready to"        
#   [118] "think of"         "through the"      "bit of"          
#   [121] "it a"             "need a"           "have the"        
#   [124] "decided to"       "top of"           "wants to"        
#   [127] "around the"       "due to"           "chance to"       
#   [130] "supposed to"      "thought i"        "close to"        
#   [133] "thousands of"     "member of"        "idea of"         
#   [136] "state of"         "make the"         "had the"         
#   [139] "willing to"       "best of"          "nice to"         
#   [142] "history of"       "back of"          "city of"         
#   [145] "picture of"       "take the"         "beginning of"    
#   [148] "much to"          "glad to"          "life of"         
#   [151] "game and"         "use the"          "allowed to"      
#   [154] "difficult to"     "room and"         "morning and"     
#   [157] "keep the"         "importance of"    "edge of"         
#   [160] "win the"          "friend and"       "choose to"       
#   [163] "onto the"         "lot to"           "approach to"     
#   [166] "hear the"         "kids to"          "means to"        
#   [169] "end the"          "away the"         "fact the"   
#   uni:
#     [1] "the"         "to"          "and"         "a"          
#   [5] "of"          "i"           "for"         "that"       
#   [9] "you"         "it"          "on"          "with"       
#   [13] "at"          "be"          "have"        "are"        
#   [17] "not"         "an"          "your"        "out"        
#   [21] "one"         "if"          "what"        "like"       
#   [25] "who"         "can"         "more"        "get"        
#   [29] "were"        "time"        "some"        "been"       
#   [33] "good"        "dont"        "back"        "than"       
#   [37] "see"         "going"       "well"        "much"       
#   [41] "year"        "am"          "want"        "life"       
#   [45] "while"       "since"       "same"        "sure"       
#   [49] "lot"         "family"      "doing"       "end"        
#   [53] "friends"     "least"       "anything"    "ago"        
#   [57] "able"        "friend"      "games"       "months"     
#   [61] "idea"        "comes"       "else"        "information"
#   [65] "york"        "bed"         "page"        "longer"     
#   [69] "bowl"        "changed"     "ahead"       "happens"    
#   [73] "francisco"   "meantime"  

    
    
    
