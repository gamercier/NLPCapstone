# Capstone Project
#   Test performance of guess application prototype in nextWordApp

setwd("~/git/nextWordApp")
#source("nextWordApp.R")
source("global.R")

library(pryr)
app.size <- mem_used()
db.size <- object_size(scoresDB) + object_size(basesDB)

setwd("~/git/NLPCapstone/nlpData.dir/testing")
load("test_web_dev.r")

##########  automating testing
dropLastWord <- function(ngram){
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { n<-length(s); s[1:(n-1)] }))
}

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

test.sample <- test.quads.dev   ### Must set this.
n.test <- length(test.sample)

start.time <- proc.time()
results <- testing(test.sample)
end.time <- proc.time()
user.time <- end.time["user.self"] + end.time["user.child"] -
             start.time["user.self"] - start.time["user.child"]

n.hits <- sum(sapply(results,function(x) x$hit))

print("*********** Performance Stats ***********")
print(paste("App size (Mb):",round(app.size/(1048576),1)))
print(paste("Database size (Mb):",round(db.size/(1048576),1)))
print(paste("User time (sec):",round(user.time,1),"for",n.test,"guesses."))
print(paste("Speed (msec): ", round(1000*(user.time/n.test),1),"per guess"))
print(paste("Hits: ",sum(n.hits)," out of ",n.test))
print(paste("Accuracy:", 100.0*round(sum(n.hits)/n.test,digits=3),"%"))

# [1] "*********** Performance Stats ***********"
# [1] "App size (Mb): 30.9"
# [1] "Database size (Mb): 3.6"
# [1] "User time (sec): 1.3 for 1000 guesses."
# [1] "Speed (msec):  1.3 per guess"
# [1] "Hits:  244  out of  1000"
# [1] "Accuracy: 24.4 %"