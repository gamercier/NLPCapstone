#For stupid back up implementation:
#http://stackoverflow.com/questions/16383194/stupid-backoff-implementation-clarification
# Input:
#    Vector of ngrams (NOT a list of ngrams)
#        To get the vector from the list use function unlist
score.sb <- function(ngram,n,a=0.4){
  # assumes 0 < n <= 4  0 and a unigram can be found in dictionary
  # dictionary is limited to the ngrams.db$unigram
  # assumes abbreviated dictionary so that higher order ngrams may have words(tokens)
  # not present in the dictionary. In such case, the frequency of the missing ngram
  # is set to 1 in computing the score.
  if(n == 0) return(1/N.db$unigram)
  if(n == 1) {
    if(ngramsInDB(ngram,ngrams.db$unigram)){
      return(freq.db$unigram[ngram]/N.db$unigram)
    }
    else {
      # assign freq.db$unigram[ngram] = 1 because it is missing
      return (1/N.db$unigram)
    }
  }
  else {
    if(ngramsInDB(ngram,ngrams.db[[n]])){
      top <- freq.db[[n]][ngram]
      base <- unlist(dropLastWord(ngram))
      if(ngramsInDB(base,ngrams.db[[(n-1)]])){
        bottom <- freq.db[[(n-1)]][base]
      }
      else {
        # assign freq.db[[(n-1)]][base] = 1 because it is missing
        bottom <- 1
      }
      return(top/bottom)
    }
    else {
      # do back off step
      new_gram <- unlist(dropFirstWord(ngram))
      return(a*score.sb(new_gram,(n-1)))
    }   
  }
}

# TESTING
library(assertthat)
#   Scoring function score.sb
g1 <- "yes yes yes yes"      # quad with all ngrams present   3/7
g2 <- "baba baba baba baba"  # quad with no ngrams present   (0.4^3)*(1/1085425)
g3 <- "yes"                  # unigram present                830/1085425
g4 <- "baba"                 # unigram not present              1/1085425
g5 <- "baba yes yes yes"     # quad not present, tri present (0.4)*(7/28)
g6 <- "baba yes baba yes"    # quad with no grams present    (0.4^3)*(830/1085425)
g7 <- "baba baba yes yes"    # quad not present bigram is    (0.4^2)*(28/830)
g8 <- "im glad"              # bigram present unigram absent 93/1

test.ngrams <- list(g1=list(g1,4),
                    g2=list(g2,4),
                    g3=list(g3,1),
                    g4=list(g4,1),
                    g5=list(g5,4),
                    g6=list(g6,4),
                    g7=list(g7,4),
                    g8=list(g8,2))
test.scores <- c(g1=(3/7),g2=(0.4^3)*(1/1085425),
                 g3=830/1085425, g4=1/1085425,
                 g5=(0.4)*(7/28), g6=(0.4^3)*(830/1085425),
                 g7=(0.4^2)*(28/830),g8=93/1)
# checks out if TRUE
print("Doing test... will give error if problem! Otherwise silent")
test.1 <- assert_that(
  abs(sum(
    (sapply(test.ngrams,function(x) score.sb(x[[1]],x[[2]])) - test.scores) < 10^-8)
  ) == 8
)

