library(tm)
docs <- c("this is the first document. numbers four three two",
          "this is the second document. numbers four three  two",
          "this is the third document.  numbers four three",
          "this is the fourth document. numbers four",
          "this is the fifth document. numbers")
cp <- VCorpus(VectorSource(docs))
dtm <- DocumentTermMatrix(cp)
print(as.matrix(dtm))
doc2 <- c("this a first document. numbers four three two",
          "this a second document. numbers four three  two",
          "this a third document.  numbers four three",
          "this a the fourth document. numbers four",
          "this a the fifth document. numbers")
cp2 <- VCorpus(VectorSource(doc2))
dtm2 <- DocumentTermMatrix(cp2)
doc3 <- c("is as a",
          "is a as",
          "a is as",
          "as is a",
          "a as is")
cp3 <- VCorpus(VectorSource(doc3))
dtm3 <- DocumentTermMatrix(cp3,control=list(wordLengths=c(1,Inf)))