#Quiz 1

#Question 1
# answered using looking at ls output in operating system
setwd("~/git/NLPCapstone")
source("toCorpusDir.R")
system("ls -lh")
# Answer 200MB

#Question 2
system("wc -l en_US.twitter.txt")
#Answer 2,360,148

#Question 3
fnames <- c("en_US.blogs.txt","en_US.twitter.txt","en_US.news.txt")
corpus <- list("en_US.blogs.txt"="","en_US.twitter.txt"="","en_US.news.txt"="")
for(f in fnames){
  con <- file(f,open="r'")
  corpus[[f]] <- readLines(con)
  close(con)
}
nline.blogs <- max(sapply(corpus[["en_US.blogs.txt"]],nchar)) # 40833
nline.twitter <- max(sapply(corpus[["en_US.twitter.txt"]],nchar)) # 11384
nline.news <- max(sapply(corpus[["en_US.news.txt"]],nchar)) # 140
#Answer 40833 in blogs

#Question 4
nlove <- sapply(corpus[["en_US.twitter.txt"]],function(x) grepl('love',x))
nhate <- sapply(corpus[["en_US.twitter.txt"]],function(x) grepl('hate',x))
sum(nlove)/sum(nhate) #4.108592
#Answer 4

#Question 5
txt<- corpus[["en_US.twitter.txt"]][grep("biostats",corpus[["en_US.twitter.txt"]])]
# Answer: They have not studied for their exam.
# "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

#Question 6
sent <- "A computer once beat me at chess, but it was no match for me at kickboxing"
txt2 <- corpus[["en_US.twitter.txt"]][grep(sent,corpus[["en_US.twitter.txt"]])]
length(txt2)
#Answer 3

#All answers correct!