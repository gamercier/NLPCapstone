# Capstone Project
# File: exAnal.R
# Takes Builds Document Term Matrix and does exploratory analysis

print("Started script: exAnal.R")
library(ggplot2)
library(wordcloud)

# Got to project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

# load resources
source("nlpTools.R")

#Switch to processed corpus directory
source("toProcCorpusDir.R")
dir()

# Load the corpus
print("Loading corpus.r file.")
load("corpus.r")
ls()

# generate clean corpus but remove stop words and do stemming
very.clean.corpus <- lapply(corpus,purify.corpus,
            toASCII=FALSE,collapseContractions=TRUE,
                       removeStopWords=TRUE,stemWords=TRUE)

# proc.corpus.dir set in toProcCorpusDir.R
text.files <- c("unix.blogs.txt", "unix.news.txt", "unix.twitter.txt")
print(paste("Processed Corpus Directory Files: ",paste0(text.files,collapse=", ")))
#  put in file names
for(k in seq(very.clean.corpus[[1]])){ 
  meta(very.clean.corpus[[1]][[k]],"id") <- text.files[k]
}

# create DTM
print("Working with unigrams.")
dtm.unigram <- DocumentTermMatrix(very.clean.corpus[[1]])
tdm.unigram <- TermDocumentMatrix(very.clean.corpus[[1]])

print("Looking at the most frequent words")

#analysis
# **** Unigrams - Unrestricted
print("Unigram frequency:")
fr.unigram <- colSums(as.matrix(dtm.unigram))
ord <- order(fr.unigram,decreasing=TRUE)
mostFreq.word <-  names(fr.unigram[ord[1]])
print(paste("Most frequent word is *",mostFreq.word,
            "* with frequency: ",fr.unigram[mostFreq.word]))

# Most frequent words:
# Look at top 10% of the words; highest frequency is max(fr.unigram).
fr.words <- findFreqTerms(dtm.unigram,lowfreq=round(0.5*max(fr.unigram)))
print("Words with frequency within 50% of most frequent word:")
print(fr.words)

# Find associations with these most frequent word
print("Associations to most frequent word:")
cor.words <- findAssocs(dtm.unigram,mostFreq.word,0.9)
# This has many with associations with correlation of 1.
print(head(cor.words[[1]]))

# getting a histogram... of the most frequent words - unrestricted
fr.unigram.df <- data.frame(words=names(fr.unigram),frequency=fr.unigram)
h1.plot <- ggplot(subset(fr.unigram.df,
                         fr.unigram >= round(0.5*max(fr.unigram))),
                  aes(x=reorder(words,-frequency),y=frequency))
h1.plot <- h1.plot + geom_bar(stat="Identity",fill="lightblue")
h1.plot <- h1.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h1.plot <- h1.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1))
print(h1.plot)

# creating a word cloud - unrestricted
set.seed(1357) # so it looks the same each time
wordcloud(names(fr.unigram),fr.unigram,min=round(0.4*max(fr.unigram)),
          colors=brewer.pal(10,"Paired"))

# looking at cummulative distribution of unigrams
fr.unigram.sorted <- sort(fr.unigram,decreasing=TRUE)
fcs.unigram <- cumsum(fr.unigram.sorted)/sum(fr.unigram.sorted)
fcs.unigram.df <- data.frame(fraction=fcs.unigram,
                             index=seq(1,length(fcs.unigram)))
fcs.unigram.plot <- ggplot(fcs.unigram.df,aes(x=index,y=fraction))
fcs.unigram.plot <- fcs.unigram.plot + geom_point(color="lightblue") + ggtitle("Unigrams")
print(fcs.unigram.plot)

# Tokenizers
BigramTokenizer  <- function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)
# Going over bigrams
dtm.bigram <- DocumentTermMatrix(very.clean.corpus[[1]],
                    control=list(tokenizer=BigramTokenizer))
tdm.bigram <- TermDocumentMatrix(very.clean.corpus[[1]],
                    control=list(tokenizer=BigramTokenizer))

print("Bigram frequency:")
fr.bigram <- colSums(as.matrix(dtm.bigram))
ordr <- order(fr.bigram,decreasing=TRUE)
mostFreq.bigram  <- names(fr.bigram[ordr[1]])
print(paste("Most frequent bigram is *",mostFreq.bigram,
            "* with frequency: ",fr.bigram[mostFreq.bigram]))

# getting a histogram... of the most frequent bigrams
fr.bigram.df <- data.frame(words=names(fr.bigram),frequency=fr.bigram)
h2.plot <- ggplot(subset(fr.bigram.df,fr.bigram >= round(0.5*max(fr.bigram))),
                 aes(x=reorder(words,-frequency),y=frequency))
h2.plot <- h2.plot + geom_bar(stat="Identity",fill="lightblue")
h2.plot <- h2.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h2.plot <- h2.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1))
print(h2.plot)

# Useful information... How many bigrams cover a given percentage of the corpus?
# sort in decreasing order and plot cummulative distribution

fr.bigram.sorted <- sort(fr.bigram,decreasing=TRUE)
fcs.bigram <- cumsum(fr.bigram.sorted)/sum(fr.bigram.sorted)
fcs.bigram.df <- data.frame(fraction=fcs.bigram,
                             index=seq(1,length(fcs.bigram)))
fcs.bigram.plot <- ggplot(fcs.bigram.df,aes(x=index,y=fraction))
fcs.bigram.plot <- fcs.bigram.plot + geom_point(color="lightblue") + ggtitle("Bigrams")
print(fcs.bigram.plot)

# Going over trigrams
TrigramTokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "),
         use.names = FALSE)
# Going over Trigrams
dtm.trigram <- DocumentTermMatrix(very.clean.corpus[[1]],
                                 control=list(tokenizer=TrigramTokenizer))
tdm.trigram <- TermDocumentMatrix(very.clean.corpus[[1]],
                                 control=list(tokenizer=TrigramTokenizer))

print("Trigram frequency:")
fr.trigram <- colSums(as.matrix(dtm.trigram))
ordr <- order(fr.trigram,decreasing=TRUE)
mostFreq.trigram  <- names(fr.trigram[ordr[1]])
print(paste("Most frequent trigram is *",mostFreq.trigram,
            "* with frequency: ",fr.trigram[mostFreq.trigram]))

# getting a histogram... of the most frequent trigrams
fr.trigram.df <- data.frame(words=names(fr.trigram),frequency=fr.trigram)
h3.plot <- ggplot(subset(fr.trigram.df,fr.trigram >= round(0.5*max(fr.trigram))),
                 aes(x=reorder(words,-frequency),y=frequency))
h3.plot <- h3.plot + geom_bar(stat="Identity",fill="lightblue")
h3.plot <- h3.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h3.plot <- h3.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1))
print(h3.plot)

# Useful information... How many trigrams cover a given percentage of the corpus?
# sort in decreasing order and plot cummulative distribution

fr.trigram.sorted <- sort(fr.trigram,decreasing=TRUE)
fcs.trigram <- cumsum(fr.trigram.sorted)/sum(fr.trigram.sorted)
fcs.trigram.df <- data.frame(fraction=fcs.trigram,
                             index=seq(1,length(fcs.trigram)))
fcs.trigram.plot <- ggplot(fcs.trigram.df,aes(x=index,y=fraction))
fcs.trigram.plot <- fcs.trigram.plot + geom_point(color="lightblue") + ggtitle("Trigrams")
print(fcs.trigram.plot)

# Going over quadgrams
QuadgramTokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 4), paste, collapse = " "),
         use.names = FALSE)
dtm.quadgram <- DocumentTermMatrix(very.clean.corpus[[1]],
                                   control=list(tokenizer=QuadgramTokenizer))
tdm.quadgram <- TermDocumentMatrix(very.clean.corpus[[1]],
                                  control=list(tokenizer=QuadgramTokenizer))

print("Quadgram frequency:")
fr.quadgram <- colSums(as.matrix(dtm.quadgram))
ordr <- order(fr.quadgram,decreasing=TRUE)
mostFreq.quadgram  <- names(fr.quadgram[ordr[1]])
print(paste("Most frequent quadgram is *",mostFreq.quadgram,
            "* with frequency: ",fr.quadgram[mostFreq.quadgram]))

# getting a histogram... of the most frequent quadgrams
fr.quadgram.df <- data.frame(words=names(fr.quadgram),frequency=fr.quadgram)
h4.plot <- ggplot(subset(fr.quadgram.df,fr.quadgram >= round(0.5*max(fr.quadgram))),
                  aes(x=reorder(words,-frequency),y=frequency))
h4.plot <- h4.plot + geom_bar(stat="Identity",fill="lightblue")
h4.plot <- h4.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h4.plot <- h4.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1))
print(h4.plot)

# Useful information... How many quads cover a given percentage of the corpus?
# sort in decreasing order
fr.quadgram.sorted <- sort(fr.quadgram,decreasing=TRUE)
fcs.quadgram <- cumsum(fr.quadgram.sorted)/sum(fr.quadgram.sorted)
fcs.quadgram.df <- data.frame(fraction=fcs.quadgram,
                             index=seq(1,length(fcs.quadgram)))
fcs.quadgram.plot <- ggplot(fcs.quadgram.df,aes(x=index,y=fraction))
fcs.quadgram.plot <- fcs.quadgram.plot + geom_point(color="lightblue") + ggtitle("Quadgrams")
print(fcs.quadgram.plot)

print("Saving to exAnal.r")
save(very.clean.corpus,dtm.unigram,tdm.unigram,
     dtm.bigram,tdm.bigram,dtm.trigram,tdm.trigram,dtm.quadgram,tdm.quadgram,
     file="exAnal.r")

####
############### Looking at the frequencies ####
# load("dbfreq.r")
# ffDB<- lapply(freq.db[[1]],freq2ff)
# lapply(ffDB,head,n=5)

print("Returning to main directory!")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

print("Completed analysis.R")