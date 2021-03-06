# Data Science Milestone Report 
Gustavo Mercier  
`r format(Sys.time(), "%d %B, %Y")`  


## The project.

The [Johns Hopkins Bloomberg School of Public Health](http://www.jhsph.edu/) runs a data science specialization [course](https://www.coursera.org/specializations/jhu-data-science) through the Coursera platform. The capstone project for this course is a prototype application that is able to guess the "next" word that follows a string of three words.

Software like this is behind the smart keyboards that assist users in writing text in smartphones. At the core of the software is a dictionary of words and strings of words of varying length. These strings are called _n-grams_. The words are called _tokens_. For example, the phrase "typing text" is a 2-gram, or bigram, with 2 tokens, "typing" and "text". 

The n-gram dictionaries are empirically generated from text files. A collection of text files is called a _corpus_. In this milestone report I pre-process and describe a corpus that is the source for the n-grams dictionary of the capstone application for the data science course.

### The data set.

[HC Corpora](http://www.corpora.heliohost.org/) collected and did initial processing of the corpus. This corpus is stored in a [zip file](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) and contains text files in several languages, including English. The raw data for the text files consists of blogs, news feeds, and twitter twits gathered by a webcrawler. HC Corpora makes an effort to eliminate foreign words and make the source text anonymous. The corpus is in the public domain without copyright. Additional details regarding the raw files is available from their [web page](http://www.corpora.heliohost.org/aboutcorpus.html). Users are also encouraged to contact [Hans Christensen](mailto:hc.corpus@gmail.com) for additional information.

### Download of data and choice of text files.

The zip file is downloaded and expanded into the subdirectory of the _git_ directory for this project.


```r
URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zip.file <- "CourseraSwiftKey.zip"
download.file(URL,destfile=zip.file,method="libcurl",mode="wb")
system2("unzip",c(zip.file))
```

The zip file is 548Mb in size. Its expansion generates the subdirectory `final` that contains corpora in different languages. I will use the english corpus under `final/en_US`:


```
## [1] "en_US.blogs.txt"   "en_US.news.txt"    "en_US.twitter.txt"
```

### Raw text file processing.

For speed I use system utilities to collect information on the raw text files. These [utilities](https://www.wikiwand.com/en/List_of_Unix_commands) are `ls`, `wc`, `file`, `iconv`, `awk`, and `dos2unix`. They are called from within R using the `system2` command with the results collected in a data frame.




Table: Raw Text File Characteristics

File                 Size    Characters.Single    Characters.Multi     Words       Lines    Encoding                                                                     
------------------  ------  -------------------  ------------------  ----------  ---------  -----------------------------------------------------------------------------
en_US.blogs.txt      200M        210160014           208623081        37334690    899288    UTF-8 Unicode English text, with very long lines, with CRLF line terminators 
en_US.news.txt       196M        205811889           205243643        34372720    1010242   UTF-8 Unicode English text, with very long lines, with CRLF line terminators 
en_US.twitter.txt    159M        167105338           166816544        30374206    2360148   UTF-8 Unicode English text, with CRLF line terminators                       

The table provides insight into the following issues:

1. The encoding is UTF-8 English. This is a superset of the [US-ASCII](www.asciitable.com), a one byte encoding of the english alphabet, numbers and  punctuation, plus additional non-printing control characters. The discrepancy between single byte characters and multi byte characters indicates that non-ascii characters are present. So the text contains characters outside those encoding the English language.

2. The line terminators are consistent with the Windows operating system, CR+LF, and not Unix convention, LF. Mac OS X also uses LF.

We know twits are 140 characters long. The unix `awk` utility is convenient to figure out the maximum number of characters in a twit (line) in the twitter file.


```
## [1] "Maximum number of characters in twitter file twit (line):  214"
```

```
## [1] "THE LINE IS: It's time for you to give me a little bit of lovin'（さぁちょっとはあなたの愛をちょうだい）Baby, hold me tight and do what I tell you！（ベイビー抱きしめて私が言うように！）\r"
```

This confirms the presence of non-English characters. So before we explore the files, I will process the files to make the following changes:

1. Convert to the files to US-ASCII using the `iconv` utility.

2. Convert the line terminations to Mac OS X conventions using the `dos2unix` utility.






```
## [1] "Performed file conversions using system utilities."
```

```
## [1] "Converted files: unix.blogs.txt unix.news.txt unix.twitter.txt"
```

After conversion we see the number of single byte characters equals that of multi-byte characters, and the `file` utility confirms the US-ASCII encoding and LF termination.


Table: Converted Text File Characteristics

File                Size    Characters.Single    Characters.Multi     Words       Lines    Encoding                                 
-----------------  ------  -------------------  ------------------  ----------  ---------  -----------------------------------------
unix.blogs.txt      197M        206943194           206943194        37272580    899288    ASCII English text, with very long lines 
unix.news.txt       194M        203927846           203927846        34309644    1010242   ASCII English text, with very long lines 
unix.twitter.txt    157M        164321710           164321710        30341030    2360148   ASCII English text                       

The twitter file now conforms to 140 characters!


```
## [1] "Maximum number of characters in twitter file twit (line):  140"
```

```
## [1] "THE LINE IS: Don't care what others think of you, and you will save yourself a lot of mental energy that instead can be used to push you towards success."
```

### Cleaning the text and generating a corpus object.

There are several steps to take before we can explore the n-grams in the corpus. In our case the corpus will consist of the pre-processed text files:


```
## [1] "unix.blogs.txt"   "unix.news.txt"    "unix.twitter.txt"
```

This corpus is loaded as a `corpus` object using the `tm` package. This object is then processed with the goal of computing a _document text matrix_ for a given n-gram in the corpus. This matrix encapsulates the frequency of the n-grams in the corpus. 

For example a 1-gram document text matrix describes the frequency of words in each document. The matrix consists of one row per document and multiple columns, one for each word. The transpose is called the _text document matrix_. The statistical analysis of natural language starts with manipulations of these matrices.

The corpus in our case is nearly half a gigabyte in size, and too big for manipulation with my computer resources. For this reason, I select a random sample of the lines in the corpus. This sample is 3% of the total corpus. The sample is small, but due to its randomness should give us a glimpse of the statistical properties of the corpus.

The filter I apply to the corpus does the following:

1. Reduces characters to lower case.
2. Removes profanity
3. Removes special entries:
    a. url
    b. email addresses
    c. twitter hash tags
    d. user names
    e. twitter and chat slang
4. Remove punctuation and numbers
7. Strips extra white spaces

When removing profanity I remove the entire sentence that contains the profane words. This makes sure that no artificial n-grams are created as would be the case if I would only deleted the word. There are several lists of profane words in the public domain. I use the one at [www.banned.wordlist.com](http://www.banned.wordlist.com). It is relatively short, so reasonable words like _sex_ are not excluded.

To remove special entries I apply a series of regular expressions constructed with the aide of multiple sources in the web. These regular expressions are:


```r
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
```

To remove punctuation and numbers, and strip extra white spaces I use the built-in facilities in the `tm` package.

It is common to apply addititonal filters that collapse contractions, i.e. _don't_ is replaced by _dont_, remove stop words, and stem the words. [Stop words](https://www.wikiwand.com/en/Stop_words) are common words in the language that serve as glue between phrases, and do not necessarily carry information. They are usually omitted when doing natural language processing for the purspose of understanding the "message" and "feelings" (often called the _sentiment_) in the corpus. [Stemming](https://www.wikiwand.com/en/Stemming) is the process of reducing derived words to their root or stem. In this way _consider_ and _considering_ are grouped together as the same "word". This is a common operation in linguistic morphology analysis or information retrieval.

Because the goal is to understand the distribution of n-grams as applied to a word guessing application, I will avoid stemming and keep contractions and stop words. Removing contractions means that _doesn't_ reduces to _doesnt_. This puts people that skip the apostrophe when typing contractions in their smartphones on the same foot as those that keep them. _However, it also means that we will have to pre-process the three word input so that it matches our filtered corpus._





## Exploring the processed corpus.

### _Sparsity of the document text matrix_
The document text matrices for each n-gram contain the frequency of the n-gram in the corpus broken down by document. It is interesting to see that the matrices are sparse, i.e. many entries are zero. This implies that many n-grams are not present in all three text files that are the documents in the corpus.


```
## $unigram
## <<DocumentTermMatrix (documents: 3, terms: 104130)>>
## Non-/sparse entries: 156951/155439
## Sparsity           : 50%
## Maximal term length: 109
## Weighting          : term frequency (tf)
## 
## $bigram
## <<DocumentTermMatrix (documents: 3, terms: 1108211)>>
## Non-/sparse entries: 1322319/2002314
## Sparsity           : 60%
## Maximal term length: 118
## Weighting          : term frequency (tf)
## 
## $trigram
## <<DocumentTermMatrix (documents: 3, terms: 2292149)>>
## Non-/sparse entries: 2439615/4436832
## Sparsity           : 65%
## Maximal term length: 121
## Weighting          : term frequency (tf)
## 
## $quadgram
## <<DocumentTermMatrix (documents: 3, terms: 2785385)>>
## Non-/sparse entries: 2827127/5529028
## Sparsity           : 66%
## Maximal term length: 126
## Weighting          : term frequency (tf)
```

Although sparse, the matrices do take significant space in memory:




Table: Memory requirement of n-gram dictionaries

n.gram      size.MB 
---------  ---------
unigram       8.4   
bigram       89.9   
trigram      195.7  
quadgram     253.0  

Looking at the cummulative frequency distribution of unigrams I can see that the first 25,000 most frequent words in the dictionary compose over 90% of the text. Considering that there are over 100,000 words in our list of unigrams this means that we only need to know 25% of the dictionary to build over 90% of the text! This can help in reducing the memory requirements of our dictionary.  Moreover, it suggests that 75% of the words in the dictionary are less than 10% likely to follow the trigram input by the user.





<img src="MilestoneReport_files/figure-html/cum_sum_unigram-1.png" style="display: block; margin: auto;" />

Unfortunately similar savings are not possible with other n-grams. For example the cummulative distribution becomes more linear with higher order n-grams. For quadgrams it is almost a straight line:

<img src="MilestoneReport_files/figure-html/cum_sum_quadgram-1.png" style="display: block; margin: auto;" />

### _Frequency distribution of the unigrams --- the dictionary_

The frequency distribution of n-grams is important. Unigrams (n-grams where n=1) form the dictionary for our n-gram model of the language. Higher order n-grams are built from this dictionary.

Looking at the unigrams I can identify the most frequent words. It is not surprising that the most common words are stop words!


```
## [1] "Most frequent word is * the * with frequency:  140415"
```

```
## [1] "Words with frequency within 10% of most frequent word:"
```

```
##  [1] "a"    "and"  "are"  "as"   "at"   "be"   "but"  "for"  "have" "i"   
## [11] "in"   "is"   "it"   "my"   "of"   "on"   "that" "the"  "this" "to"  
## [21] "was"  "with" "you"
```

<img src="MilestoneReport_files/figure-html/hist_unigram-1.png" style="display: block; margin: auto;" />

### _Frequency distribution of the trigrams --- the input_

The frequency distribution of trigrams tells me what is the most likely 3 word phrase that the user will type:


```
## [1] "Most frequent trigram is * one of the * with frequency:  1024"
```

```
## [1] "Trigrams with frequency within 30% of most frequent trigram:"
```

```
##  [1] "a lot of"           "as well as"         "be able to"        
##  [4] "going to be"        "i dont know"        "i have a"          
##  [7] "i have to"          "i want to"          "im going to"       
## [10] "it was a"           "looking forward to" "one of the"        
## [13] "out of the"         "part of the"        "some of the"       
## [16] "thanks for the"     "the end of"         "the first time"    
## [19] "the rest of"        "to be a"
```

<img src="MilestoneReport_files/figure-html/hist_trigram-1.png" style="display: block; margin: auto;" />

### _Frequency distribution of the quadgrams --- the guess_

The frequency of distribution of quadgrams (n-grams with n=4) is of utmost interest. This is because quadgrams are at the heart of our first guess to the word that would follow the three words (i.e. the trigram) typed by our user. A good choice for the missing word would be the last word in a quadgram that begins with the input trigram. 


```
## [1] "Most frequent quadgram is * the end of the * with frequency:  246"
```

```
## [1] "Quadgrams with frequency within 40% of most frequent quadgram:"
```

```
##  [1] "at the end of"         "at the same time"     
##  [3] "cant wait to see"      "for the first time"   
##  [5] "going to be a"         "i dont want to"       
##  [7] "in the middle of"      "is going to be"       
##  [9] "is one of the"         "one of the most"      
## [11] "thanks for the follow" "thanks for the rt"    
## [13] "the end of the"        "the rest of the"      
## [15] "to be able to"         "when it comes to"
```

<img src="MilestoneReport_files/figure-html/hist_quadgram-1.png" style="display: block; margin: auto;" />

## Strategy for guessing

Smartphone programs that help users type input normally make multiple suggestions. Three suggestions is common. A trivial strategy to guess the upcoming word after the user types three words (i.e. after the input trigram) would be to suggest the three most frequent words in the dictionary (i.e. the top 3 unigrams). We can quickly get an estimate of the accuracy of this strategy by checking the fourth word in all quadgrams.


```
## [1] "The top three words in the unigram dictionary: "
```

```
## [1] "the" "to"  "and"
```

```
## [1] "Percent of success with simple strategy:  9.4%"
```

This means that in 9.4% of the quadgrams the last word is one of the three most frequent words in the dictionary, i.e. the unigram dictionary.

An improvement over this strategy would be to look at quadgrams that start with the input trigram and return the fourth word in the quadgram. In the case of multiple options for the fourth word we could choose the three that are most common. However, there is a problem with this strategy.

What do I do if the input trigram is not in the dictionary of trigrams, and therefore, there is no quadgram that starts with the 3 input words?

The idea of [*backoff*](https://cxwangyi.wordpress.com/2010/07/28/backoff-in-n-gram-language-models/) is a possible solution to this problem. *Backoff* means that I reduce the input trigram to a bigram (by eliminating the first word on the left side in the case of the English language). Given this bigram I then search the dictionary of trigrams for phrase that start with the bigram, and return the third word as a guess. I can do this in a recurssion in the event that I again fail to find an adequate trigram. In the end, I fall back in guessing the most frequent unigrams when everything fails.

In the language of statistical text analysis, the backoff process is an example of _Laplacian smoothing_. The term smoothing stems from an effort to address gaps in the frequency distribution function of n-grams when sampling corpora. The sampling is finite, so low probability n-grams are unlikely to be detected in the sample, and they would be considered to have _zero_ probability of occurring. However, this is not true. Smoothing aims at filling these gaps.

In addition to _Laplacian smoothing_ there is _discounting_. In the process of discounting the missing probabilities are generated by shifting the frequency distribution function "to the left". This means that the observed probability of infrequent n-grams is applied to those that were never observed in the sample. An example of this method is the [Good-Turing](https://www.wikiwand.com/en/Good%E2%80%93Turing_frequency_estimation) algorithm.

The _backoff_ smoothing is considered by some to be superior to the _discounting_ smoothing. A simple implementation of the _backoff_ smoothing is the [_stupid backoff_](http://www.aclweb.org/anthology/D07-1090.pdf) described by Google, Inc. This seems a promising candidate to prototype in my shiny application.

## Appendix - Code


```r
# download
URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zip.file <- "CourseraSwiftKey.zip"
download.file(URL,destfile=zip.file,method="libcurl",mode="wb")
system2("unzip",c(zip.file))

text.dir <- "final/en_US"
setwd(text.dir)
text.files <- list.files() ; print(text.files)

library(stringr)
library(printr) # to delete: detach('package:printr',unload=TRUE)

# download file stats
setwd(text.dir)
arg <- c("-lh",text.files) ; size <- system2("ls",arg,stdout=TRUE)
lwb <- system2("wc",text.files,stdout=TRUE)
arg <- c("-m",text.files) ;  multi <- system2("wc",arg,stdout=TRUE)
arg <- c(text.files) ;  enc <- system2("file",arg,stdout=TRUE)

size <- sapply(str_split(size,boundary("word")),function(x) x[7])
c.single <- sapply(str_split(lwb[1:3],boundary("word")),function(x) x[3])
c.multi <- sapply(str_split(multi[1:3],boundary("word")),function(x) x[1])
words <- sapply(str_split(lwb[1:3],boundary("word")),function(x) x[2])
lines <- sapply(str_split(lwb[1:3],boundary("word")),function(x) x[1])
encoding <- sapply(str_split(enc,":"),function(x) str_trim(x[2]))

finfo <-data.frame("File"=text.files, "Size"=size, "Characters.Single"=c.single,
    "Characters.Multi"=c.multi, "Words"=words, "Lines"=lines, "Encoding"=encoding)

# print("Information on the corpus files from unix utilities: ls, wc, and file.")
knitr::kable(finfo, align=c("l","c","c","c","c","c","l"), caption="Raw Text File Characteristics")

# twitter line analysis
setwd(text.dir)
# maximum number of characters in a twitter file line and the corresponding line
arg <- "'BEGIN {mx=0; mxtxt=\"\"}; {n=length($0); if(mx<n){mx=n; mxtxt=$0}}; END {print mx; print mxtxt}'"
arg <- paste(arg," ","en_US.twitter.txt")

maxTwitTxt <- system2("awk",arg,stdout=TRUE)
print(
  paste("Maximum number of characters in twitter file twit (line): ",
        round(as.numeric(maxTwitTxt[1]))) )
print(paste("THE LINE IS:",maxTwitTxt[2]))

#### ACTIVATE ON FIRST RUN
# initial pre-processing of downloaded files
setwd(text.dir)

# converting files from UTF-8 to US-ASCII and from dos termination to unix
c1 <- "-s --from-code UTF-8 --to-code US-ASCII -c"
ascii.files <- c()
for(f in text.files){
  ts <- unlist(str_split(f,"[.]"))
  new.f <- paste("ascii",ts[2],ts[3],sep=".")
  ascii.files <- c(ascii.files, new.f)
  arg <- paste(c1,f," > ",new.f)
  if(!file.exists(new.f)) system2("iconv",arg)
}

unix.files <- c()
for(f in ascii.files){
  ts <- unlist(str_split(f,"[.]"))
  new.f <- paste("unix",ts[2],ts[3],sep=".")
  unix.files <- c(unix.files, new.f)
  arg <- paste("-f -n ",f,new.f)
  if(!file.exists(new.f)) {
    system2("dos2unix",arg)
    file.remove(f)
  }
}

# save pre-processing
setwd(text.dir)
for(f in unix.files){
  arg <- paste(f," ","../proc") # this directory must exist
  system2("mv",arg)
}
#### END ACTIVATE ON FIRST RUN

# working with pre-processed files in the unix.dir
unix.dir <- "final/proc"
setwd(unix.dir)
unix.files <- list.files(pattern="unix.*")
print("Performed file conversions using system utilities.")
print(paste("Converted files:",paste(unix.files,collapse=" ")))

# redo stats of files using pre-processed files
setwd(unix.dir)
text.files <- unix.files   # now the text files are the ascii, unix line terminated

arg <- c("-lh",text.files) ; size <- system2("ls",arg,stdout=TRUE)
lwb <- system2("wc",text.files,stdout=TRUE)
arg <- c("-m",text.files) ;  multi <- system2("wc",arg,stdout=TRUE)
arg <- c(text.files) ;  enc <- system2("file",arg,stdout=TRUE)

size <- sapply(str_split(size,boundary("word")),function(x) x[7])
c.single <- sapply(str_split(lwb[1:3],boundary("word")),function(x) x[3])
c.multi <- sapply(str_split(multi[1:3],boundary("word")),function(x) x[1])
words <- sapply(str_split(lwb[1:3],boundary("word")),function(x) x[2])
lines <- sapply(str_split(lwb[1:3],boundary("word")),function(x) x[1])
encoding <- sapply(str_split(enc,":"),function(x) str_trim(x[2]))

finfo <-data.frame("File"=text.files, "Size"=size, "Characters.Single"=c.single,
    "Characters.Multi"=c.multi, "Words"=words, "Lines"=lines, "Encoding"=encoding)

# print("Information on the corpus files from unix utilities: ls, wc, and file.")
knitr::kable(finfo, align=c("l","c","c","c","c","c","l"), caption="Converted Text File Characteristics")

# reanalyze twitter line
setwd(unix.dir)
# maximum number of characters in a twitter file line and the corresponding line
arg <- "'BEGIN {mx=0; mxtxt=\"\"}; {n=length($0); if(mx<n){mx=n; mxtxt=$0}}; END {print mx; print mxtxt}'"
arg <- paste(arg," ","unix.twitter.txt")

maxTwitTxt <- system2("awk",arg,stdout=TRUE)
print(
  paste("Maximum number of characters in twitter file twit (line): ",
        round(as.numeric(maxTwitTxt[1]))) )
print(paste("THE LINE IS:",maxTwitTxt[2]))

# now generate a clean corpus using tm
print(text.files) # the unix.xx.txt files are the new text.files

# load resources
source("../nlpTools.R")

# ACTIVATE ON FIRST RUN
# sampling the pre-processed files, building a corpus, and filtering to clean it
setwd(unix.dir)
pcent <- 0.03 # percent to sample
set.seed(1537) # this will make it the same all the time.
s.text <- lapply(pcent,sampleText,text.files)
print("Text files sampled!")

# Building corpus from sample files generated by sampleText function.
corpus <- lapply(s.text,buildCorpus)
print("Corpus built using tm!")

# release some memory
rm(s.text)

clean.corpus <- lapply(corpus,purify.corpus)
#  put in file names
for(j in seq(clean.corpus)){
  for(k in seq(clean.corpus[[j]])){ 
    meta(clean.corpus[[j]][[k]],"id") <- text.files[k]
  }
}

print(paste("Filtered Corpus Directory Files: ",paste0(text.files,collapse=", ")))
print("Saving raw corpus and clean corpus.")
if(file.exists("corpus.r")){
  file.remove("corpus.r")
}
save(corpus,clean.corpus,file="corpus.r")

# Second, Generate DTMS for clean corpus
print("Generating Document Text Matrix for clean corpus.")
dtms <- lapply(clean.corpus,buildDTM) # using default 4-gram as maximum
for(k in seq_along(clean.corpus)){
  names(dtms[[k]])<- c("unigram","bigram","trigram","quadgram")
}

print("Saving Document Text Matrix.")
if(file.exists("dbdtms.r")){
  file.remove("dbdtms.r")
}
save(dtms,file="dbdtms.r")

# Second, Generate TDMS for clean corpus
print("Generating Term Document Matrix for clean corpus.")
tdms <- lapply(clean.corpus,buildTDM) # using default 4-gram as maximum
for(k in seq_along(clean.corpus)){
  names(tdms[[k]])<- c("unigram","bigram","trigram","quadgram")
}

print("Saving Document Text Matrix.")
if(file.exists("dbtdms.r")){
  file.remove("dbtdms.r")
}
save(tdms,file="dbtdms.r")
## END OF ACTIVATE ON FIRST PASS

# Exploratory analysis - document text matrix
setwd(unix.dir)
# read the Document Text Matrices and the Term Document Matrices
load("dbdtms.r")
print(dtms[[1]])
# the dtms contain one entry per sample of text. We generated 1 sample.
# For each sample we have the dtm for unigrams, bigrams, trigrams, and quadgrams

library(pryr) # to use object_size, instead of object.size
t <- lapply(dtms[[1]],object_size)
t.df <- data.frame(n.gram=names(t),size.MB=round(c(t$unigram,t$bigram,t$trigram,t$quadgram)/1048576,digits=1))
knitr::kable(t.df,align=c("l","c"),caption="Memory requirement of n-gram dictionaries")

# Exploratory analysis - looking at cummulative frequencies of unigrams, quadgrams
# document text matrices for different n-grams
dtm.unigram <- dtms[[1]]$unigram
dtm.bigram <- dtms[[1]]$bigram
dtm.trigram <- dtms[[1]]$trigram
dtm.quadgram <- dtms[[1]]$quadgram

# frequencies
fr.unigram <- colSums(as.matrix(dtm.unigram))
fr.bigram <- colSums(as.matrix(dtm.bigram))
fr.trigram <- colSums(as.matrix(dtm.trigram))
fr.quadgram <- colSums(as.matrix(dtm.quadgram))

library(ggplot2)

# looking at cummulative distribution of unigrams
fr.unigram.sorted <- sort(fr.unigram,decreasing=TRUE)
fcs.unigram <- cumsum(fr.unigram.sorted)/sum(fr.unigram.sorted)
fcs.unigram.df <- data.frame(cum.sum=fcs.unigram,
                             fraction=(seq(1,length(fcs.unigram)))/length(fcs.unigram))
fcs.unigram.plot <- ggplot(fcs.unigram.df,aes(x=fraction,y=cum.sum))
fcs.unigram.plot <- fcs.unigram.plot + geom_point(color="lightblue") + ggtitle("Unigrams") + xlab("% of all unigrams") + ylab("% of Cummulative Frequency")
print(fcs.unigram.plot)

# cummulative frequency distribution of quadgrams
fr.quadgram.sorted <- sort(fr.quadgram,decreasing=TRUE)
fcs.quadgram <- cumsum(fr.quadgram.sorted)/sum(fr.quadgram.sorted)
fcs.quadgram.df <- data.frame(cum.sum=fcs.quadgram,
                         fraction=(seq(1,length(fcs.quadgram)))/length(fcs.quadgram))
fcs.quadgram.plot <- ggplot(fcs.quadgram.df,aes(x=fraction,y=cum.sum))
fcs.quadgram.plot <- fcs.quadgram.plot + geom_point(color="lightblue") + ggtitle("Quadgrams") + xlab("% of all unigrams") + ylab("% of Cummulative Frequency")
print(fcs.quadgram.plot)

# Exploratory Analysis - Frequencies of unigrams, trigrams, and quadgrams
# getting most frequent word.
ord <- order(fr.unigram,decreasing=TRUE)
mostFreq.word <-  names(fr.unigram[ord[1]])
print(paste("Most frequent word is *",mostFreq.word,
            "* with frequency: ",fr.unigram[mostFreq.word]))

# Most frequent words:
# Within 10 percent of the most frequent word.
ngram.percent <- 0.10
fr.words <- findFreqTerms(dtm.unigram,lowfreq=round(ngram.percent*max(fr.unigram)))
print(paste("Words with frequency within",
            paste(round(100*ngram.percent),"%",sep=""),"of most frequent word:"))
print(fr.words)

# getting a histogram... of the most frequent words
fr.unigram.df <- data.frame(words=names(fr.unigram),frequency=fr.unigram)
h1.plot <- ggplot(subset(fr.unigram.df,
                         fr.unigram >= round(ngram.percent*max(fr.unigram))),
                  aes(x=reorder(words,-frequency),y=frequency))
h1.plot <- h1.plot + geom_bar(stat="Identity",fill="lightblue")
h1.plot <- h1.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h1.plot <- h1.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1)) +
  ggtitle("Frequency histogram of common unigrams")
print(h1.plot)

# getting most frequent trigram.
ordr <- order(fr.trigram,decreasing=TRUE)
mostFreq.trigram  <- names(fr.trigram[ordr[1]])
print(paste("Most frequent trigram is *",mostFreq.trigram,
            "* with frequency: ",fr.trigram[mostFreq.trigram]))

# Most frequent trigrams:
# Within 30 percent of the most frequent trigram
ngram.percent <- 0.3
fr.3words <- findFreqTerms(dtm.trigram,lowfreq=round(ngram.percent*max(fr.trigram)))
print(paste("Trigrams with frequency within",
            paste(round(100*ngram.percent),"%",sep=""),"of most frequent trigram:"))
print(fr.3words)

# getting a histogram... of the most frequent trigrams
fr.trigram.df <- data.frame(words=names(fr.trigram),frequency=fr.trigram)
h3.plot <- ggplot(subset(fr.trigram.df,fr.trigram >= round(ngram.percent*max(fr.trigram))),
                 aes(x=reorder(words,-frequency),y=frequency))
h3.plot <- h3.plot + geom_bar(stat="Identity",fill="lightblue")
h3.plot <- h3.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h3.plot <- h3.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1)) +
  ggtitle("Frequency histogram of common trigrams")
print(h3.plot)

# getting most frequent quadgram.
ordr <- order(fr.quadgram,decreasing=TRUE)
mostFreq.quadgram  <- names(fr.quadgram[ordr[1]])
print(paste("Most frequent quadgram is *",mostFreq.quadgram,
            "* with frequency: ",fr.quadgram[mostFreq.quadgram]))

# Most frequent quadgrams:
# Within 40 percent of the most frequent quadgram
ngram.percent <- 0.4
fr.4words <- findFreqTerms(dtm.quadgram,lowfreq=round(ngram.percent*max(fr.quadgram)))
print(paste("Quadgrams with frequency within",
            paste(round(100*ngram.percent),"%",sep=""),"of most frequent quadgram:"))
print(fr.4words)

# getting a histogram... of the most frequent quadgrams
fr.quadgram.df <- data.frame(words=names(fr.quadgram),frequency=fr.quadgram)
h4.plot <- ggplot(subset(fr.quadgram.df,fr.quadgram >= round(ngram.percent*max(fr.quadgram))),
                  aes(x=reorder(words,-frequency),y=frequency))
h4.plot <- h4.plot + geom_bar(stat="Identity",fill="lightblue")
h4.plot <- h4.plot + geom_text(aes(label=frequency),vjust=-0.2,size=2)
h4.plot <- h4.plot + theme(axis.title.x=element_blank(),
                           axis.text.x=element_text(angle=45,hjust=1)) +
  ggtitle("Frequency histogram of common quadgrams")
print(h4.plot)

# Exploring success of naive guess strategy
ordr <- order(fr.unigram,decreasing=TRUE)
top.3uni <- names(fr.unigram[ordr[1:3]])

print("The top three words in the unigram dictionary: ")
print(top.3uni)

quads <- names(fr.quadgram)
last.word.in.quad <- unlist(getLastWord(quads))
pcnt <- 100*mean(last.word.in.quad %in% top.3uni)
pcnt <- round(pcnt,digits=1)
pcnt.str <- paste(pcnt,"%",sep="")
print(paste("Percent of success with simple strategy: ", pcnt.str))
```

## Appendix - Utilities


```r
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
  toStr(lapply(words,function(s) { n<-length(s); s[-(1:(n-1))] }))
}
dropFirstWord <- function(ngram) {
  words <- toWords(ngram)
  toStr(lapply(words,function(s) { n<-length(s); s[2:n] }))
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
```
