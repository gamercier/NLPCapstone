# Captone Project
# File: fixRawData.R
# checks and fixes raw text files.
# Uses OS dependent system calls

print("Started script: fixRawData.R")

# Got to data directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))
source("toCorpusDir.R")

library(stringr)
# source("helpers.R") # for read lines function

#### Working with raw corpus
####
text.files <- dir(corpus.dir) #corpus.dir set in toCorpusDir.R
print(paste("Corpus Database Files: ",paste0(text.files,collapse=", ")))

# system specific commands
print("Using shell commands to get information on the corpus files.")

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

print(format(
  paste(finfo$File,finfo$Size,finfo$Characters.Single,finfo$Words,finfo$Lines),
  justify=c("right")))
print(format(
  paste(finfo$File,finfo$Characters.Single,finfo$Characters.Multi),justify=c("right")))
print(paste(finfo$File,":",finfo$Encoding,sep=""))

# peeking at the file
sample.txt <- system2("head","-n 3 en_US.blogs.txt",stdout=TRUE)
print("File sample: en_US.blogs.txt")
print(sample.txt)

# maximum number of characters in a twitter file line and the corresponding line
arg <- "'BEGIN {mx=0; mxtxt=\"\"}; {n=length($0); if(mx<n){mx=n; mxtxt=$0}}; END {print mx; print mxtxt}' en_US.twitter.txt"
maxTwitTxt <- system2("awk",arg,stdout=TRUE)
print(
  paste("Maximum number of characters in twitter file: ",
        round(as.numeric(maxTwitTxt[1]))) )

print("The line:")
print(maxTwitTxt[2])

#### Conversions
print("Performing file conversions using system utilities.")

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
  arg <- paste("-f -n ",f,new.f) # to force change even if apparent binary code found
  if(!file.exists(new.f)) system2("dos2unix",arg)
}

# remove ascii.xx.txt files. Not needed.
print("Removing ascii.xx.txt files.")
file.remove(ascii.files)

#### Working with processed corpus
####
text.files <- unix.files  # converted files
print(paste("Corpus Database Files: ",paste0(text.files,collapse=", ")))

# system specific commands
print("Using shell commands to get information on the modified corpus files.")

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

print(format(
  paste(finfo$File,finfo$Size,finfo$Characters.Single,finfo$Words,finfo$Lines),
  justify=c("right")))
print(format(
  paste(finfo$File,finfo$Characters.Single,finfo$Characters.Multi),justify=c("right")))
print(paste(finfo$File,":",finfo$Encoding,sep=""))

# peeking at the file
sample.txt <- system2("head","-n 3 unix.blogs.txt",stdout=TRUE)
print("File sample: unix.blogs.txt")
print(sample.txt)

# maximum number of characters in a twitter file line and the corresponding line
arg <- "'BEGIN {mx=0; mxtxt=\"\"}; {n=length($0); if(mx<n){mx=n; mxtxt=$0}}; END {print mx; print mxtxt}' unix.twitter.txt"
maxTwitTxt <- system2("awk",arg,stdout=TRUE)

print(
  paste("Maximum number of characters in twitter file: ",
        round(as.numeric(maxTwitTxt[1]))) )

print("The line:")
print(maxTwitTxt[2])

# #load the files and peek
# set.seed(3567)
# for(f in text.files){
#   # load
#   print("=======================================")
#   print(paste("Reading file: ",f))
#   corpus <- read.txt(f)
#   n <- length(corpus) # number of lines
#   nr <- round(runif(1,1,n))
#   # peak
#   print(paste("****Random sample. Line no.: ",nr," in file: ",f,"****"))
#   print(paste(corpus[nr]))
# }

print("Returning to main directory!")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

print("Script, fixRawData.R, successfully finished!")

