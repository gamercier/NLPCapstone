# fixRawBench.R
# Take raw text files to generate benchmark data and fixs it
# Deletes files that are not UTF-8 or ASCII
# Converts files to US-ASCII
# Converts line terminations to Unix standard
# Final files are named unix.xx.txt

# Data for this is created from a sample out of the
# http://www.anc.org/OANC/OANC_GrAF.zip
# This is from the American National Corpus: http://www.anc.org/
# Wikipedia: https://www.wikiwand.com/en/American_National_Corpus

# Got to project directory and move to bench directory
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

source("toBenchDir.R")

library(stringr)

#### Fixing raw corpus to ASCII Unix files ####

f.info <- function(dir.name){
  text.files <- dir(dir.name)
  #print(paste(dir.name," Files: ",paste0(text.files,collapse=", ")))
  # system specific commands
  print("Using shell commands to get information on the bench files.")
  
  arg <- c("-lh",text.files) ; size <- system2("ls",arg,stdout=TRUE)
  lwb <- system2("wc",text.files,stdout=TRUE);
  lwb <- lwb[-length(lwb)] # gets rid of the total
  arg <- c("-m",text.files) ;  multi <- system2("wc",arg,stdout=TRUE)
  multi <- multi[-length(multi)] # gets rid of the total
  arg <- c(text.files) ;  enc <- system2("file",arg,stdout=TRUE)
  
  # modified
  size <- sapply(str_split(size,boundary("word")),function(x) x[7])
  c.single <- sapply(str_split(lwb,boundary("word")),function(x) x[3])
  c.multi <- sapply(str_split(multi,boundary("word")),function(x) x[1])
  words <- sapply(str_split(lwb,boundary("word")),function(x) x[2])
  lines <- sapply(str_split(lwb,boundary("word")),function(x) x[1])
  encoding <- sapply(str_split(enc,":"),function(x) str_trim(x[2]))
  
  finfo <-data.frame("File"=text.files, "Size"=size, "Characters.Single"=c.single,
                     "Characters.Multi"=c.multi, "Words"=words,
                     "Lines"=lines, "Encoding"=encoding, stringsAsFactors = FALSE)
  return(finfo)
}

pr.finfo <- function(finfo){
  print(format(
    paste(finfo$File,finfo$Size,finfo$Characters.Single,finfo$Words,finfo$Lines),
    justify=c("right")))
  print(format(
    paste(finfo$File,finfo$Characters.Single,finfo$Characters.Multi),justify=c("right")))
  print(paste(finfo$File,":",finfo$Encoding,sep=""))
}

finfo <- f.info(bench.dir)
pr.finfo(finfo=finfo)

# peeking at the file
for(f in text.files){
  sample.txt <- system2("head",paste("-n 3",f),stdout=TRUE)
  print(paste("File sample: ",f))
  print(sample.txt)
}

#### Conversions
print("Performing file conversions using system utilities.")

# converting files from UTF-8 to US-ASCII and from dos termination to unix
pick.utf8 <- grepl("UTF-8",finfo$Encoding,fixed=TRUE)
pick.ascii <- grepl("ASCII",finfo$Encoding,fixed=TRUE)
pick.other <- !mapply(any,pick.utf8,pick.ascii) #these are not true text files

utf.files <- finfo[pick.utf8,"File"]
ascii.files <- finfo[pick.ascii,"File"]
other.files <- finfo[pick.other,"File"]

# presumably other are not truely text files; so delete
file.remove(other.files)

c1 <- "-s --from-code ASCII --to-code US-ASCII -c"
for(f in ascii.files){
  new.f <- paste("ascii.",f,sep="")
  arg <- paste(c1,f," > ",new.f)
  if(!file.exists(new.f)){
    system2("iconv",arg)
  }
}

c1 <- "-s --from-code UTF-8 --to-code US-ASCII -c"
for(f in utf.files){
  new.f <- paste("ascii.",f,sep="")
  arg <- paste(c1,f," > ",new.f)
  if(!file.exists(new.f)){
    system2("iconv",arg)
  }
}

# update finfo
finfo <- f.info(bench.dir)

ascii.files <- finfo$File
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

# update finfo
finfo <- f.info(bench.dir)

print("Returning to main directory!")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

print("Script, fixRawCorpus.R, successfully finished!")
