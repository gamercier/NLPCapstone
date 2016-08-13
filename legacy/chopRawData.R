# Captone Project
# File: chopRawData.R
# chops files to create smaller versions for analysis.

print("Started script: chopRawData.R")

prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

source("helpers.R")
source("toCorpusDir.R")

text.files <- c("en_US.blogs.txt", "en_US.new.txt", " en_US.twitter.txt")
print(paste("Corpus Database Files: ",paste0(text.files,collapse=", ")))

#partition files into chunks of 2% from original
percent = 0.02
n.samples = 1
for(f in text.files){
  # read entire file, sample lines, and output lines
  print(paste("Reading file: ",f))
  txt <- read.txt(f)
  fparts <- unlist(strsplit(f,split="[.]")) # process file name for write
  fbase <- paste0(fparts[1],".",fparts[2])
  print(paste("Sampling ",round(percent*100),"%"," of the file."))
  for(k in 1:(n.samples)){
    txt.kept <- txt[sample.int(length(txt),round(percent*length(txt)))]
    outf <- sprintf("%s_%02i.txt",fbase,k)
    if(file.exists(outf)){
      file.remove(outf)
    }
    writeLines(txt.kept,con=outf)
    print(paste("Wrote sample to file: ",outf))
  }
}

print("Returning to main directory!")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))
print("Script, chopRawData.R, successfully finished!")

