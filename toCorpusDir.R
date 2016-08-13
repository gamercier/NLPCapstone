# Capstone Project
# File: toCorpusDir.R
# Set working directory to the corpus directory

prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
download.dir <- "nlpData.dir"; sub.dir <- "final"; corpus.dir <- "en_US"
corpus.dir <- file.path(prj.dir,download.dir,sub.dir,corpus.dir)
setwd(corpus.dir)
print(paste("Current directory: ",getwd()))
