# Capstone Project
# File: toBenchDir.R
# Set working directory to the benchmark data directory

prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
download.dir <- "nlpData.dir"; bench.dir <- "bench"
bench.dir <- file.path(prj.dir,download.dir,bench.dir)
setwd(bench.dir)
print(paste("Current directory: ",getwd()))
