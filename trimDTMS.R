# Capstone Project
# File: trimDTMS.R
#   Trims the Document Text Matrix by reducing the sparsity

print("Started script: trimDTM.R")
# Got to project directory and load tools
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
setwd(prj.dir)
print(paste("Current directory: ",getwd()))

# load resources
source("nlpTools.R")
print("Loaded tools.")

#Switch to processed corpus directory
source("toProcCorpusDir.R")
print(paste("Switched to diretory",getwd()))

#Select document text matrix directory. THIS IS REQUIRED
dtms.dir <- c("../100.dir")
print(paste("Going to directory containing document text matrix: ",dtms.dir))
setwd(dtms.dir)
print(paste("Switched to diretory",getwd()))

# Loading document text matrices
load("dtms.r")
print("Loaded document text matrices")

trimDTM <- function(dtm,sparsity=0.33){
  dtm.dense <- list()
  n <- length(dtm)
  dtm.dense[1:n] <- lapply(dtm[1:n],removeSparseTerms,sparsity)
  names(dtm.dense) <- names(dtm)
  return(dtm.dense)
}

dtms.dense <- lapply(dtms,trimDTM)

print("Saving dense dtms, etc...")
if(file.exists("dtmsDense.r")){
  file.remove("dtmsDense.r")
}
save(dtms.dense,file="dtmsDense.r")
print("Finished saving dtms.dense stuff.")

print("Completed trimDTMS.R")

print("Resetting to project directory.")
setwd(prj.dir)
