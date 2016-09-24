# Capstone Project
# File: makeAppDB.R
# Takes databases used here and transforms them for use in the wordGuessApp

# Move to dbs directory and load the database to process
prj.dir <- file.path(Sys.getenv("HOME"),"git","NLPCapstone")
print(paste("Moving to",prj.dir)) 
setwd(prj.dir)
print(paste("Current directory to",getwd()))

##### Load the scores database and set the global variables
source("toDbs.R")
dir()

### Set choice.dir under dbs.dir -- REQUIRED
choice.dir <- "75.dir"     ## sampling 75% of corpus
choice.sub.dir <- "95.dir" ## trimming to keep 95% of vocabulary
scores.dir <- file.path(dbs.dir,choice.dir,choice.sub.dir)
setwd(scores.dir)
print(paste("Current directory:",getwd()))
dir()

### Set scores.file under dibs.dir/choice.dir -- REQUIRED
scores.file <- "scoresSB.trimmed.dense.r"
load(scores.file)
print(paste("Loading scores file",scores.file))
print(paste("Loaded scores database:",
            ls(pattern="^(bases|ngrams|scores)\\.[a-zA-Z0-9\\.]*db")))

### Set scoresDB, basesDB, TOP.UNI.SCORES, ALPHA
db.element <- "seventyfive.pct"  ### reflects choice.dir

scoresDB <-  scores.trimmed.dense.db[[db.element]]
# ngramsDB <-  ngrams.trimmed.dense.db[[db.element]] # don't need it
basesDB  <-  bases.trimmed.dense.db[[db.element]] 
TOP.UNI.SCORES <- scoresDB$unigram[1:3] # presumed to be sorted with higher score first
ALPHA <- 0.4

# save it
app.dir <- "~/git/nextWordApp"
save.file <- file.path("~","git","nextWordApp","nextWordDB.r")
if(file.exists(save.file)){
  file.remove(save.file)
}

save(scoresDB,basesDB,TOP.UNI.SCORES,ALPHA,file=save.file)
print(paste("Saved nextWord! App database in",save.file))
print("Finished processing makeAppDB.R")

