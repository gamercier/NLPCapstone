# Captone Project
# File: loadTools.R
# Loads libraries for the project

print("Loading packages...")
# library(NLP) # loaded with tm
# library(RWeka) # probably loaded with tm
# library(SnowballC) # loaded with tm
# library(openNLP) # loaded with NLP
# library(magrittr) # loaded with plyr
# library(plyr)
library(stringi) # for stri_extract_all_words, stri_extract_first/last_words, stri_count_words
library(stringr) # using stringr, a wrapper over stringi
library(tm)
library(wordcloud)
library(RColorBrewer)

library(ggplot2)

# wordnet is special
# source: https://wordnet.princeton.edu/
# Version 3.1 installed using homebrew package manager for Mac OSX
# wnhome <- "/usr/local/Cellar/wordnet/3.1/dict"
# options(warn=-1) # turn off warning fixed with last two statements
# library(wordnet)
# options(warn=0)

# setDict(wnhome)
# initDict(wnhome)

print("Finish loading packages!")