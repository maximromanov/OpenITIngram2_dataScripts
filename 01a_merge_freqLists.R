################################################################################
# DESCRIPTION ##################################################################
################################################################################

# generating frequency list for each text in the corpus;
# these lists are temporary and will be filtered later, removing low freq items

################################################################################
# LIBRARIES ####################################################################
################################################################################

library(tidyverse)
library(tidytext)
library(progress)

library(readr)

library(foreach)
library(doParallel)

################################################################################
# ARGUMENTS ####################################################################
################################################################################

# TO RUN: Rscript 01_prepare_data_freqLists.R local numCores
# TO RUN: Rscript 01_prepare_data_freqLists.R remote numCores

args = commandArgs(trailingOnly=TRUE)

where    <- args[1] # `remote` or `local` # where <- "local"
numCores <- as.integer(args[2]) # numCores <- 3

registerDoParallel(numCores)
print(paste0("Number of activated cores: ", numCores))

source("00_functions.R")

################################################################################
# FUNCTIONS ####################################################################
################################################################################

metadataFile <- paste0(finalDataFolder, "metadata_bookID_bookURI_frozen.rds")
metadata <- readRDS(metadataFile)

files <- list.files(freqListFolder, pattern = "^\\d\\d\\d\\d.*\\.tsv")
set.seed(786) # to ensure that the split is exactly the same
files <- sample(files)

splitVectorIntoNChunks <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
filesList <- splitVectorIntoNChunks(files, 50)
#print(filesList)

print("################################################################################")
print("> MULTIPROCESSING STARTS HERE ##################################################")
print("################################################################################")


#for (n in 1:length(filesList[1:2])) {
foreach (n = 1:length(filesList)) %dopar% {    # DOPAR
  #pb <- progress_bar$new(total = length(filesList[[n]]))
  nodeID <- Sys.getpid()
  
  tempFileName <- paste0(tempFolder, sprintf("temp_ngrams_per_book_%05d", n), ".tsv")
  print(paste0(nodeID, ": ", tempFileName))
  
  ngramsTemp <- ""
  for (f in filesList[[n]]){
    #pb$tick()
    bookURIvar <- str_replace_all(f, "\\.tsv", "")
    tempPath <- paste0(freqListFolder, f)
    print(paste0("   >>> ", nodeID, ": ", tempPath))
    tempData <- read_delim(tempPath, col_types = cols(), delim = "\t") %>%
      mutate(ngramTR = str_replace_all(ngram, arabicToTranslit)) %>%
      mutate(test = ifelse(str_detect(ngramTR, "^[A-Za-z_]+$"), "TRUE", "FALSE")) %>%
      filter(test == "TRUE") %>% select(-ngram, -test) %>%
      mutate(bookURI = bookURIvar) %>%
     left_join(metadata, by = c("bookURI" = "bookURI")) %>%
     select(ngramTR, count, n, bookID)
    
    #print(tempData)
    
    ifelse(ngramsTemp == "", ngramsTemp <- tempData, ngramsTemp <- ngramsTemp %>% add_row(tempData))
  }
  
  print(ngramsTemp)
  
  #print(paste0(">>> saving a temp file with frequencies: ", tempFileName))
  #write_delim(ngramsTemp, tempFileName, delim = "\t")
  #print(paste0(">>> file saved: ", tempFileName))
  
  ngramsTempA <- ngramsTemp %>% filter(n == 1)
  tempFileName <- paste0(tempFolder, sprintf("temp_1gramsTR_per_book_%05d", n), ".tsv")
  write_delim(ngramsTempA, tempFileName, delim = "\t")
  print(paste0(">>> file saved: ", tempFileName))
  
  ngramsTempA <- ngramsTemp %>% filter(n == 2)
  tempFileName <- paste0(tempFolder, sprintf("temp_2gramsTR_per_book_%05d", n), ".tsv")
  write_delim(ngramsTempA, tempFileName, delim = "\t")
  print(paste0(">>> file saved: ", tempFileName))
  
  ngramsTempA <- ngramsTemp %>% filter(n == 3)
  tempFileName <- paste0(tempFolder, sprintf("temp_3gramsTR_per_book_%05d", n), ".tsv")
  write_delim(ngramsTempA, tempFileName, delim = "\t")
  print(paste0(">>> file saved: ", tempFileName))
  
}
