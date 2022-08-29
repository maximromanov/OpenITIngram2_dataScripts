################################################################################
# DESCRIPTION ##################################################################
################################################################################

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

# SINGLE CORE SCRIPT

# TO RUN: Rscript script.R local numCores ngramType
# TO RUN: Rscript script.R remote numCores ngramType

args <- commandArgs(trailingOnly=TRUE)

where        <- args[1] # `remote` or `local` # where <- "local"
numCores     <- as.integer(args[2]) # 10, 20, 30
ngramType    <- as.integer(args[3]) # 1, 2, or 3 # ngramType <- 3

registerDoParallel(numCores)
print(paste0("Number of activated cores: ", numCores))

source("00_functions.R")

print("################################################################################")
print("> 6. preparing ngram-per-book data #############################################")
print("################################################################################")
print("################################################################################")

# setwd("/Users/romanov/_ADHFAIS_Data_TEMP/app_NgramReaderNew_v1/")
saveFolder <- paste0(finalDataFolder, "github/ngrams_per_book/")

dir.create(paste0(finalDataFolder, "github/"))
dir.create(paste0(finalDataFolder, "github/ngrams_per_book/"))

ngFilePattern <- paste0("data_", ngramType, "gram.*")
print(ngFilePattern)
files <- list.files(finalDataFolder, pattern = ngFilePattern)

print(paste0("The following files from the folder will be processed: ", finalDataFolder))
print(files)

ngramIDs <- ""
pb <- progress_bar$new(total = length(files))

print(">>> Loading NGRAM IDs...")
for (f in files){
  pb$tick()
  tempData <- readRDS(paste0(finalDataFolder, f))
  
  ifelse(
    ngramIDs == "",
    ngramIDs <- tempData,
    ngramIDs <- ngramIDs %>%
      add_row(tempData)
  )
}

print(ngramIDs)

ngramIDs <- ngramIDs %>%
  mutate(mainGroup = plyr::round_any(ID, 50000, f = ceiling) / 1000)

mainGroups <- ngramIDs$mainGroup %>% unique()
print(mainGroups)

filePattern <- paste0("temp_", ngramType, "grams.*")
filesToProcess <- list.files(tempFolder, pattern = filePattern)
print(filesToProcess)

print("MULTIPROCESSING STARTS HERE")

foreach (n = 1:length(mainGroups)) %dopar% { # DOPAR LOOP
  mainGroupVar <- mainGroups[n]              # DOPAR LOOP
  nodeID <- paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
  #for (mainGroup in mainGroups) {                     # REGULAR LOOP
  #  mainGroupVar <- mainGroup                         # REGULAR LOOP
  ngramSubset <- ngramIDs %>% filter(mainGroup == mainGroupVar) %>%
    mutate(ID = as.character(ID)) %>% ungroup()
  
  dataToSave <- ""
  
  for (f in filesToProcess){
    bookURIvar <- str_replace_all(f, "\\.tsv", "")
    
    tempFile <- paste0(tempFolder, f)
    print(paste0(nodeID, " >>> processing: ", tempFile))
    
    temp <- read_delim(tempFile, col_types = cols(), delim = "\t") %>%
      left_join(ngramSubset, by = c("ngramTR" = "ngramTR")) %>%
      rename(ngramID = ID) %>%
      filter(!is.na(ngramID))
    
    if (nrow(temp) != 0){
      ifelse(
        dataToSave == "",
        dataToSave <- temp,
        dataToSave <- dataToSave %>% add_row(temp)
      )
    } else {
      print(">>> No relevant results in this file...")
    }
  }
  print(dataToSave)

  print(paste0(nodeID, " >>> Splitting results into small files..."))
  print(dataToSave)
  
  dataToSave <- dataToSave %>%
    mutate(fileName = idToFnameBooks(ngramType, as.integer(ngramID))) %>%
    select(ngramID, count, bookID, fileName)
  print(dataToSave)
  
  smallFilesToSave <- dataToSave$fileName %>% unique()
  
  for (smFile in smallFilesToSave){
    print(paste0(nodeID, " >>> saving a small file: ", smFile))
    
    dataWide <- dataToSave %>% filter(fileName == smFile) %>%
      select(-fileName) %>% filter(count > 1)
    
    print(dataWide)
    
    pathToSave <- paste0(saveFolder, smFile)
    saveRDS(dataWide, pathToSave)
    print(paste0(nodeID, " >>> results are saved into: ", pathToSave))
    
  }
}

print("################################################################################")
print("> THE FILES HAVE BEEN PROCESSED ################################################")
print("################################################################################")