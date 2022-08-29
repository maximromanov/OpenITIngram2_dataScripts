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
print("> 5. preparing chronological data ... ##########################################")
print("################################################################################")
print("################################################################################")

# setwd("/Users/romanov/_ADHFAIS_Data_TEMP/app_NgramReaderNew_v1/")
saveFolder <- paste0(finalDataFolder, "github/ngrams_per_period/")

dir.create(paste0(finalDataFolder, "github/"))
dir.create(paste0(finalDataFolder, "github/ngrams_per_period/"))

ngFilePattern <- paste0("data_", ngramType, "gram.*")
print(ngFilePattern)
files <- list.files(finalDataFolder, pattern = ngFilePattern)

print(paste0("The following files from the follder will be processed: ", finalDataFolder))
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
  #mutate(fileName = idToFname("chronoLARGE__", ngramType, ID))
  mutate(fileName = plyr::round_any(ID, 100000, f = ceiling) / 1000) 

filesToSave <- ngramIDs$fileName %>% unique()
print(filesToSave)

filePatternNgrams <- paste0("data_Period25AH_", ngramType, "gram__.*\\.tsv")
print(filePatternNgrams)
filesToProcess <- list.files(tempFolder, pattern = filePatternNgrams)
print(filesToProcess)

print("MULTIPROCESSING STARTS HERE")

foreach (n = 1:length(filesToSave)) %dopar% { # DOPAR LOOP
  chronoNgramFile <- filesToSave[n]           # DOPAR LOOP
  nodeID <- paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
  #for (file in filesToSave) {                    # REGULAR LOOP
  #  chronoNgramFile <- file                      # REGULAR LOOP
  ngramSubset <- ngramIDs %>% filter(fileName == chronoNgramFile) %>%
    mutate(ID = as.character(ID)) %>% ungroup()
  
  dataToSave <- ""
  
  for (f in filesToProcess){
    periodVar <- str_replace_all(f, pattern = "data_Period25AH_\\dgram__|AH_\\dup.tsv", "")
    tempFile <- paste0(tempFolder, f)
    print(paste0(nodeID, " >>> processing: ", tempFile))
    
    temp <- read_delim(tempFile, col_types = cols(), delim = "\t") %>%
      filter(count > 4) %>%
      mutate(ngramTR = str_replace_all(ngram, arabicToTranslit)) %>%
      left_join(ngramSubset, by = c("ngramTR" = "ngramTR")) %>%
      mutate(period = as.integer(periodVar)) %>%
      filter(!is.na(ID))
    
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
  
  print(paste0(nodeID, " >>> Splitting results into small files..."))
  print(dataToSave)
  dataToSave <- dataToSave %>% select(-fileName) %>%
    mutate(fileName = idToFnameChrono(ngramType, as.integer(ID)))
  
  smallFilesToSave <- dataToSave$fileName %>% unique()
  
  for (smFile in smallFilesToSave){
    print(paste0(nodeID, " >>> converting to wide format: ", smFile))
    
    dataWide <- dataToSave %>% filter(fileName == smFile) %>%
      select(ID, ngram, ngramTR, fileName, count, period) %>%
      tidyr::pivot_wider(names_from = period, values_from = count) %>%
      select(-ngramTR, -fileName)
    
    print(dataWide)
    
    pathToSave <- paste0(saveFolder, smFile)
    saveRDS(dataWide, pathToSave)
    print(paste0(nodeID, " >>> results are saved into: ", pathToSave))
    
  }
}

print("################################################################################")
print("> THE FILES HAVE BEEN PROCESSED ################################################")
print("################################################################################")