################################################################################
# DESCRIPTION ##################################################################
################################################################################

# Processing files in the following manner:
# - grouping all data into periods (50 years)
#   - performing culling: supply as an argument
# - agregating into a single file after that
#   - the same script can be used for 1grams, 2grams, 3grams (just supply n as arg)

################################################################################
# LIBRARIES ####################################################################
################################################################################

library(tidyverse)
library(tidytext)

library(readr)

library(foreach)
library(doParallel)

################################################################################
# ARGUMENTS ####################################################################
################################################################################

# TO RUN: Rscript script.R local numCores ngramType
# TO RUN: Rscript script.R remote numCores ngramType

args <- commandArgs(trailingOnly=TRUE)

where     <- args[1] # `remote` or `local` # where <- "local"
numCores  <- as.integer(args[2]) # 30 for unigrams, but less for 2grams and 3grams
ngramType <- as.integer(args[3]) # 1, 2 or 3

registerDoParallel(numCores)
print(paste0("Number of activated cores: ", numCores))

source("00_functions.R")

print("################################################################################")
print("> 2a. AGGREGATING 1gram into temp files ... ####################################")
print("################################################################################")
print("################################################################################")

#testFolder <- "/Users/romanov/_ADHFAIS_Data_TEMP/app_NgramReaderNew_v1/freqLists/"
#files <- list.files(testFolder, pattern = "^\\d\\d\\d\\d.*\\.tsv")

files <- list.files(freqListFolder, pattern = "^\\d\\d\\d\\d.*\\.tsv")

periodsTB <- as_tibble(files) %>%
  mutate(period = as.integer(substring(value, 1, 4))) %>%
  mutate(period = plyr::round_any(period, periodValueMain, f = ceiling)) %>%
  mutate(period = sprintf("%04d", period)) %>%
  rename(file = value)

periods <- periodsTB$period %>% unique()

foreach (n = 1:length(periods)) %dopar% {
  periodTemp <- periods[n]
  print(paste0(">>> Processing period: ", periodTemp))
  periodFileNameHI <- paste0(tempFolder, "data_", "Period", periodValueMain, "AH_", ngramType, "gram__", periodTemp, "AH_2up.tsv")
  
  if (!file.exists(periodFileNameHI)){
    
    filesListTB <- periodsTB %>% filter(period == periodTemp)
    filesList <- filesListTB$file
    ngramsTemp <- ""
    
    for (f in filesList){
      ### >>> AGGREGATING ngrams into a single list
      tempPath <- paste0(freqListFolder, f)
      tempData <- read_delim(tempPath, col_types = cols(), delim = "\t") %>% filter(n == ngramType) %>%
        mutate(letters = str_replace_all(ngram, arabicToTranslit)) %>%
        mutate(test = ifelse(str_detect(letters, "^[A-Za-z_]+$"), "TRUE", "FALSE")) %>%
        filter(test == "TRUE") %>% select(-letters, -test)
      ifelse(ngramsTemp == "", ngramsTemp <- tempData, ngramsTemp <- ngramsTemp %>% add_row(tempData))
    }

    ngramsTemp <- ngramsTemp %>%
      group_by(ngram, n) %>%
      summarise(count = sum(count)) %>%
      ungroup() %>%
      select(ngram, n, count)
    
    ngramsTempHI <- ngramsTemp %>%
      filter(count > 1)
    
    write_delim(ngramsTempHI, periodFileNameHI, delim = "\t")
    print(paste0("    >>> file saved: ", periodFileNameHI))

  } else {
    print(paste0("!!! >>> The files has already been processed:", periodFileNameHI))
  }
  
}

print("################################################################################")
print("> THE FILES HAVE BEEN PROCESSED ################################################")
print("################################################################################")
