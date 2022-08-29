################################################################################
# DESCRIPTION ##################################################################
################################################################################

################################################################################
# LIBRARIES ####################################################################
################################################################################

library(tidyverse)
library(tidytext)

library(readr)

#library(foreach)
#library(doParallel)

################################################################################
# ARGUMENTS ####################################################################
################################################################################

# SINGLE CORE SCRIPT

# TO RUN: Rscript script.R local ngramType culling
# TO RUN: Rscript script.R remote ngramType culling

args <- commandArgs(trailingOnly=TRUE)

where     <- args[1] # `remote` or `local` # where <- "local"
ngramType <- as.integer(args[2]) # 1, 2 or 3
culling   <- as.integer(args[3]) # 3, 4, 5, etc. (data is already culled with parameter 2)

#registerDoParallel(coresFiles)
#print(paste0("Number of activated cores: ", coresFiles))

source("00_functions.R")

print("################################################################################")
print("> 3. AGGREGATING PERIOD-FILES ... ##############################################")
print("################################################################################")
print("################################################################################")

#testFolder <- "/Users/romanov/_ADHFAIS_Data_TEMP/app_NgramReaderNew_v1/freqLists/"
#files <- list.files(testFolder, pattern = "^\\d\\d\\d\\d.*\\.tsv")

patterRE <- paste0("data_Period25AH_", ngramType, "gram_.*2up\\.tsv")
files <- list.files(tempFolder, pattern = patterRE)

aggregatedNgrams <- ""

for (f in files){
  print(paste0(">>> processing file: ", f))
  tempPath <- paste0(tempFolder, f)
  tempData <- read_delim(tempPath, col_types = cols(), delim = "\t") %>% filter(count >= culling)
  
  ifelse(
    aggregatedNgrams == "",
    aggregatedNgrams <- tempData,
    aggregatedNgrams <- aggregatedNgrams %>% add_row(tempData)
  )
}

print(">>> recalculating frequencies...")
aggregatedNgrams <- aggregatedNgrams %>%
  group_by(ngram, n) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  select(ngram, n, count)

aggregatedNgramsFile <- paste0(tempFolder, "aggregated__", ngramType, "gram__", culling, "up.tsv")
write_delim(aggregatedNgrams, aggregatedNgramsFile, delim = "\t")
print(paste0(">>> file saved: ", aggregatedNgramsFile))

print("################################################################################")
print("> THE FILES HAVE BEEN PROCESSED ################################################")
print("################################################################################")
