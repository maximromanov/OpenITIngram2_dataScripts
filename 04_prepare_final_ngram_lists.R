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

# TO RUN: Rscript script.R local
# TO RUN: Rscript script.R remote

args <- commandArgs(trailingOnly=TRUE)

where     <- args[1] # `remote` or `local` # where <- "local"
freqThr   <- as.integer(args[2])

#registerDoParallel(coresFiles)
#print(paste0("Number of activated cores: ", coresFiles))

source("00_functions.R")

print("################################################################################")
print("> 4. prepapring ngram lists with IDs ... #######################################")
print("################################################################################")
print("################################################################################")

# setwd("/Users/romanov/_ADHFAIS_Data_TEMP/app_NgramReaderNew_v1/")

unigramFile <- paste0(tempFolder, "aggregated__1gram__", freqThr, "up.tsv")
bigramFile <- str_replace(unigramFile, "__1gram__", "__2gram__")
trigramFile <- str_replace(unigramFile, "__1gram__", "__3gram__")

print("> 4a. unigrams #################################################################")

gramsFile <- unigramFile
gramsFileToSave <- paste0("data_1grams", freqThr, "up_HI.rds")

grams <- read_delim(gramsFile, col_types = cols(), delim = "\t")

#print(grams)

gramsToSave <- grams %>%
  dplyr::arrange(ngram) %>% 
  mutate(ID = row_number()) %>%
  mutate(ngramTR = str_replace_all(ngram, arabicToTranslit)) %>%
  dplyr::arrange(desc(count)) %>%
  select(ID, ngramTR)

#print(gramsToSave)

fileToSave <- paste0(finalDataFolder, gramsFileToSave)
saveRDS(gramsToSave, fileToSave)

print("> 4b. bigrams ##################################################################")

gramsFile <- bigramFile
gramsFileToSave <- paste0("data_2grams", freqThr, "up_HI.rds") #gramsFileToSave <- "data_2gramsHI.rds"
gramsFileLOW_ToSave <- paste0("data_2grams", freqThr, "up_LO_%05d.rds") #gramsFileLOW_ToSave <- "data_2gramsLO_%05d.rds" # sprintf(gramsFileLOW_ToSave, 55)
highThresh <- 10

grams <- read_delim(gramsFile, col_types = cols(), delim = "\t")

gramsToSave <- grams %>%
  arrange(desc(ngram)) %>% mutate(ID = row_number()) %>%
  select(ngram, ID, count) %>%
  mutate(ngramTR = str_replace_all(ngram, arabicToTranslit)) %>%
  arrange(desc(count)) %>%
  select(ID, ngramTR, count)

# PROCESS HIGH
gramsToSaveHI <- gramsToSave %>%
  filter(count >= highThresh) %>% select(-count)
fileToSave <- paste0(finalDataFolder, gramsFileToSave)
saveRDS(gramsToSaveHI, fileToSave)

# PROCESS LOW 
gramsToSaveLO <- gramsToSave %>% filter(count < highThresh) %>%
  mutate(divider = row_number()) %>%
  mutate(groups = plyr::round_any(divider, 50000, f = ceiling) / 1000) %>%
  select(-divider)

groups <- gramsToSaveLO$groups %>% unique()

for (n in groups){
  temp <- gramsToSaveLO %>%
    filter(groups == n) %>%
    select(-groups, -count)
  
  fileToSave <- paste0(finalDataFolder, sprintf(gramsFileLOW_ToSave, n))
  saveRDS(temp, fileToSave)
  
}

print("> 4c. trigrams #################################################################")

gramsFile <- trigramFile
gramsFileToSave <- paste0("data_3grams", freqThr, "up_HI.rds") #gramsFileToSave <- "data_3gramsHI.rds"
gramsFileLOW_ToSave <- paste0("data_3grams", freqThr, "up_LO_%05d.rds") #gramsFileLOW_ToSave <- "data_3gramsLO_%05d.rds" # sprintf(gramsFileLOW_ToSave, 55)

highThresh <- 10

grams <- read_delim(gramsFile, col_types = cols(), delim = "\t")

gramsToSave <- grams %>%
  arrange(desc(ngram)) %>% mutate(ID = row_number()) %>%
  select(ngram, ID, count) %>%
  mutate(ngramTR = str_replace_all(ngram, arabicToTranslit)) %>%
  arrange(desc(count)) %>%
  select(ID, ngramTR, count)

# PROCESS HIGH
gramsToSaveHI <- gramsToSave %>%
  filter(count >= highThresh) %>% select(-count)
fileToSave <- paste0(finalDataFolder, gramsFileToSave)
saveRDS(gramsToSaveHI, fileToSave)

# PROCESS LOW 
gramsToSaveLO <- gramsToSave %>% filter(count < highThresh) %>%
  mutate(divider = row_number()) %>%
  mutate(groups = plyr::round_any(divider, 50000, f = ceiling) / 1000) %>%
  select(-divider)

groups <- gramsToSaveLO$groups %>% unique()

for (n in groups){
  temp <- gramsToSaveLO %>%
    filter(groups == n) %>%
    select(-groups, -count)
  
  fileToSave <- paste0(finalDataFolder, sprintf(gramsFileLOW_ToSave, n))
  saveRDS(temp, fileToSave)
  
}

print("################################################################################")
print("> THE FILES HAVE BEEN PROCESSED ################################################")
print("################################################################################")

