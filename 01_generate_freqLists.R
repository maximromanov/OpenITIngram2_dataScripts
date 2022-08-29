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

print("################################################################################")
print("> PREPARING METADATA ###########################################################")
print("################################################################################")

metadata <- read_delim(metadataFile, delim = "\t", escape_double = FALSE, trim_ws = TRUE)

mdFiltered <- metadata %>%
  filter(status2 == "pri") %>%
  rename(bookURI = book) %>%
  select(bookURI, tok_length, local_path) %>%
  mutate(path = paste0(corpusFolder, str_replace(local_path, "^\\.\\./", ""))) %>%
  select(-local_path) %>%
  arrange(bookURI) %>%
  mutate(bookID = row_number()) %>%
  select(bookID, bookURI, tok_length, path)

# SAVE bookID, bookURI data (rds)
bookIDfile <- mdFiltered %>%
  select(bookID, bookURI)
saveRDS(bookIDfile, paste0(finalDataFolder, "metadata_bookID_bookURI.rds"))

print("################################################################################")
print("> MAIN PROCESSING ##############################################################")
print("################################################################################")

print("################################################################################")
print("> 1. GENERATING FREQ LISTS: 1gram, 2gram, 3gram... #############################")
print("################################################################################")


foreach (row = 1:nrow(mdFiltered)) %dopar% {
  bookURI <- as.character(mdFiltered[row, "bookURI"])
  filePath <- paste0(freqListFolder, bookURI, ".tsv")
  
  if (!file.exists(filePath)){
    print(paste0(">>> Generating Ngram FreqLists: ", bookURI))
    temp <- read_file(as.character(mdFiltered[row, "path"]))
    temp <- str_split(temp, "#META#Header#End#")[[1]][2]
    # CLEAN TEXT AND NORMALIZE
    temp <- str_replace_all(temp, arabicNormalize)
    temp <- str_replace_all(temp, bareText)
    temp <- as_tibble(temp)
    
    unigrams <- temp %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 1) %>%
      group_by(ngram) %>% summarise(count = n()) %>%  mutate(n = 1)
    
    bigrams <- temp %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 2) %>%
      group_by(ngram) %>% summarise(count = n()) %>%  mutate(n = 2)
    
    trigrams <- temp %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 3) %>%
      group_by(ngram) %>% summarise(count = n()) %>%  mutate(n = 3)
    
    final <- unigrams %>%
      add_row(bigrams) %>% add_row(trigrams) %>%
      arrange(desc(count))
    
    # save
    write_delim(final, filePath, delim="\t")
  } else {
    print(paste0(">>> The following file has been processed already: ", bookURI))
  }
}
