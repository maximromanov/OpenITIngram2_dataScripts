#!/bin/sh

# THE SHELL SCRIPT TO RUN EVERYTHING TOGETHER

# GENERATING FREQUENCY LISTS FROM ALL TEXTS
Rscript 01_prepare_data_freqLists.R remote 30
# - runs with remote configuration on 30 cores,
#   creating frequency lists from all the texts
#   in the current corpus (path to the corpus
#   is in the script)

# GENERATING FREQUENCY LISTS FROM ALL TEXTS
Rscript 01a_merge_freqLists.R remote 30
# - merges generated frequency lists into larger
#   temporary files for processing in the final step

# CALCULATING FREQUENCIES BY PERIODS (dropping singles)
Rscript 02_aggregate_ngrams_into_periods.R remote 30 1
Rscript 02_aggregate_ngrams_into_periods.R remote 30 2
Rscript 02_aggregate_ngrams_into_periods.R remote 30 3
# - runs on 30 cores for each type of ngram

# AGGREGATING NGRAMS INTO A SINGLE FILE
Rscript 03_aggregate_period_files.R remote 1 5
Rscript 03_aggregate_period_files.R remote 1 5
Rscript 03_aggregate_period_files.R remote 1 5
# - collects only useful ngrams (1, 2, or 3) with 
#   the minimal frequency of 5 per period; this is 
#   necessary, otherwise the ammount of data is too large
#   5 seems quite an optimal number on the curve

# PREPARING FINAL NGRAM LISTS
Rscript 04_prepare_final_ngram_lists.R remote
# - this generates the final lists of ngrams of each type
#   one script/one run collects all data:
#   - unigrams with overall frequencies 5 and up;
#   - bigrams and trigrams with overall frequencies 10 and up;

# AGGREATING CHRONOLOGICAL DATA (LARGE)
Rscript 05_preparing_chronological_data.R remote 30 1
Rscript 05_preparing_chronological_data.R remote 30 2
Rscript 05_preparing_chronological_data.R remote 30 3
# - this script prepares chronological data, where 
#   ngrams are aggregated into small files by their 
#   numeric IDs

# AGGREGATING NGRAMS-BOOKS DATA (LARGE)
Rscript 06_aggregating_ngram_to_book_data.R remote 30 1
Rscript 06_aggregating_ngram_to_book_data.R remote 30 2
Rscript 06_aggregating_ngram_to_book_data.R remote 30 3
# - this script prepares book data, where 
#   ngrams are aggregated into small files by their 
#   numeric IDs
