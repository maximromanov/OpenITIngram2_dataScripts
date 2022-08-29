################################################################################
### GLOBAL VARIABLES & FUNCTIONS ###############################################
################################################################################

if (where == "remote"){
  corpusFolder <- "../../RELEASE_20220106/"
} else {
  print("assuming that you are running `local`; use `remote` if running on the server...")
  corpusFolder   <- "/Volumes/LaCie4Tb/OpenITI_TEMP/_RELEASE_ADHFAIS/"
}

freqListFolder   <- "./freqLists/" # `/_ADHFAIS_Data_TEMP/`
tempFolder       <- "./tempDataFolder/" # `/_ADHFAIS_Data_TEMP/`
finalDataFolder  <- "./ngramReaderDATA/" # `/_ADHFAIS_Data_TEMP/`
metadataFile <- "paragon_metadata.csv"

periodSelected <- 50

################################################################################
### NORMALIZATION VARIABLES ####################################################
################################################################################

# add short vowels to remove; add normalization steps
harakat <- "[ًٌٍَُِّْ]"

arabicNormalize <- c("[ًٌٍَُِّْ]" = "", "ى" = "ي", "[آأإاٱ]" = "ا", "يء" = "ء", "ىء" = "ء", "[ئؤ]" = "ء")

# add elements to be removed from OpenITI texts to get to the bare ARABIC TEXT
bareText <- c("[a-zA-Z]" = "", "\\W+" = " ", "\\d+" = " ",
              "_+" = " ", " +" = " ", "‍" = "")

arabicToTranslit <- c("‍" = "", " " = "_", "ء" = "c", "ا" = "A", "ب" = "b",
                      "ة" = "o", "ت" = "t", "ث" = "v", "ج" = "j", "ح" = "H", "خ" = "x",
                      "د" = "d", "ذ" = "V", "ر" = "r", "ز" = "z", "س" = "s", "ش" = "E",
                      "ص" = "S", "ض" = "D", "ط" = "T", "ظ" = "Z", "ع" = "C", "غ" = "g",
                      "ف" = "f", "ق" = "q", "ك" = "k", "ل" = "l", "م" = "m", "ن" = "n",
                      "ه" = "h", "و" = "w", "ي" = "y")


translitToLetter <- c("_" = "_space_", "c" = "_hamza_", "A" = "_alif_",
                      "b" = "_ba_", "o" = "_tm_", "t" = "_ta_", "v" = "_tha_", "j" = "_jim_",
                      "H" = "_hha_", "x" = "_kha_", "d" = "_dal_", "V" = "_dhal_", "r" = "_ra_",
                      "z" = "_zay_", "s" = "_sin_", "E" = "_shin_", "S" = "_ssad_", "S" = "_ddad_",
                      "T" = "_tta_", "Z" = "_zza_", "C" = "_cayn_", "g" = "_ghayn_", "f" = "_fa_",
                      "q" = "_qaf_", "k" = "_kaf_", "l" = "_lam_", "m" = "_mim_", "n" = "_nun_",
                      "h" = "_ha_", "w" = "_waw_", "y" = "_ya_")

arabicToLetter   <- c(" " = "_space_", "ء" = "_hamza_", "ا" = "_alif_",
                      "ب" = "_ba_", "ة" = "_tm_", "ت" = "_ta_", "ث" = "_tha_", "ج" = "_jim_",
                      "ح" = "_hha_", "خ" = "_kha_", "د" = "_dal_", "ذ" = "_dhal_", "ر" = "_ra_",
                      "ز" = "_zay_", "س" = "_sin_", "ش" = "_shin_", "ص" = "_ssad_", "ض" = "_ddad_",
                      "ط" = "_tta_", "ظ" = "_zza_", "ع" = "_cayn_", "غ" = "_ghayn_", "ف" = "_fa_",
                      "ق" = "_qaf_", "ك" = "_kaf_", "ل" = "_lam_", "م" = "_mim_", "ن" = "_nun_",
                      "ه" = "_ha_", "و" = "_waw_", "ي" = "_ya_")

################################################################################
### functions ##################################################################
################################################################################

periodValueMain <- 25


# PREPARE LARGE FILES

idToFname <- function(prefix, ngramType, ngramID){
  fileSuffix <- plyr::round_any(ngramID, 1000, f = ceiling) / 1000
  fileName <- sprintf(paste0(prefix, ngramType, "gram_%06d.rds"), fileSuffix)
  return(fileName)
}

idToFnameChrono <- function(ngramType, ngramID){
  fileSuffix <- plyr::round_any(ngramID, 1000, f = ceiling) / 1000
  fileName <- sprintf(paste0("chrono_", ngramType, "gram_%09d.rds"), fileSuffix)
  return(fileName)
}

idToFnameBooks <- function(ngramType, ngramID){
  fileSuffix <- plyr::round_any(ngramID, 100, f = ceiling) / 100
  fileName <- sprintf(paste0("books_", ngramType, "gram_%09d.rds"), fileSuffix)
  return(fileName)
}