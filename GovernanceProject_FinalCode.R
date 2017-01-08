##############################################################

# R Code for Text Analysis of AntASOCtic Governance Documents
# Created by Danya Lagos, 06/23/16 (danya@uchicago.edu)
# Last Updated: 00:04 08/24/16

##############################################################

# SECTION 0 - LOADING LIBRARIES AND SETTING INITIAL FILEPATHS

# Section 0.1 - Loading Libraries Necessary for the Analysis"
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc) 
library(Rmpfr)
library(Rgraphviz)
library(data.table)
library(bigmemory)

# Section 0.2 - Setting Directories and File Paths 
# You can edit the filepath below to match your machine. 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AllFiles_") 
dir(cname)

# SECTION 1 - DATA PREPROCESSING - ALL FILES

# Step 1.1 -Roll Files into a Corpus
Corpus_AllFiles <- Corpus(DirSource(cname))

# Step 1.2 -  Create Document Term Matrix for All Files 
# The resulting DTMs is too big for R to process and turn into a CSV. 
# Once this is converted to a DTM, need to remove some sparse terms for it to run.
dtm_AllFiles <- DocumentTermMatrix(Corpus_AllFiles,
                                   control = list(stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhiteSpace = TRUE))

# Step 1.3 - Change Folders for Saving
setwd(parent.folder)
dtmsfolder <- file.path("CSVs/DocumentTermMatrices")
dir(dtmsfolder)


# Step 1.4a.1 - 80% Sparsity - 1060 terms, 5136 docs. 
dtms_AllFiles_080 <- removeSparseTerms(dtm_AllFiles, 0.900)
# Step 1.4a.2 - Counts the terms, docs, and sparsity 
dtms_AllFiles_080
# Step 1.4d.3 Get into format that can be saved in a .csv
dtms_AllFiles_080_matrix <- as.matrix(dtms_AllFiles_080)
dtms_AllFiles_080_frame <- data.frame(dtms_AllFiles_080_matrix)
write.csv(dtms_AllFiles_080_frame, "dtm_AllFiles_080.csv", quote = FALSE)

# Step 1.5 Reset directory
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AllFiles_") 
dir(cname)

# Step 1.6 Create Term Document Matrices for All Files
tdm_AllFiles <- TermDocumentMatrix(Corpus_AllFiles,
                                   control = list(stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhiteSpace = TRUE))
# Step 1.7 
setwd(parent.folder)
dtmsfolder <- file.path("CSVs/FrequencyTables")
dir(dtmsfolder)

# Step 1.8 - Removing Sparse Terms and Saving to CSVs 
# Step 1.8.1 - 80% Sparsity - 1060 terms, 5136 docs. 
tdms_AllFiles_080 <- removeSparseTerms(tdm_AllFiles, 0.900)
# Step 1.8.2 - Counts the terms, docs, and sparsity 
tdms_AllFiles_080
# Step 1.8d.3 Get into format that can be saved in a .csv
tdms_AllFiles_080_matrix <- as.matrix(tdms_AllFiles_080)
tdms_AllFiles_080_frame <- data.frame(tdms_AllFiles_080_matrix)
write.csv(tdms_AllFiles_080_frame, "tdm_AllFiles_080.csv", quote = FALSE)

# SECTION 2 - DATA PREPROCESSING - ATS FILES
# Section 2.0 - Resetting Directories and File Paths 
# You can edit the filepath below to match your machine. 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ATS") 
dir(cname)
# Step 2.1 -Roll Files into a Corpus
Corpus_ATS <- Corpus(DirSource(cname))
# Step 2.2 - Create Document Term Matrix for ATS 
dtm_ATS <- DocumentTermMatrix(Corpus_ATS,
                                   control = list(stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhiteSpace = TRUE))

# Step 2.3 - Change Folders for Saving
setwd(parent.folder)
dtmsfolder <- file.path("CSVs/DocumentTermMatrices")
dir(dtmsfolder)

# Step 2.4 - Removing Sparse Terms and Saving to CSVs 
# Step 2.4d.1 - 80% Sparsity - 1052 terms, 4423 docs. 
dtms_ATS_080 <- removeSparseTerms(dtm_ATS, 0.900)
# Step 2.4d.2 - Counts the terms, docs, and sparsity 
dtms_ATS_080
# Step 2.4d.3 Get into format that can be saved in a .csv
dtms_ATS_080_matrix <- as.matrix(dtms_ATS_080)
dtms_ATS_080_frame <- data.frame(dtms_ATS_080_matrix)
write.csv(dtms_ATS_080_frame, "dtm_ATS_080.csv", quote = FALSE)

# Step 2.5 Reset directory
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ATS") 
dir(cname)
Corpus_ATS <- Corpus(DirSource(cname))
# Step 2.6 Create Term Document Matrices for All Files
tdm_ATS <- TermDocumentMatrix(Corpus_ATS,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))
# Step 2.7 
setwd(parent.folder)
tdmsfolder <- file.path("CSVs/FrequencyTables")
dir(tdmsfolder)

# Step 2.8d.1 - 80% Sparsity - 1060 terms, 5136 docs. 
tdms_ATS_080 <- removeSparseTerms(tdm_ATS, 0.900)
# Step 2.8d.2 - Counts the terms, docs, and sparsity 
tdms_ATS_080
# Step 2.8d.3 Get into format that can be saved in a .csv
tdms_ATS_080_matrix <- as.matrix(tdms_ATS_080)
tdms_ATS_080_frame <- data.frame(tdms_ATS_080_matrix)
write.csv(tdms_ATS_080_frame, "tdm_ATS_080.csv", quote = FALSE)

# SECTION 3 - DATA PREPROCESSING - CCAS FILES
# Section 3.0 - Resetting Directories and File Paths 
# You can edit the filepath below to match your machine. 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("CCAS") 
dir(cname)
# Step 3.1 -Roll Files into a Corpus
Corpus_CCAS <- Corpus(DirSource(cname))
# Step 3.2 - Create Document Term Matrix for ATS 
dtm_CCAS <- DocumentTermMatrix(Corpus_CCAS,
                               control = list(stemming = TRUE,
                                              stopwords = TRUE,
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhiteSpace = TRUE))

# Could not be reduced to 80% Sparsity
# 58% is maximum 
# Step 3.3 Reset directory 
setwd(parent.folder)
dtmsfolder <- file.path("CSVs/DocumentTermMatrices")
dir(dtmsfolder)
# Step 3.4 - Convert to csv and save. 
dtms_CCAS_matrix <- as.matrix(dtm_CCAS)
dtms_CCAS_frame <- data.frame(dtms_CCAS_matrix)
write.csv(dtms_CCAS_frame, "dtm_CCAS_058.csv", quote = FALSE)

# Step 3.5 Reset directory
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("CCAS") 
dir(cname)

# Step 3.6 Create Term Document Matrices for All Files
tdm_CCAS <- TermDocumentMatrix(Corpus_CCAS,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))
# Step 3.7 
setwd(parent.folder)
tdmsfolder <- file.path("CSVs/FrequencyTables")
dir(tdmsfolder)
# Step 3.4 - Convert to csv and save. 
tdms_CCAS_matrix <- as.matrix(tdm_CCAS)
tdms_CCAS_frame <- data.frame(tdms_CCAS_matrix)
write.csv(tdms_CCAS_frame, "tdm_CCAS_058.csv", quote = FALSE)

# SECTION 4 - DATA PREPROCESSING - ALL NON ATS
# Section 4.0 - Resetting Directories and File Paths 
# You can edit the filepath below to match your machine. 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AllNonATS") 
dir(cname)
# Step 4.1 -Roll Files into a Corpus
Corpus_AllNonATS <- Corpus(DirSource(cname))
# Step 4.2 - Create Document Term Matrix for ATS 
dtm_AllNonATS <- DocumentTermMatrix(Corpus_AllNonATS,
                               control = list(stemming = TRUE,
                                              stopwords = TRUE,
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhiteSpace = TRUE))


# Step 4.3a.1 - 98% Sparsity - 11367 terms, 708 docs. 
dtms_AllNonATS_080 <- removeSparseTerms(dtm_AllNonATS, 0.89)
dtms_AllNonATS_080
# Step 4.3a.2 - Counts the terms, docs, and sparsity 
dtms_AllNonATS_080_matrix <- as.matrix(dtms_AllNonATS_080)
dtms_AllNonATS_080_frame <- data.frame(dtms_AllNonATS_080_matrix)
write.csv(dtms_AllNonATS_080_frame, "dtm_AllNonATS_080.csv", quote = FALSE)

# Step 4.4 Reset directory
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AllNonATS") 
dir(cname)

# Step 4.5 Create Term Document Matrices for All Files
tdm_AllNonATS <- TermDocumentMatrix(Corpus_AllNonATS,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))

# Step 4.6d.1 - 80% Sparsity - 1137 terms, 708 docs. 
tdms_AllNonATS_080 <- removeSparseTerms(tdm_AllNonATS, 0.89)
# Step 2.6d.2 - Counts the terms, docs, and sparsity 
tdms_AllNonATS_080
# Step 2.6d.3 Get into format that can be saved in a .csv
tdms_AllNonATS_080_matrix <- as.matrix(tdms_AllNonATS_080)
tdms_AllNonATS_080_frame <- data.frame(tdms_AllNonATS_080_matrix)
write.csv(tdms_AllNonATS_080_frame, "tdm_AllNonATS_080.csv", quote = FALSE)

# STEP 5 - DATA PREPROCESSING - AARI Files 
# Step 5.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AARI") 
dir(cname)
# Step 5.1 - Roll into corpus. 
Corpus_AARI <- Corpus(DirSource(cname))
# Step 5.2 - Convert to document term matrix 
dtm_AARI<- DocumentTermMatrix(Corpus_AARI,
                                    control = list(stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhiteSpace = TRUE))
# Sparsity - 0%, Terms 460, 1 Document
dtm_AARI
# Step 5.3 Get into format that can be saved in a .csv
dtms_AARI_000_matrix <- as.matrix(dtm_AARI)
dtms_AARI_000_frame <- data.frame(dtms_AARI_000_matrix)
write.csv(dtms_AARI_000_frame, "dtm_AARI_000.csv", quote = FALSE)

# Step 5.4 Convert to term document Matrix
tdm_AARI <- TermDocumentMatrix(Corpus_AARI,
                                    control = list(stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhiteSpace = TRUE))
tdms_AARI_000_matrix <- as.matrix(tdm_AARI)
tdms_AARI_000_frame <- data.frame(tdms_AARI_000_matrix)
write.csv(tdms_AARI_000_frame, "tdm_AARI_000.csv", quote = FALSE)

# STEP 6 - DATA PREPROCESSING - ACECRC Files 
# Step 6.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ACECRC") 
dir(cname)

# Step 6.1 - Roll into corpus. 
Corpus_ACECRC <- Corpus(DirSource(cname))
# Step 6.2 - Convert to document term matrix 
dtm_ACECRC<- DocumentTermMatrix(Corpus_ACECRC,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))

# Step 6.3 - Remove Sparse Terms
# Step 6.3a.1 - 80% Sparsity - 1475 terms, 26 docs. 
dtms_ACECRC_080 <- removeSparseTerms(dtm_ACECRC, 0.85)
dtms_ACECRC_080
# Step 6.4 Convert to csv
dtms_ACECRC_080_matrix <- as.matrix(dtms_ACECRC_080)
dtms_ACECRC_080_frame <- data.frame(dtms_ACECRC_080_matrix)
write.csv(dtms_ACECRC_080_frame, "dtm_ACECRC_080.csv", quote = FALSE)

# Step 6.5 Convert to term document Matrix
tdm_ACECRC <- TermDocumentMatrix(Corpus_ACECRC,
                               control = list(stemming = TRUE,
                                              stopwords = TRUE,
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhiteSpace = TRUE))
# Step 6.6 Remove Sparse Terms 
# Step 6.6a.1 - 80% Sparsity - 1475 terms, 26 docs. 
tdms_ACECRC_080 <- removeSparseTerms(tdm_ACECRC, 0.85)
tdms_ACECRC_080
# Step 6.7 - Convert to CSV 
tdms_ACECRC_080_matrix <- as.matrix(tdms_ACECRC_080)
tdms_ACECRC_080_frame <- data.frame(tdms_ACECRC_080_matrix)
write.csv(tdms_ACECRC_080_frame, "tdm_ACECRC_080.csv", quote = FALSE)

# STEP 7 - DATA PREPROCESSING - ARC Files 
# Step 7.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ARC") 
dir(cname)

# Step 7.1 - Roll into corpus. 
Corpus_ARC <- Corpus(DirSource(cname))
# Step 7.2 - Convert to document term matrix 
dtm_ARC<- DocumentTermMatrix(Corpus_ARC,
                                control = list(stemming = TRUE,
                                               stopwords = TRUE,
                                               removeNumbers = TRUE,
                                               removePunctuation = TRUE,
                                               stripWhiteSpace = TRUE))

# Step 7.3 - Remove Sparse Terms
# Step 7.3a.1 - 80% Sparsity - 2635 terms, 32 docs. 
dtms_ARC_080 <- removeSparseTerms(dtm_ARC, 0.91)
dtms_ARC_080
# Step 7.4 Convert to csv
dtms_ARC_080_matrix <- as.matrix(dtms_ARC_080)
dtms_ARC_080_frame <- data.frame(dtms_ARC_080_matrix)
write.csv(dtms_ARC_080_frame, "dtm_ARC_080.csv", quote = FALSE)

# Step 7.5 Convert to term document Matrix
tdm_ARC <- TermDocumentMatrix(Corpus_ARC,
                                 control = list(stemming = TRUE,
                                                stopwords = TRUE,
                                                removeNumbers = TRUE,
                                                removePunctuation = TRUE,
                                                stripWhiteSpace = TRUE))
# Step 7.6 Remove Sparse Terms 
# Step 7.6a.1 - 80% Sparsity - 26355 terms, 32 docs. 
tdms_ARC_080 <- removeSparseTerms(tdm_ARC, 0.91)
tdms_ARC_080
# Step 7.7 - Convert to CSV 
tdms_ARC_080_matrix <- as.matrix(tdms_ARC_080)
tdms_ARC_080_frame <- data.frame(tdms_ARC_080_matrix)
write.csv(tdms_ARC_080_frame, "tdm_ARC_080.csv", quote = FALSE)


# STEP 8 - DATA PREPROCESSING - ASOC Files 
# Step 8.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ASOC") 
dir(cname)

# Step 8.1 - Roll into corpus. 
Corpus_ASOC <- Corpus(DirSource(cname))
# Step 8.2 - Convert to document term matrix 
dtm_ASOC<- DocumentTermMatrix(Corpus_ASOC,
                             control = list(stemming = TRUE,
                                            stopwords = TRUE,
                                            removeNumbers = TRUE,
                                            removePunctuation = TRUE,
                                            stripWhiteSpace = TRUE))

# Step 8.3 - Remove Sparse Terms
# Step 8.3a.1 - 80% Sparsity - 1750 terms, 301 docs. 
dtms_ASOC_080 <- removeSparseTerms(dtm_ASOC, 0.92)
dtms_ASOC_080
# Step 8.4 Convert to csv
dtms_ASOC_080_matrix <- as.matrix(dtms_ASOC_080)
dtms_ASOC_080_frame <- data.frame(dtms_ASOC_080_matrix)
write.csv(dtms_ASOC_080_frame, "dtm_ASOC_080.csv", quote = FALSE)

# Step 8.5 Convert to term document Matrix
tdm_ASOC <- TermDocumentMatrix(Corpus_ASOC,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))
# Step 8.6 Remove Sparse Terms 
# Step 8.6a.1 - 80% Sparsity - 1750 terms, 301 docs. 
tdms_ASOC_080 <- removeSparseTerms(tdm_ASOC, 0.92)
tdms_ASOC_080
# Step 8.7 - Convert to CSV 
tdms_ASOC_080_matrix <- as.matrix(tdms_ASOC_080)
tdms_ASOC_080_frame <- data.frame(tdms_ASOC_080_matrix)
write.csv(tdms_ASOC_080_frame, "tdm_ASOC_080.csv", quote = FALSE)

# STEP 9 - DATA PREPROCESSING - CCAMLR Files 
# Step 9.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("CCAMLR") 
dir(cname)

# Step 9.1 - Roll into corpus. 
Corpus_CCAMLR <- Corpus(DirSource(cname))
# Step 9.2 - Convert to document term matrix 
dtm_CCAMLR<- DocumentTermMatrix(Corpus_CCAMLR,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))

# Step 9.3 - Remove Sparse Terms
# Step 9.3a.1 - 80% Sparsity - 5611 terms, 34 docs. 
dtms_CCAMLR_080 <- removeSparseTerms(dtm_CCAMLR, 0.92)
dtms_CCAMLR_080
# Step 9.4 Convert to csv
dtms_CCAMLR_080_matrix <- as.matrix(dtms_CCAMLR_080)
dtms_CCAMLR_080_frame <- data.frame(dtms_CCAMLR_080_matrix)
write.csv(dtms_CCAMLR_080_frame, "dtm_CCAMLR_080.csv", quote = FALSE)

# Step 9.5 Convert to term document Matrix
tdm_CCAMLR <- TermDocumentMatrix(Corpus_CCAMLR,
                               control = list(stemming = TRUE,
                                              stopwords = TRUE,
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhiteSpace = TRUE))
# Step 9.6 Remove Sparse Terms 
# Step 9.6a.1 - 80% Sparsity - 5611 terms, 34 docs. 
tdms_CCAMLR_080 <- removeSparseTerms(tdm_CCAMLR, 0.92)
tdms_CCAMLR_080
# Step 9.7 - Convert to CSV 
tdms_CCAMLR_080_matrix <- as.matrix(tdms_CCAMLR_080)
tdms_CCAMLR_080_frame <- data.frame(tdms_CCAMLR_080_matrix)
write.csv(tdms_CCAMLR_080_frame, "tdm_CCAMLR_080.csv", quote = FALSE)

# STEP 10 - DATA PREPROCESSING - COMNAP Files 
# Step 10.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("COMNAP") 
dir(cname)

# Step 10.1 - Roll into corpus. 
Corpus_COMNAP <- Corpus(DirSource(cname))
# Step 10.2 - Convert to document term matrix 
dtm_COMNAP<- DocumentTermMatrix(Corpus_COMNAP,
                                control = list(stemming = TRUE,
                                               stopwords = TRUE,
                                               removeNumbers = TRUE,
                                               removePunctuation = TRUE,
                                               stripWhiteSpace = TRUE))

# Step 10.3 - Remove Sparse Terms
# Step 10.3a.1 - 80% Sparsity - 228 terms, 37 docs. 
dtms_COMNAP_080 <- removeSparseTerms(dtm_COMNAP, 0.85)
dtms_COMNAP_080
# Step 10.4 Convert to csv
dtms_COMNAP_080_matrix <- as.matrix(dtms_COMNAP_080)
dtms_COMNAP_080_frame <- data.frame(dtms_COMNAP_080_matrix)
write.csv(dtms_COMNAP_080_frame, "dtm_COMNAP_080.csv", quote = FALSE)

# Step 10.5 Convert to term document Matrix
tdm_COMNAP <- TermDocumentMatrix(Corpus_COMNAP,
                                 control = list(stemming = TRUE,
                                                stopwords = TRUE,
                                                removeNumbers = TRUE,
                                                removePunctuation = TRUE,
                                                stripWhiteSpace = TRUE))
# Step 10.6 Remove Sparse Terms 
# Step 10.6a.1 - 80% Sparsity - 228 terms, 37 docs. 
tdms_COMNAP_080 <- removeSparseTerms(tdm_COMNAP, 0.85)
tdms_COMNAP_080
# Step 10.7 - Convert to CSV 
tdms_COMNAP_080_matrix <- as.matrix(tdms_COMNAP_080)
tdms_COMNAP_080_frame <- data.frame(tdms_COMNAP_080_matrix)
write.csv(tdms_COMNAP_080_frame, "tdm_COMNAP_080.csv", quote = FALSE)

# STEP 11 - DATA PREPROCESSING - IAATO Files 
# Step 11.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("IAATO") 
dir(cname)

# Step 11.1 - Roll into corpus. 
Corpus_IAATO <- Corpus(DirSource(cname))
# Step 11.2 - Convert to document term matrix 
dtm_IAATO<- DocumentTermMatrix(Corpus_IAATO,
                                control = list(stemming = TRUE,
                                               stopwords = TRUE,
                                               removeNumbers = TRUE,
                                               removePunctuation = TRUE,
                                               stripWhiteSpace = TRUE))

# Step 11.3 - Remove Sparse Terms
# Step 11.3a.1 - 80% Sparsity - 2740 terms, 89 docs. 
dtms_IAATO_080 <- removeSparseTerms(dtm_IAATO, 0.94)
dtms_IAATO_080
# Step 11.4 Convert to csv
dtms_IAATO_080_matrix <- as.matrix(dtms_IAATO_080)
dtms_IAATO_080_frame <- data.frame(dtms_IAATO_080_matrix)
write.csv(dtms_IAATO_080_frame, "dtm_IAATO_080.csv", quote = FALSE)

# Step 11.5 Convert to term document Matrix
tdm_IAATO <- TermDocumentMatrix(Corpus_IAATO,
                                 control = list(stemming = TRUE,
                                                stopwords = TRUE,
                                                removeNumbers = TRUE,
                                                removePunctuation = TRUE,
                                                stripWhiteSpace = TRUE))
# Step 11.6 Remove Sparse Terms 
# Step 11.6a.1 - 80% Sparsity - 2740 terms, 89 docs. 
tdms_IAATO_080 <- removeSparseTerms(tdm_IAATO, 0.94)
tdms_IAATO_080
# Step 11.7 - Convert to CSV 
tdms_IAATO_080_matrix <- as.matrix(tdms_IAATO_080)
tdms_IAATO_080_frame <- data.frame(tdms_IAATO_080_matrix)
write.csv(tdms_IAATO_080_frame, "tdm_IAATO_080.csv", quote = FALSE)

# STEP 12 - DATA PREPROCESSING - IMAS FILES
# Step 12.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("IMAS") 
dir(cname)

# Step 12.1 - Roll into corpus. 
Corpus_IMAS <- Corpus(DirSource(cname))
# Step 12.2 - Convert to document term matrix 
dtm_IMAS<- DocumentTermMatrix(Corpus_IMAS,
                               control = list(stemming = TRUE,
                                              stopwords = TRUE,
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhiteSpace = TRUE))

# Step 12.3 - Remove Sparse Terms
# Step 12.3a.1 - 80% Sparsity - 345 terms, 122 docs. 
dtms_IMAS_080 <- removeSparseTerms(dtm_IMAS, 0.89)
dtms_IMAS_080
# Step 12.4 Convert to csv
dtms_IMAS_080_matrix <- as.matrix(dtms_IMAS_080)
dtms_IMAS_080_frame <- data.frame(dtms_IMAS_080_matrix)
write.csv(dtms_IMAS_080_frame, "dtm_IMAS_080.csv", quote = FALSE)

# Step 12.5 Convert to term document Matrix
tdm_IMAS <- TermDocumentMatrix(Corpus_IMAS,
                                control = list(stemming = TRUE,
                                               stopwords = TRUE,
                                               removeNumbers = TRUE,
                                               removePunctuation = TRUE,
                                               stripWhiteSpace = TRUE))
# Step 12.6 Remove Sparse Terms 
# Step 12.6a.1 - 80% Sparsity - 345 terms, 122 docs. 
tdms_IMAS_080 <- removeSparseTerms(tdm_IMAS, 0.89)
tdms_IMAS_080
# Step 12.7 - Convert to CSV 
tdms_IMAS_080_matrix <- as.matrix(tdms_IMAS_080)
tdms_IMAS_080_frame <- data.frame(tdms_IMAS_080_matrix)
write.csv(tdms_IMAS_080_frame, "tdm_IMAS_080.csv", quote = FALSE)

# STEP 10 - DATA PREPROCESSING - SCAR FILES
# Step 10.0 - Reset Directories and Filepaths
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("SCAR") 
dir(cname)

# Step 10.1 - Roll into corpus. 
Corpus_SCAR <- Corpus(DirSource(cname))
# Step 10.2 - Convert to document term matrix 
dtm_SCAR <- DocumentTermMatrix(Corpus_SCAR,
                              control = list(stemming = TRUE,
                                             stopwords = TRUE,
                                             removeNumbers = TRUE,
                                             removePunctuation = TRUE,
                                             stripWhiteSpace = TRUE))

# Step 10.3 - Remove Sparse Terms
# Step 10.3a.1 - 80% Sparsity - 1002 terms, 52 docs. 
dtms_SCAR_080 <- removeSparseTerms(dtm_SCAR, 0.90)
dtms_SCAR_080
# Step 10.4 Convert to csv
dtms_SCAR_080_matrix <- as.matrix(dtms_SCAR_080)
dtms_SCAR_080_frame <- data.frame(dtms_SCAR_080_matrix)
write.csv(dtms_SCAR_080_frame, "dtm_SCAR_080.csv", quote = FALSE)

# Step 10.5 Convert to term document Matrix
tdm_SCAR <- TermDocumentMatrix(Corpus_SCAR,
                               control = list(stemming = TRUE,
                                              stopwords = TRUE,
                                              removeNumbers = TRUE,
                                              removePunctuation = TRUE,
                                              stripWhiteSpace = TRUE))
# Step 10.6 Remove Sparse Terms 
# Step 10.6a.1 - 80% Sparsity - 345 terms, 122 docs. 
tdms_SCAR_080 <- removeSparseTerms(tdm_SCAR, 0.90)
tdms_SCAR_080
# Step 10.7 - Convert to CSV 
tdms_SCAR_080_matrix <- as.matrix(tdms_SCAR_080)
tdms_SCAR_080_frame <- data.frame(tdms_SCAR_080_matrix)
write.csv(tdms_SCAR_080_frame, "tdm_SCAR_080.csv", quote = FALSE)

# Step 11.1 - GENERATE NGRAM TOKENIZERS

# Generate Tokenizers for various "n" grams 
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), 
                                             paste, collapse = " "),
                                             use.names = FALSE)

TrigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), 
                                              paste, collapse = " "),
                                              use.names = FALSE)

FourgramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 4), 
                                              paste, collapse = " "),
                                              use.names = FALSE)

FivegramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 5),
                                               paste, collapse = " "),
                                               use.names = FALSE)

# STEP 11.2 GENERATE NGRAMS FOR AARI 

parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AARI") 
dir(cname)

bigrams_AARI <- TermDocumentMatrix(Corpus_AARI,
                                   control = list(tokenize = BigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                                  ))


trigrams_AARI <- TermDocumentMatrix(Corpus_AARI,
                                   control = list(tokenize = TrigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                                  ))

fourgrams_AARI <- TermDocumentMatrix(Corpus_AARI,
                                    control = list(tokenize = FourgramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                                   ))

fivegrams_AARI <- TermDocumentMatrix(Corpus_AARI,
                                     control = list(tokenize = FivegramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                                    ))
'
   
bigrams_AllFiles080 <- removeSparseTerms(bigrams_AllFiles, 0.900)
'


bigrams_AARI_matrix <- as.matrix(bigrams_AARI)
bigrams_AARI_frame <- data.frame(bigrams_AARI_matrix)
write.csv(bigrams_AARI_frame, "bigrams_AARI_000.csv", quote = FALSE)
trigrams_AARI_matrix <- as.matrix(trigrams_AARI)
trigrams_AARI_frame <- data.frame(trigrams_AARI_matrix)
write.csv(trigrams_AARI_frame, "trigrams_AARI_000.csv", quote = FALSE)
fourgrams_AARI_matrix <- as.matrix(fourgrams_AARI)
fourgrams_AARI_frame <- data.frame(fourgrams_AARI_matrix)
write.csv(fourgrams_AARI_frame, "fourgrams_AARI_000.csv", quote = FALSE)
fivegrams_AARI_matrix <- as.matrix(fivegrams_AARI)
fivegrams_AARI_frame <- data.frame(fivegrams_AARI_matrix)
write.csv(fivegrams_AARI_frame, "fivegrams_AARI_000.csv", quote = FALSE)

# STEP 11.3 NGRAMS FOR ACECRC

parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ACECRC") 
dir(cname)

bigrams_ACECRC <- TermDocumentMatrix(Corpus_ACECRC,
                                   control = list(tokenize = BigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                                   ))


trigrams_ACECRC <- TermDocumentMatrix(Corpus_ACECRC,
                                    control = list(tokenize = TrigramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                                   ))

fourgrams_ACECRC <- TermDocumentMatrix(Corpus_ACECRC,
                                     control = list(tokenize = FourgramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                                    ))

fivegrams_ACECRC <- TermDocumentMatrix(Corpus_ACECRC,
                                     control = list(tokenize = FivegramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                                    ))

bigrams_ACECRC_080 <- removeSparseTerms(bigrams_ACECRC, 0.85)
trigrams_ACECRC_080 <- removeSparseTerms(trigrams_ACECRC, 0.85)
fourgrams_ACECRC_080 <- removeSparseTerms(fourgrams_ACECRC, 0.85)
fivegrams_ACECRC_080 <- removeSparseTerms(fivegrams_ACECRC, 0.85)

'

bigrams_AllFiles080 <- removeSparseTerms(bigrams_AllFiles, 0.900)
'

bigrams_ACECRC_matrix <- as.matrix(bigrams_ACECRC)
bigrams_ACECRC_frame <- data.frame(bigrams_ACECRC_matrix)
write.csv(bigrams_ACECRC_frame, "bigrams_ACECRC_000.csv", quote = FALSE)
trigrams_ACECRC_matrix <- as.matrix(trigrams_ACECRC)
trigrams_ACECRC_frame <- data.frame(trigrams_ACECRC_matrix)
write.csv(trigrams_ACECRC_frame, "trigrams_ACECRC_000.csv", quote = FALSE)
fourgrams_ACECRC_matrix <- as.matrix(fourgrams_ACECRC)
fourgrams_ACECRC_frame <- data.frame(fourgrams_ACECRC_matrix)
write.csv(fourgrams_ACECRC_frame, "fourgrams_ACECRC_000.csv", quote = FALSE)
fivegrams_ACECRC_matrix <- as.matrix(fivegrams_ACECRC)
fivegrams_ACECRC_frame <- data.frame(fivegrams_ACECRC_matrix)
write.csv(fivegrams_ACECRC_frame, "fivegrams_ACECRC_000.csv", quote = FALSE)

bigrams_ACECRC_080_matrix <- as.matrix(bigrams_ACECRC_080)
bigrams_ACECRC_080_frame <- data.frame(bigrams_ACECRC_080_matrix)
write.csv(bigrams_ACECRC_080_frame, "2grams_ACECRC_080.csv", quote = FALSE)
trigrams_ACECRC_080_matrix <- as.matrix(trigrams_ACECRC)
trigrams_ACECRC_080_frame <- data.frame(trigrams_ACECRC_080_matrix)
write.csv(trigrams_ACECRC_080_frame, "3grams_ACECRC_080.csv", quote = FALSE)
fourgrams_ACECRC_080_matrix <- as.matrix(fourgrams_ACECRC)
fourgrams_ACECRC_080_frame <- data.frame(fourgrams_ACECRC_080_matrix)
write.csv(fourgrams_ACECRC_080_frame, "4grams_ACECRC_080.csv", quote = FALSE)
fivegrams_ACECRC_080_matrix <- as.matrix(fivegrams_ACECRC)
fivegrams_ACECRC_080_frame <- data.frame(fivegrams_ACECRC_080_matrix)
write.csv(fivegrams_ACECRC_080_frame, "5grams_ACECRC_080.csv", quote = FALSE)

# STEP 11.4 NGRAMS FOR ALL FILES 

parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AllFiles_") 
dir(cname)


bigrams_AllFiles <- TermDocumentMatrix(Corpus_AllFiles,
                                     control = list(tokenize = BigramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))


trigrams_AllFiles <- TermDocumentMatrix(Corpus_AllFiles,
                                      control = list(tokenize = TrigramTokenizer,
                                                     stemming = TRUE,
                                                     stopwords = TRUE,
                                                     removeNumbers = TRUE,
                                                     removePunctuation = TRUE,
                                                     stripWhitespace = TRUE
                                      ))

fourgrams_AllFiles <- TermDocumentMatrix(Corpus_AllFiles,
                                       control = list(tokenize = FourgramTokenizer,
                                                      stemming = TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      removePunctuation = TRUE,
                                                      stripWhitespace = TRUE
                                       ))

fivegrams_AllFiles <- TermDocumentMatrix(Corpus_AllFiles,
                                       control = list(tokenize = FivegramTokenizer,
                                                      stemming = TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      removePunctuation = TRUE,
                                                      stripWhitespace = TRUE
                                       ))


bigrams_AllFiles_080 <- removeSparseTerms(bigrams_AllFiles, 0.90)
trigrams_AllFiles_080 <- removeSparseTerms(trigrams_AllFiles, 0.90)
fourgrams_AllFiles_080 <- removeSparseTerms(fourgrams_AllFiles, 0.90)
fivegrams_AllFiles_080 <- removeSparseTerms(fivegrams_AllFiles, 0.90)

bigrams_AllFiles_080_matrix <- as.matrix(bigrams_AllFiles_080)
bigrams_AllFiles_080_frame <- data.frame(bigrams_AllFiles_080_matrix)
write.csv(bigrams_AllFiles_080_frame, "2grams_AllFiles_080_080.csv", quote = FALSE)
trigrams_AllFiles_080_matrix <- as.matrix(trigrams_AllFiles_080)
trigrams_AllFiles_080_frame <- data.frame(trigrams_AllFiles_080_matrix)
write.csv(trigrams_AllFiles_080_frame, "3grams_AllFiles_080_080.csv", quote = FALSE)
fourgrams_AllFiles_080_matrix <- as.matrix(fourgrams_AllFiles_080)
fourgrams_AllFiles_080_frame <- data.frame(fourgrams_AllFiles_080_matrix)
write.csv(fourgrams_AllFiles_080_frame, "4grams_AllFiles_080_080.csv", quote = FALSE)
fivegrams_AllFiles_080_matrix <- as.matrix(fivegrams_AllFiles_080)
fivegrams_AllFiles_080_frame <- data.frame(fivegrams_AllFiles_080_matrix)
write.csv(fivegrams_AllFiles_080_frame, "5grams_AllFiles_080_080.csv", quote = FALSE)

# 11.5 - NGrams for all Non-ATS

parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("AllNonATS") 
dir(cname)


bigrams_AllNonATS <- TermDocumentMatrix(Corpus_AllNonATS,
                                       control = list(tokenize = BigramTokenizer,
                                                      stemming = TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      removePunctuation = TRUE,
                                                      stripWhitespace = TRUE
                                       ))


trigrams_AllNonATS <- TermDocumentMatrix(Corpus_AllNonATS,
                                        control = list(tokenize = TrigramTokenizer,
                                                       stemming = TRUE,
                                                       stopwords = TRUE,
                                                       removeNumbers = TRUE,
                                                       removePunctuation = TRUE,
                                                       stripWhitespace = TRUE
                                        ))

fourgrams_AllNonATS <- TermDocumentMatrix(Corpus_AllNonATS,
                                         control = list(tokenize = FourgramTokenizer,
                                                        stemming = TRUE,
                                                        stopwords = TRUE,
                                                        removeNumbers = TRUE,
                                                        removePunctuation = TRUE,
                                                        stripWhitespace = TRUE
                                         ))

fivegrams_AllNonATS <- TermDocumentMatrix(Corpus_AllNonATS,
                                         control = list(tokenize = FivegramTokenizer,
                                                        stemming = TRUE,
                                                        stopwords = TRUE,
                                                        removeNumbers = TRUE,
                                                        removePunctuation = TRUE,
                                                        stripWhitespace = TRUE
                                         ))



bigrams_AllNonATS_080 <- removeSparseTerms(bigrams_AllNonATS, 0.89)
trigrams_AllNonATS_080 <- removeSparseTerms(trigrams_AllNonATS, 0.89)
fourgrams_AllNonATS_080 <- removeSparseTerms(fourgrams_AllNonATS, 0.89)
fivegrams_AllNonATS_080 <- removeSparseTerms(fivegrams_AllNonATS, 0.89)

bigrams_AllNonATS_080_matrix <- as.matrix(bigrams_AllNonATS_080)
bigrams_AllNonATS_080_frame <- data.frame(bigrams_AllNonATS_080_matrix)
write.csv(bigrams_AllNonATS_080_frame, "2grams_AllNonATS_080_080.csv", quote = FALSE)
trigrams_AllNonATS_080_matrix <- as.matrix(trigrams_AllNonATS_080)
trigrams_AllNonATS_080_frame <- data.frame(trigrams_AllNonATS_080_matrix)
write.csv(trigrams_AllNonATS_080_frame, "3grams_AllNonATS_080_080.csv", quote = FALSE)
fourgrams_AllNonATS_080_matrix <- as.matrix(fourgrams_AllNonATS_080)
fourgrams_AllNonATS_080_frame <- data.frame(fourgrams_AllNonATS_080_matrix)
write.csv(fourgrams_AllNonATS_080_frame, "4grams_AllNonATS_080_080.csv", quote = FALSE)
fivegrams_AllNonATS_080_matrix <- as.matrix(fivegrams_AllNonATS_080)
fivegrams_AllNonATS_080_frame <- data.frame(fivegrams_AllNonATS_080_matrix)
write.csv(fivegrams_AllNonATS_080_frame, "5grams_AllNonATS_080_080.csv", quote = FALSE)


# 11.6 N GRAMS FOR All ARC

parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ARC") 
dir(cname)


bigrams_ARC <- TermDocumentMatrix(Corpus_ARC,
                                        control = list(tokenize = BigramTokenizer,
                                                       stemming = TRUE,
                                                       stopwords = TRUE,
                                                       removeNumbers = TRUE,
                                                       removePunctuation = TRUE,
                                                       stripWhitespace = TRUE
                                        ))


trigrams_ARC <- TermDocumentMatrix(Corpus_ARC,
                                         control = list(tokenize = TrigramTokenizer,
                                                        stemming = TRUE,
                                                        stopwords = TRUE,
                                                        removeNumbers = TRUE,
                                                        removePunctuation = TRUE,
                                                        stripWhitespace = TRUE
                                         ))

fourgrams_ARC <- TermDocumentMatrix(Corpus_ARC,
                                          control = list(tokenize = FourgramTokenizer,
                                                         stemming = TRUE,
                                                         stopwords = TRUE,
                                                         removeNumbers = TRUE,
                                                         removePunctuation = TRUE,
                                                         stripWhitespace = TRUE
                                          ))

fivegrams_ARC <- TermDocumentMatrix(Corpus_ARC,
                                          control = list(tokenize = FivegramTokenizer,
                                                         stemming = TRUE,
                                                         stopwords = TRUE,
                                                         removeNumbers = TRUE,
                                                         removePunctuation = TRUE,
                                                         stripWhitespace = TRUE
                                          ))



bigrams_ARC_080 <- removeSparseTerms(bigrams_ARC, 0.91)
trigrams_ARC_080 <- removeSparseTerms(trigrams_ARC, 0.91)
fourgrams_ARC_080 <- removeSparseTerms(fourgrams_ARC, 0.91)
fivegrams_ARC_080 <- removeSparseTerms(fivegrams_ARC, 0.91)

bigrams_ARC_080_matrix <- as.matrix(bigrams_ARC_080)
bigrams_ARC_080_frame <- data.frame(bigrams_ARC_080_matrix)
write.csv(bigrams_ARC_080_frame, "2grams_ARC_080_080.csv", quote = FALSE)
trigrams_ARC_080_matrix <- as.matrix(trigrams_ARC_080)
trigrams_ARC_080_frame <- data.frame(trigrams_ARC_080_matrix)
write.csv(trigrams_ARC_080_frame, "3grams_ARC_080_080.csv", quote = FALSE)
fourgrams_ARC_080_matrix <- as.matrix(fourgrams_ARC_080)
fourgrams_ARC_080_frame <- data.frame(fourgrams_ARC_080_matrix)
write.csv(fourgrams_ARC_080_frame, "4grams_ARC_080_080.csv", quote = FALSE)
fivegrams_ARC_080_matrix <- as.matrix(fivegrams_ARC_080)
fivegrams_ARC_080_frame <- data.frame(fivegrams_ARC_080_matrix)
write.csv(fivegrams_ARC_080_frame, "5grams_ARC_080_080.csv", quote = FALSE)


# 11.6 - NGRAMS FOR ASOC 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ASOC") 
dir(cname)


bigrams_ASOC <- TermDocumentMatrix(Corpus_ASOC,
                                  control = list(tokenize = BigramTokenizer,
                                                 stemming = TRUE,
                                                 stopwords = TRUE,
                                                 removeNumbers = TRUE,
                                                 removePunctuation = TRUE,
                                                 stripWhitespace = TRUE
                                  ))


trigrams_ASOC <- TermDocumentMatrix(Corpus_ASOC,
                                   control = list(tokenize = TrigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                   ))

fourgrams_ASOC <- TermDocumentMatrix(Corpus_ASOC,
                                    control = list(tokenize = FourgramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))

fivegrams_ASOC <- TermDocumentMatrix(Corpus_ASOC,
                                    control = list(tokenize = FivegramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))



bigrams_ASOC_080 <- removeSparseTerms(bigrams_ASOC, 0.92)
trigrams_ASOC_080 <- removeSparseTerms(trigrams_ASOC, 0.92)
fourgrams_ASOC_080 <- removeSparseTerms(fourgrams_ASOC, 0.92)
fivegrams_ASOC_080 <- removeSparseTerms(fivegrams_ASOC, 0.92)

bigrams_ASOC_080_matrix <- as.matrix(bigrams_ASOC_080)
bigrams_ASOC_080_frame <- data.frame(bigrams_ASOC_080_matrix)
write.csv(bigrams_ASOC_080_frame, "ASOC_2grams_080.csv", quote = FALSE)
trigrams_ASOC_080_matrix <- as.matrix(trigrams_ASOC_080)
trigrams_ASOC_080_frame <- data.frame(trigrams_ASOC_080_matrix)
write.csv(trigrams_ASOC_080_frame, "ASOC_3grams_080.csv", quote = FALSE)
fourgrams_ASOC_080_matrix <- as.matrix(fourgrams_ASOC_080)
fourgrams_ASOC_080_frame <- data.frame(fourgrams_ASOC_080_matrix)
write.csv(fourgrams_ASOC_080_frame, "ASOC_4grams_080.csv", quote = FALSE)
fivegrams_ASOC_080_matrix <- as.matrix(fivegrams_ASOC_080)
fivegrams_ASOC_080_frame <- data.frame(fivegrams_ASOC_080_matrix)
write.csv(fivegrams_ASOC_080_frame, "ASOC_5grams_080.csv", quote = FALSE)

# 11.7 - NGRAMS FOR ATS 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("ATS") 
dir(cname)


bigrams_ATS <- TermDocumentMatrix(Corpus_ATS,
                                   control = list(tokenize = BigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                   ))


trigrams_ATS <- TermDocumentMatrix(Corpus_ATS,
                                    control = list(tokenize = TrigramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))

fourgrams_ATS <- TermDocumentMatrix(Corpus_ATS,
                                     control = list(tokenize = FourgramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))

fivegrams_ATS <- TermDocumentMatrix(Corpus_ATS,
                                     control = list(tokenize = FivegramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))

bigrams_ATS_080 <- removeSparseTerms(bigrams_ATS, 0.90)     
trigrams_ATS_080 <- removeSparseTerms(trigrams_ATS, 0.90)
fourgrams_ATS_080 <- removeSparseTerms(fourgrams_ATS, 0.90)
fivegrams_ATS_080 <- removeSparseTerms(fivegrams_ATS, 0.90)

bigrams_ATS_080_matrix <- as.matrix(bigrams_ATS_080)
bigrams_ATS_080_frame <- data.frame(bigrams_ATS_080_matrix)
write.csv(bigrams_ATS_080_frame, "ATS_2grams_080.csv", quote = FALSE)
trigrams_ATS_080_matrix <- as.matrix(trigrams_ATS_080)
trigrams_ATS_080_frame <- data.frame(trigrams_ATS_080_matrix)
write.csv(trigrams_ATS_080_frame, "ATS_3grams_080.csv", quote = FALSE)
fourgrams_ATS_080_matrix <- as.matrix(fourgrams_ATS_080)
fourgrams_ATS_080_frame <- data.frame(fourgrams_ATS_080_matrix)
write.csv(fourgrams_ATS_080_frame, "ATS_4grams_080.csv", quote = FALSE)
fivegrams_ATS_080_matrix <- as.matrix(fivegrams_ATS_080)
fivegrams_ATS_080_frame <- data.frame(fivegrams_ATS_080_matrix)
write.csv(fivegrams_ATS_080_frame, "ATS_5grams_080.csv", quote = FALSE)


# 11.8 - NGRAMS FOR COMNAP 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("CCAMLR") 
dir(cname)


bigrams_CCAMLR <- TermDocumentMatrix(Corpus_CCAMLR,
                                  control = list(tokenize = BigramTokenizer,
                                                 stemming = TRUE,
                                                 stopwords = TRUE,
                                                 removeNumbers = TRUE,
                                                 removePunctuation = TRUE,
                                                 stripWhitespace = TRUE
                                  ))


trigrams_CCAMLR <- TermDocumentMatrix(Corpus_CCAMLR,
                                   control = list(tokenize = TrigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                   ))

fourgrams_CCAMLR <- TermDocumentMatrix(Corpus_CCAMLR,
                                    control = list(tokenize = FourgramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))

fivegrams_CCAMLR <- TermDocumentMatrix(Corpus_CCAMLR,
                                    control = list(tokenize = FivegramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))



bigrams_CCAMLR_080 <- removeSparseTerms(bigrams_CCAMLR, 0.92)     
trigrams_CCAMLR_080 <- removeSparseTerms(trigrams_CCAMLR, 0.92)
fourgrams_CCAMLR_080 <- removeSparseTerms(fourgrams_CCAMLR, 0.92)
fivegrams_CCAMLR_080 <- removeSparseTerms(fivegrams_CCAMLR, 0.92)

bigrams_CCAMLR_080_matrix <- as.matrix(bigrams_CCAMLR_080)
bigrams_CCAMLR_080_frame <- data.frame(bigrams_CCAMLR_080_matrix)
write.csv(bigrams_CCAMLR_080_frame, "CCAMLR_2grams_080.csv", quote = FALSE)
trigrams_CCAMLR_080_matrix <- as.matrix(trigrams_CCAMLR_080)
trigrams_CCAMLR_080_frame <- data.frame(trigrams_CCAMLR_080_matrix)
write.csv(trigrams_CCAMLR_080_frame, "CCAMLR_3grams_080.csv", quote = FALSE)
fourgrams_CCAMLR_080_matrix <- as.matrix(fourgrams_CCAMLR_080)
fourgrams_CCAMLR_080_frame <- data.frame(fourgrams_CCAMLR_080_matrix)
write.csv(fourgrams_CCAMLR_080_frame, "CCAMLR_4grams_080.csv", quote = FALSE)
fivegrams_CCAMLR_080_matrix <- as.matrix(fivegrams_CCAMLR_080)
fivegrams_CCAMLR_080_frame <- data.frame(fivegrams_CCAMLR_080_matrix)
write.csv(fivegrams_CCAMLR_080_frame, "CCAMLR_5grams_080.csv", quote = FALSE)

# 11.9 - NGRAMS FOR CCAS 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("CCAS") 
dir(cname)


bigrams_CCAS <- TermDocumentMatrix(Corpus_CCAS,
                                  control = list(tokenize = BigramTokenizer,
                                                 stemming = TRUE,
                                                 stopwords = TRUE,
                                                 removeNumbers = TRUE,
                                                 removePunctuation = TRUE,
                                                 stripWhitespace = TRUE
                                  ))


trigrams_CCAS <- TermDocumentMatrix(Corpus_CCAS,
                                   control = list(tokenize = TrigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                   ))

fourgrams_CCAS <- TermDocumentMatrix(Corpus_CCAS,
                                    control = list(tokenize = FourgramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))

fivegrams_CCAS <- TermDocumentMatrix(Corpus_CCAS,
                                    control = list(tokenize = FivegramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))



bigrams_CCAS_058_matrix <- as.matrix(bigrams_CCAS)
bigrams_CCAS_058_frame <- data.frame(bigrams_CCAS_058_matrix)
write.csv(bigrams_CCAS_058_frame, "CCAS_2grams_058.csv", quote = FALSE)
trigrams_CCAS_058_matrix <- as.matrix(trigrams_CCAS)
trigrams_CCAS_058_frame <- data.frame(trigrams_CCAS_058_matrix)
write.csv(trigrams_CCAS_058_frame, "CCAS_3grams_058.csv", quote = FALSE)
fourgrams_CCAS_058_matrix <- as.matrix(fourgrams_CCAS)
fourgrams_CCAS_058_frame <- data.frame(fourgrams_CCAS_058_matrix)
write.csv(fourgrams_CCAS_058_frame, "CCAS_4grams_058.csv", quote = FALSE)
fivegrams_CCAS_058_matrix <- as.matrix(fivegrams_CCAS)
fivegrams_CCAS_058_frame <- data.frame(fivegrams_CCAS_058_matrix)
write.csv(fivegrams_CCAS_058_frame, "CCAS_5grams_058.csv", quote = FALSE)

# 11.10 - NGRAMS FOR COMNAP 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("COMNAP") 
dir(cname)


bigrams_COMNAP <- TermDocumentMatrix(Corpus_COMNAP,
                                   control = list(tokenize = BigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                   ))


trigrams_COMNAP <- TermDocumentMatrix(Corpus_COMNAP,
                                    control = list(tokenize = TrigramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))

fourgrams_COMNAP <- TermDocumentMatrix(Corpus_COMNAP,
                                     control = list(tokenize = FourgramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))

fivegrams_COMNAP <- TermDocumentMatrix(Corpus_COMNAP,
                                     control = list(tokenize = FivegramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))



bigrams_COMNAP_080 <- removeSparseTerms(bigrams_COMNAP, 0.85)     
trigrams_COMNAP_080 <- removeSparseTerms(trigrams_COMNAP, 0.85)
fourgrams_COMNAP_080 <- removeSparseTerms(fourgrams_COMNAP, 0.85)
fivegrams_COMNAP_080 <- removeSparseTerms(fivegrams_COMNAP, 0.85)

bigrams_COMNAP_080_matrix <- as.matrix(bigrams_COMNAP_080)
bigrams_COMNAP_080_frame <- data.frame(bigrams_COMNAP_080_matrix)
write.csv(bigrams_COMNAP_080_frame, "COMNAP_2grams_080.csv", quote = FALSE)
trigrams_COMNAP_080_matrix <- as.matrix(trigrams_COMNAP_080)
trigrams_COMNAP_080_frame <- data.frame(trigrams_COMNAP_080_matrix)
write.csv(trigrams_COMNAP_080_frame, "COMNAP_3grams_080.csv", quote = FALSE)
fourgrams_COMNAP_080_matrix <- as.matrix(fourgrams_COMNAP_080)
fourgrams_COMNAP_080_frame <- data.frame(fourgrams_COMNAP_080_matrix)
write.csv(fourgrams_COMNAP_080_frame, "COMNAP_4grams_080.csv", quote = FALSE)
fivegrams_COMNAP_080_matrix <- as.matrix(fivegrams_COMNAP_080)
fivegrams_COMNAP_080_frame <- data.frame(fivegrams_COMNAP_080_matrix)
write.csv(fivegrams_COMNAP_080_frame, "COMNAP_5grams_080.csv", quote = FALSE)
# 11.11 - NGRAMS FOR IAATO 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("IAATO") 
dir(cname)


bigrams_IAATO <- TermDocumentMatrix(Corpus_IAATO,
                                     control = list(tokenize = BigramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))


trigrams_IAATO <- TermDocumentMatrix(Corpus_IAATO,
                                      control = list(tokenize = TrigramTokenizer,
                                                     stemming = TRUE,
                                                     stopwords = TRUE,
                                                     removeNumbers = TRUE,
                                                     removePunctuation = TRUE,
                                                     stripWhitespace = TRUE
                                      ))

fourgrams_IAATO <- TermDocumentMatrix(Corpus_IAATO,
                                       control = list(tokenize = FourgramTokenizer,
                                                      stemming = TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      removePunctuation = TRUE,
                                                      stripWhitespace = TRUE
                                       ))

fivegrams_IAATO <- TermDocumentMatrix(Corpus_IAATO,
                                       control = list(tokenize = FivegramTokenizer,
                                                      stemming = TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      removePunctuation = TRUE,
                                                      stripWhitespace = TRUE
                                       ))



bigrams_IAATO_080 <- removeSparseTerms(bigrams_IAATO, 0.94)     
trigrams_IAATO_080 <- removeSparseTerms(trigrams_IAATO, 0.94)
fourgrams_IAATO_080 <- removeSparseTerms(fourgrams_IAATO, 0.94)
fivegrams_IAATO_080 <- removeSparseTerms(fivegrams_IAATO, 0.94)

bigrams_IAATO_080_matrix <- as.matrix(bigrams_IAATO_080)
bigrams_IAATO_080_frame <- data.frame(bigrams_IAATO_080_matrix)
write.csv(bigrams_IAATO_080_frame, "IAATO_2grams_080.csv", quote = FALSE)
trigrams_IAATO_080_matrix <- as.matrix(trigrams_IAATO_080)
trigrams_IAATO_080_frame <- data.frame(trigrams_IAATO_080_matrix)
write.csv(trigrams_IAATO_080_frame, "IAATO_3grams_080.csv", quote = FALSE)
fourgrams_IAATO_080_matrix <- as.matrix(fourgrams_IAATO_080)
fourgrams_IAATO_080_frame <- data.frame(fourgrams_IAATO_080_matrix)
write.csv(fourgrams_IAATO_080_frame, "IAATO_4grams_080.csv", quote = FALSE)
fivegrams_IAATO_080_matrix <- as.matrix(fivegrams_IAATO_080)
fivegrams_IAATO_080_frame <- data.frame(fivegrams_IAATO_080_matrix)
write.csv(fivegrams_IAATO_080_frame, "IAATO_5grams_080.csv", quote = FALSE)
# 11.12 - NGRAMS FOR IMAS 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("IMAS") 
dir(cname)


bigrams_IMAS <- TermDocumentMatrix(Corpus_IMAS,
                                    control = list(tokenize = BigramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))


trigrams_IMAS <- TermDocumentMatrix(Corpus_IMAS,
                                     control = list(tokenize = TrigramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))

fourgrams_IMAS <- TermDocumentMatrix(Corpus_IMAS,
                                      control = list(tokenize = FourgramTokenizer,
                                                     stemming = TRUE,
                                                     stopwords = TRUE,
                                                     removeNumbers = TRUE,
                                                     removePunctuation = TRUE,
                                                     stripWhitespace = TRUE
                                      ))

fivegrams_IMAS <- TermDocumentMatrix(Corpus_IMAS,
                                      control = list(tokenize = FivegramTokenizer,
                                                     stemming = TRUE,
                                                     stopwords = TRUE,
                                                     removeNumbers = TRUE,
                                                     removePunctuation = TRUE,
                                                     stripWhitespace = TRUE
                                      ))



bigrams_IMAS_080 <- removeSparseTerms(bigrams_IMAS, 0.89)     
trigrams_IMAS_080 <- removeSparseTerms(trigrams_IMAS, 0.89)
fourgrams_IMAS_080 <- removeSparseTerms(fourgrams_IMAS, 0.89)
fivegrams_IMAS_080 <- removeSparseTerms(fivegrams_IMAS, 0.89)

bigrams_IMAS_080_matrix <- as.matrix(bigrams_IMAS_080)
bigrams_IMAS_080_frame <- data.frame(bigrams_IMAS_080_matrix)
write.csv(bigrams_IMAS_080_frame, "IMAS_2grams_080.csv", quote = FALSE)
trigrams_IMAS_080_matrix <- as.matrix(trigrams_IMAS_080)
trigrams_IMAS_080_frame <- data.frame(trigrams_IMAS_080_matrix)
write.csv(trigrams_IMAS_080_frame, "IMAS_3grams_080.csv", quote = FALSE)
fourgrams_IMAS_080_matrix <- as.matrix(fourgrams_IMAS_080)
fourgrams_IMAS_080_frame <- data.frame(fourgrams_IMAS_080_matrix)
write.csv(fourgrams_IMAS_080_frame, "IMAS_4grams_080.csv", quote = FALSE)
fivegrams_IMAS_080_matrix <- as.matrix(fivegrams_IMAS_080)
fivegrams_IMAS_080_frame <- data.frame(fivegrams_IMAS_080_matrix)
write.csv(fivegrams_IMAS_080_frame, "IMAS_5grams_080.csv", quote = FALSE)


# 11.13 - NGRAMS FOR SCAR 
parent.folder <-"/Users/danyalagos/Dropbox/LagosRAFolder_Summer16/ReadyData"
setwd(parent.folder)
cname <- file.path("SCAR") 
dir(cname)


bigrams_SCAR <- TermDocumentMatrix(Corpus_SCAR,
                                   control = list(tokenize = BigramTokenizer,
                                                  stemming = TRUE,
                                                  stopwords = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stripWhitespace = TRUE
                                   ))


trigrams_SCAR <- TermDocumentMatrix(Corpus_SCAR,
                                    control = list(tokenize = TrigramTokenizer,
                                                   stemming = TRUE,
                                                   stopwords = TRUE,
                                                   removeNumbers = TRUE,
                                                   removePunctuation = TRUE,
                                                   stripWhitespace = TRUE
                                    ))

fourgrams_SCAR <- TermDocumentMatrix(Corpus_SCAR,
                                     control = list(tokenize = FourgramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))

fivegrams_SCAR <- TermDocumentMatrix(Corpus_SCAR,
                                     control = list(tokenize = FivegramTokenizer,
                                                    stemming = TRUE,
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    removePunctuation = TRUE,
                                                    stripWhitespace = TRUE
                                     ))




bigrams_SCAR_080 <- removeSparseTerms(bigrams_SCAR, 0.90)     
trigrams_SCAR_080 <- removeSparseTerms(trigrams_SCAR, 0.90)
fourgrams_SCAR_080 <- removeSparseTerms(fourgrams_SCAR, 0.90)
fivegrams_SCAR_080 <- removeSparseTerms(fivegrams_SCAR, 0.90)

bigrams_SCAR_080_matrix <- as.matrix(bigrams_SCAR_080)
bigrams_SCAR_080_frame <- data.frame(bigrams_SCAR_080_matrix)
write.csv(bigrams_SCAR_080_frame, "SCAR_2grams_080.csv", quote = FALSE)
trigrams_SCAR_080_matrix <- as.matrix(trigrams_SCAR_080)
trigrams_SCAR_080_frame <- data.frame(trigrams_SCAR_080_matrix)
write.csv(trigrams_SCAR_080_frame, "SCAR_3grams_080.csv", quote = FALSE)
fourgrams_SCAR_080_matrix <- as.matrix(fourgrams_SCAR_080)
fourgrams_SCAR_080_frame <- data.frame(fourgrams_SCAR_080_matrix)
write.csv(fourgrams_SCAR_080_frame, "SCAR_4grams_080.csv", quote = FALSE)
fivegrams_SCAR_080_matrix <- as.matrix(fivegrams_SCAR_080)
fivegrams_SCAR_080_frame <- data.frame(fivegrams_SCAR_080_matrix)
write.csv(fivegrams_SCAR_080_frame, "SCAR_5grams_080.csv", quote = FALSE)

