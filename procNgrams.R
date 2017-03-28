# UPdate to Exploratory Analysis

procNgrams <- function() {
  
#Assume data have been unziped and downloaded
library(quanteda)
library(stringr)

#Open the connection and use either readLines or scan functions
conFile <- file("cleanedSet.txt", "r")
 

#Read all the lines from the files
allLines <- readLines(conFile) ## Read lines from file

#Close connections
close(conFile)


#Run another iteration to remove punctuation
rmSpaces <- gsub("[^a-zA-Z0-9[:space:]]", " ", allLines)

#Perform Pre-processing
#Convert words to lower case, remove punctuation, numbers and white spaces
tokensAll <- tokenize(char_tolower(rmSpaces), removePunct = TRUE, 
                      removeNumbers = TRUE, removeSeparators = TRUE)

#Remove addtl stopwords/characters that showed up in the top features initially
tokensAll <- removeFeatures(tokensAll, c(stopwords("english"), "c", "t", "s"))

#Create the DTM
dtm1 <- dfm(tokensAll)

#Calculate the total tokens and unique words (features)
totalWords <- sum(colSums(dtm1)) #total words
totalFeatures <- length(featnames(dtm1)) #unique words - features

#Tokenize the 2-grams and create the Document Term Matrix (DTM)
tokens2gram <- tokens_ngrams(tokensAll, 2)
dtm2 <- dfm(tokens2gram)

#Tokenize the 3-grams and create the Document Term Matrix (DTM)
tokens3gram <- tokens_ngrams(tokensAll, 3)
dtm3 <- dfm(tokens3gram)

#Tokenize the 4-grams and create the Document Term Matrix (DTM)
#tokens4gram <- tokens_ngrams(tokensAll, 4)
#dtm4 <- dfm(tokens4gram)

#Create the dictionaries
#Create the function to extract DTM info and convert to data frame
ngramToDF <- function(calcDTM) {
  df.ngram <- data.frame(Content = featnames(calcDTM), Frequency = colSums(calcDTM), 
                         row.names = NULL, stringsAsFactors = FALSE)
  df.ngram <- df.ngram[order(df.ngram$Frequency, decreasing = TRUE), ]
  return(df.ngram)
}

#Create the data tables for n-gram and order descending
#Data tables chosen over data frames for efficient searching (keys and fast binary seach)
library(data.table)
set.seed(45L)
DT.ngram1 <- as.data.table(ngramToDF(dtm1))
DT.ngram2 <- as.data.table(ngramToDF(dtm2))
DT.ngram3 <- as.data.table(ngramToDF(dtm3))
#DT.ngram4 <- as.data.table(ngramToDF(dtm4))

#Create function to subset the n-grams from data tables
#Collect the FirstTerms
trimNgram <- function(ngramChar) {
  temp <- unlist(strsplit(ngramChar, "_"))
  sizeNgram <- length(temp)
  lengthNgram <- nchar(ngramChar)
  lengthTemp <- nchar(temp[sizeNgram])
  firstTerm <- substr(ngramChar,1, lengthNgram-lengthTemp-1)
}

#Collect the LastTerms, based on previously defined function
lastTerm <- function(ngramChar) {
  sizeFirst <- nchar(trimNgram(ngramChar))
  lengthNgram <- nchar(ngramChar)
  output <- substr(ngramChar, sizeFirst+2, lengthNgram)
}

#Split and reshape the 2-gram  
DT.ngram2[, c("FirstTerm", "LastTerm") := tstrsplit(Content, "_", fixed=TRUE)]

#Split and reshape the 3-gram   
DT.ngram3[, c("T1", "T2", "LastTerm") := tstrsplit(Content, "_", fixed=TRUE)]
DT.ngram3[, FirstTerm := paste0(T1, "_", T2)]
DT.ngram3[, c("T1","T2") := NULL]

#Split and reshape the 4-gram
#DT.ngram4[, c("T1", "T2", "T3", "LastTerm") := tstrsplit(Content, "_", fixed=TRUE)]
#DT.ngram4[, FirstTerm := paste0(T1, "_", T2, "_", T3)]
#DT.ngram4[, c("T1","T2", "T3") := NULL]


#Prepare the left_over pprobability tables with the first terms only
source("myBackoffMethod.R")
source("myEstimateUsingKatz_no4.R")

#DT4_leftover <- discountFn(DT.ngram4)
DT3_leftover <- discountFn(DT.ngram3)
DT2_leftover <- discountFn(DT.ngram2)


#Save the data locally and re-load in Shinny app in order to save processing time
#This approach favors handling of large % of the initial SWift test data
library(readr)

#Save n-gram tables with discount coefficient
system.time(write_csv(DT.ngram3, path = "ngram3.csv"))
system.time(write_csv(DT.ngram2, path = "ngram2.csv"))

#Save tables with calculated leftover probabilities
system.time(write_csv(DT3_leftover, path = "ngram3_leftover.csv"))
system.time(write_csv(DT2_leftover, path = "ngram2_leftover.csv"))

#system.time({
#DT_test3 <- as.data.table(read_csv("ngram3.csv", col_names = TRUE, progress = interactive()))
#DT_test2 <- as.data.table(read_csv("ngram2.csv", col_names = TRUE, progress = interactive()))
#DT_test3_left <- as.data.table(read_csv("ngram3_leftover.csv", col_names = TRUE, progress = interactive()))
#DT_test2_left <- read_csv("ngram2_leftover.csv", col_names = TRUE, progress = interactive())
#})
  
  
#system.time({
#proc_outcome(inputString, DT.ngram4, DT.ngram3, DT.ngram2, DT4_leftover, DT3_leftover)
#proc_outcome(inputString, DT.ngram3, DT.ngram2, DT3_leftover)

}


