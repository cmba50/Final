library(stringr)
library(data.table)
library(NLP)
library(tm)

#These function is used to pre-process the input string
getLastTerms <- function(inputString, num){
  
  # Preprocessing the inout ngram
  inputString <- gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  
  #Remove the stop words from input for better performance (I, the, etc.)
  stopWords <- stopwords("english")
  class(stopWords)
  inputstring <- unlist(inputString)[!(unlist(inputString) %in% stopWords)]

  sepWords <- unlist(strsplit(inputString, " "))
  
  #if (length(sepWords) < num){
  #  stop("Please provide a sentence with at least two words")
  #}
  
  startWord <- length(sepWords)-num+1
  endWord <- length(sepWords)
  tempWords <- sepWords[startWord:endWord]
  
  paste(tempWords, collapse="_")
}

#This function processes the inout string and uses the Katz Backoff approach to predict the next word 
#based on n-grams from the imported tables
proc_outcome <- function(inputString, DT3, DT2, DT3_leftover){
  # Preprocessing
  #Separate terms from the input string, get the text input in order to start search for First Term
  inFirstTerm3gram <- getLastTerms(inputString, num = 2)
  
  #Search for the first term in 3-gram table from the input string, based on the input string
  #matchFTD4 <- DT4[FirstTerm == inFirstTerm3gram]
  matchFTD3 <- DT3[FirstTerm == inFirstTerm3gram]
  
  #Check whether 3-gram is matched
  if (nrow(matchFTD3) > 0){  #if N-gram is available
    # Start probability calculations
    all_freq <- sum(matchFTD3$Frequency)
    
    # Calculate conditional probability of First and Second Terms appearing in vocabulary
    matchFTD3[, Probability := round(((Discount * Frequency) / all_freq), 2), 
              by = FirstTerm][order(-Probability)]
    if (nrow(matchFTD3) > 2) {
      textout <- paste(matchFTD3$LastTerm[1],matchFTD3$LastTerm[2], 
                        matchFTD3$LastTerm[3], sep = "    ")
    }
    else if (nrow(matchFTD3) == 2)
       textout <- paste(matchFTD3$LastTerm[1],matchFTD3$LastTerm[2], sep = "    ")
    else textout <- matchFTD3$LastTerm[1]
    print(textout)
  } 
  else { #fall back on leftover probabilities, start looking in lower-level grams
    #Check whether 2-gram is matched
    matchFTD2 <- calc_beta(inputString, num = 1, DT2, DT3_leftover)
    if (nrow(matchFTD2) > 0){  #if N-gram is available
      if (nrow(matchFTD2) > 2) {
        textout <- paste(matchFTD2$LastTerm[1],matchFTD2$LastTerm[2], 
                         matchFTD2$LastTerm[3], sep = "    ")
      }
      else if (nrow(matchFTD2) == 2)
        textout <- paste(matchFTD2$LastTerm[1],matchFTD2$LastTerm[2], sep = "    ")
      else textout <- matchFTD2$LastTerm[1]
      print(textout)
    } 
    else {
      print("No match was found!")
    }
  }
}

#Function to calculate beta and alpha for back-ff method
calc_beta <- function(inputString, num, DT, DTLeftover){
  #start with DT_leftover
  # NOT found in current level => check lower levels grams
  #Separate terms from the input string, get the grams in order to start search for First Term in vocabulary
  FirstTermCurr <- getLastTerms(inputString, num)
  FirstTermNext <- getLastTerms(inputString, num+1)
  
  # Get the left-over probability so that we can distribute it for lower-order grams.
  beta <- DTLeftover[FirstTerm == FirstTermNext]$leftoverprob
  
  #Search for the first term in N_gram table from the input string, based on input string
  matchFTD <- DT[FirstTerm == FirstTermCurr]
  
  if (!is.null(matchFTD)){  #if N-gram is available
    # We only consider the final words that do not appear in N+1 grams
    
    # Start probability calculations
    all_freq <- sum(matchFTD$Frequency)
    
    if (!is.null(dim(beta))) {
      matchFTD[, Probability := round(beta/sum((matchFTD$Discount * matchFTD$Frequency) / all_freq), 2)][order(-Probability)]
    } else {
      matchFTD[, Probability := round(((matchFTD$Discount * matchFTD$Frequency) / all_freq), 2)][order(-Probability)]
    } 
    
  }
  return(matchFTD)
}