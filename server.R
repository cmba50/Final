library(shiny)
library(readr)
library(RCurl)

  
  
shinyServer(
  function(input, output, session) {
    #Get the user input and pre-process
    #Include a button to load the vocabulary
    
    outputVoc <- reactive({
      # Load the vocabulary when button pressed
      
      if (input$loadNgram) {
        withProgress(message = "Loading vocabulary...", value = 0, {
          
          #Save the data locally and re-load in Shinny app in order to save processing time
          #This approach favors handling of large % of the initial SWift test datad
          #Load the underlying data tables with pre-processed n-grams from vocabulary
          file3 <- "https://raw.githubusercontent.com/cmba50/Capstone_Project/master/ngram3.csv"
          file2 <- "https://raw.githubusercontent.com/cmba50/Capstone_Project/master/ngram2.csv"
          file3_left <- "https://raw.githubusercontent.com/cmba50/Capstone_Project/master/ngram3_leftover.csv"
          
          #Load the tables and updates the progress bar with % of overall loading time
          set.seed(45L)
          DT3_leftover <- as.data.table(read_csv(file3_left, col_names = TRUE, progress = interactive()))
          incProgress(0.11)
          
          DT.ngram2 <- as.data.table(read_csv(file2, col_names = TRUE, progress = interactive()))
          incProgress(0.24)
            
          DT.ngram3 <- as.data.table(read_csv(file3, col_names = TRUE, progress = interactive()))
          incProgress(0.65)
          Sys.sleep(0.1)
          
          loadedVoc <- list(DT3 = DT.ngram3, DT2 = DT.ngram2, DT3_left = DT3_leftover)
        })
      }
      
    })
    
    outputMatch <- reactive({
      loadedVoc <- outputVoc()
      DT.ngram3 <- loadedVoc$DT3
      DT.ngram2 <- loadedVoc$DT2
      DT3_leftover <- loadedVoc$DT3_left
   
      proc_outcome(input$inpNgram, DT.ngram3, DT.ngram2, DT3_leftover)
    })
    
    
    output$predWord <- renderText({
      if (input$loadNgram == 0)  {
        if (nchar(input$inpNgram) > 0) {
          print("Please load the vocabulary first!") 
        } else print("")
      }
      else if (input$loadNgram == 1) {
        outputVoc()
        if (nchar(input$inpNgram) > 0) outputMatch()
      }
      else if (input$loadNgram > 1) {
        print("The vocabulary is already loaded, please type a sentence")
        outputMatch()
      }
      
    })
    
  }
)
