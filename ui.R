#This is an applicaiton that predicts the next word based on ngram user input

library(shiny)

# Define UI for the application
shinyUI(fluidPage(
    titlePanel("Next Word Prediction"),
    sidebarLayout(
      sidebarPanel(
        # Create the text input box
        textInput("inpNgram", label = h5("Please provide the text to be completed"), 
                  value = ""),
        hr(),
        actionButton("loadNgram", "Load Vocabulary"),
        helpText("Please load the vocabulary only once then type sentences")
      ),
      mainPanel(
        h3("Best Predicted Next Words - left to right"),
        #hr(),
        strong(verbatimTextOutput('predWord'))
      )
     )
    )
)
