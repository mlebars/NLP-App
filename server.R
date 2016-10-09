
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(Rcpp)
source('script.R')

library(shiny)

# Define application ####

shinyServer(function(input, output) {
  
  # Reactive statement for prediction function when user input changes ####
  prediction =  reactive( {
    
    # Get input
    inputType = input$type
    dfTrain1 = getType1(inputType)
    dfTrain2 = getType2(inputType)
    dfTrain3 = getType3(inputType)
    inputText = input$text
    input1 =  getInput(inputText)[1, ]
    input2 =  getInput(inputText)[2, ]
    
    
    # Predict
    prediction = predictNextWord(input1, input2, n = 5, dfTrain1, dfTrain2, dfTrain3)
  })
  
  # Output data table ####
  output$chosentype = renderText(
    if(input$type == "Any") {
      "No type has been selected"
    } else {
      paste("The type '",input$type,"' has been selected.", sep="")
    })
  output$text = renderText({input$text})
  
  output$guess1 = renderText({
    if (is.na(prediction()$NextWord[1])) {
      "(No suggestion)"
    } else {
      prediction()$NextWord[1]
    }
    })
  
  output$guess2 = renderText({if(!is.na(prediction()$NextWord[2])){prediction()$NextWord[2]}})
  output$guess3 = renderText({if(!is.na(prediction()$NextWord[3])){prediction()$NextWord[3]}})
  output$guess4 = renderText({if(!is.na(prediction()$NextWord[4])){prediction()$NextWord[4]}})
  output$guess5 = renderText({if(!is.na(prediction()$NextWord[5])){prediction()$NextWord[5]}})

})