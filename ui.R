# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)

shinyUI(fluidPage(theme = "stylesheet.css",

  titlePanel("Guess the next word"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("type", label = ('Choose the text type'), c("Blog", "News", "Twitter", "Any"), selected = "Any", 
                  inline = TRUE),
      textInput("text", label = ('Start typing'), value = '')),
    

    mainPanel(
      
      wellPanel(
        
        textOutput("chosentype"),
        tags$hr(),
        textOutput("text"),
        tags$ul(id="guesslist",
          tags$li(textOutput("guess1")),
          tags$li(textOutput("guess2")),
          tags$li(textOutput("guess3")),
          tags$li(textOutput("guess4")),
          tags$li(textOutput("guess5"))
      )
    )
  )
)
)
)