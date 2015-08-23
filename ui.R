library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Two Dice Roll Distribution Viewer"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      htmlOutput("instruction"),
      sliderInput("numberOfGames",
                  "Number of Games:",
                  min = 10,
                  max = 1000,
                  value = 50),
      sliderInput("diceRollsPerGame",
                  "Dice roll per game",
                  min = 20,
                  max = 100,
                  value = 50),
      htmlOutput("rethrowInfo"),
      actionButton(label="Rethrow", inputId="rethrowDice")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("introduction"),
      plotOutput("diceRollMean"),
      htmlOutput("summary1"),
      plotOutput("diceRollData"),
      htmlOutput("summary2")
    )
  )
))