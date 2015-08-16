library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Backgammon Dice Roll Mean generator"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("numberOfGames",
                  "Number of Games:",
                  min = 25,
                  max = 100,
                  value = 50),
      sliderInput("diceRollsPerGame",
                  "Dice roll per game",
                  min = 25,
                  max = 100,
                  value = 50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("diceRollMean")
    )
  )
))