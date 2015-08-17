library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Two Dice Roll Distribution Viewer"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("numberOfGames",
                  "Number of Games:",
                  min = 6,
                  max = 1000,
                  value = 50),
      sliderInput("diceRollsPerGame",
                  "Dice roll per game",
                  min = 5,
                  max = 100,
                  value = 50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("summary1"),
      plotOutput("diceRollMean"),
      verbatimTextOutput("summary2")
    )
  )
))