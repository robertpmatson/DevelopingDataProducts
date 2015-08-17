library(shiny)
library(ggplot2)
library(data.table)

# function that will be called from a reactive context
getPlotData <- function(numberOfGames, diceRollsPerGame){
  
  meanVector <- vector()
  diceRolls <- vector()
  
  for(n in 1 : numberOfGames){
    # get a set of normal distribution randoms
    samples <- sample(2:12, size = diceRollsPerGame ,replace = TRUE , prob = table(outer(1:6,1:6,"+")) / 36)
    
    # get the mean of each sample
    sM <- mean(samples)
    diceRolls <- c(diceRolls, samples)
    meanVector <- c(meanVector, sM)  
  }
  
  meanGame <<- mean(meanVector)     
  sdGame <<- sd(meanVector)
  meanAllDice <<- mean(diceRolls)
  sdAllDice <<- sd(diceRolls)
  summaryData <- data.table("mean"=meanGame, "meanAll"=meanAllDice, "sdGame"=sdGame, "sdAll"=sdAllDice)
  
  list(popMeans=as.data.frame(meanVector), diceRolls = as.data.frame(diceRolls), sumData = summaryData)
  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  getDataList <- reactive({
    getPlotData(input$numberOfGames, input$diceRollsPerGame)
  })

  output$diceRollMean <- renderPlot({
    
    dataList <- getDataList()
    dfMeans <- dataList[[1]]
    breakMean = c(seq(min(dfMeans) - 0.1, max(dfMeans) + 0.1, by=0.1))
    
    # mean histogram
    ggplot(dfMeans) + aes(x=meanVector) + 
        geom_histogram(binwidth=0.1) + 
        theme_bw() + 
        scale_x_continuous(breaks=breakMean)
   })
   
   output$diceRollData <- renderPlot({
     
    dataList <- getDataList()
    dfDiceRolls <- dataList[[2]]
    lims <- c(2:12)
    
    # dice roll histogram
    ggplot(dfDiceRolls) + aes(x=diceRolls) + geom_histogram(binwidth=1) + 
        scale_x_continuous(breaks=lims) + 
        theme_bw()
    
  })
  
  output$summary1 <- reactive({
    dataList <- getDataList()
    data <- dataList[[3]]
    v1 <- paste("Mean of paired dice roll means", data$mean)
    v2 <- paste("SD of paired dice roll mean games", data$sdGame) 
    print(paste(v1, v2, sep='<br/>'))
  })
  
  output$summary2 <- reactive({
    dataList <- getDataList()
    data <- dataList[[3]]
    v3 <- paste("Mean of all paired dice throws", data$meanAll)
    v4 <- paste("SD of all paired dice throws ", data$sdAll)
    print(paste(v3, v4, sep='<br/>'))
  })
  
  output$introduction <- renderText({
    print("When playing games with dice, I am interested in the statistics of getting high or low numbers.")
  })
  
  output$instruction <- renderText({
    print("Use the sliders to increase the number of games and watch how the mean and distribution chnages as you increase or decrease the number of games.<br/><br/>")
  })
  
})
