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
  summaryData <- data.table("mean"=meanGame, "sdGame"=sdGame)
  
  list(popMeans=as.data.frame(meanVector), diceRolls = as.data.frame(diceRolls), sumData = summaryData)
  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  getDataList <- reactive({
    v$data <- getPlotData(input$numberOfGames, input$diceRollsPerGame)
  })

  observeEvent(input$rethrowDice, {
    v$data <- getPlotData(input$numberOfGames, input$diceRollsPerGame)
  })
  
  output$diceRollMean <- renderPlot({
    
    dataList <- v$data
    dfMeans <- dataList[[1]]
    breakMean = c(seq(round(min(dfMeans) - 0.1,1), round(max(dfMeans) + 0.1,1), by=0.1))
    
    # mean histogram
    ggplot(data=dfMeans, aes(x=meanVector)) + 
        geom_histogram(aes(y = ..density..), binwidth=0.1, colour="black", fill="lightblue") + 
        stat_density(geom = "line", colour="darkblue", na.rm=TRUE) + 
        stat_function(fun=dnorm,  colour="green", args=list(sd=1/sqrt(7), mean=7)) + 
        theme_bw() + 
        scale_x_continuous(breaks=breakMean) +
      xlab("Mean dice pair total from each game") + 
      geom_vline(aes(xintercept=7), color="cyan", linetype="dashed", size=1) +
      geom_vline(aes(xintercept=mean(meanVector)), color="red", linetype="dashed", size=1)
      
   })
   
   output$diceRollData <- renderPlot({
     
    dataList <- v$data
    dfDiceRolls <- dataList[[2]]
    lims <- c(2:12)
    
    # dice roll histogram
    ggplot(data=dfDiceRolls, aes(x=diceRolls)) + 
        geom_histogram(binwidth=1, colour="black", fill="lightblue", origin = 1.5) + 
        scale_x_continuous(breaks=lims) + 
        theme_bw() + xlab("Distribution of the sum of the roll of 2 dice")
  })
  
  output$summary1 <- reactive({
    dataList <- v$data
    sData <- dataList[[3]]
    v1 <- paste("Mean of paired dice roll means", round(sData$mean,3))
    v2 <- paste("Standard Deviation of paired dice roll mean games", round(sData$sdGame,3)) 
    print(paste(v1, v2, sep='<br/>'))
  })
  
  output$summary2 <- reactive({
    getDataList()
    print('<br/>')
  })
  
  output$introduction <- renderText({
    print("When playing games with dice, I am interested in the statistics of getting high or low numbers. The mean of the dice roll is a gaussian distribution 
          and this should form a bell shape distribution curve. As you increase the number of games, notice how the mean converges to the value 7. The mean of a single dice roll is 3.5 so for 2 dice it is 7.
          As you decrease the number of games, the mean stays near 7 but might vary.</br></br>The cyan line shows the expected mean, the red line shows the actual mean and the red line might 
          hide the actual mean line if they are the same value or very close. The green curve shows the normal distribution curve and the blue curve shows the distribution of the sample means.")
  })
  
  output$instruction <- renderText({
    print("Use the sliders to increase the number of games and watch how the mean and distribution changes as you increase or decrease the number of games.<br/><br/>")
  })
  
  output$rethrowInfo <- renderText({
    print("<br/>Click to rethrow the dice.<br/>")
  })
  
})
