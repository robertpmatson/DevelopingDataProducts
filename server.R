library(shiny)
library(ggplot2)
library(data.table)

#Function used to add multiple ggplots to the samne call
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

meanGame <- 0

meanAllDice <- 0

sdGame <- 0

sdAllDice <- 0

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  getGlobals <- reactive({
    data.table("mean"=meanGame, "meanAll"=meanAllDice, "sdGame"=sdGame, "sdAll"=sdAllDice)
  })
  
  # Expression that generates a histogram based on rolling 2 dice. 
  # The expression is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a multiplot
  
  output$diceRollMean <- renderPlot({
    
    diceRollsPerGame <- input$diceRollsPerGame
    meanVector <- vector()
    diceRolls <- vector()
    
    for(n in 1 : input$numberOfGames){
      # get a set of normal distribution randoms
      samples <- sample(2:12, size = diceRollsPerGame ,replace = TRUE , prob = table(outer(1:6,1:6,"+")) / 36)
      
      # get the mean of each sample
      sM <- mean(samples)
      diceRolls <- c(diceRolls, samples)
      meanVector <- c(meanVector, sM)  
    }
    
    dfMeans <- as.data.frame(meanVector)
    
    breakMean = c(seq(min(meanVector) - 0.1, max(meanVector) + 0.1, by=0.1))
    meanGame <<- mean(meanVector)     
    sdGame <<- sd(meanVector)
    
    # mean histogram
    a <- ggplot(dfMeans) + aes(x=meanVector) + 
        geom_histogram(binwidth=0.1) + 
        theme_bw() + 
        scale_x_continuous(breaks=breakMean)

    dfDiceRolls <- as.data.frame(diceRolls)
    
    meanAllDice <<- mean(diceRolls)
    sdAllDice <<- sd(diceRolls)
    
    lims <- c(2:12)
    
    # dice roll histogram
    plot <- ggplot(dfDiceRolls) + aes(x=diceRolls) + geom_histogram(binwidth=1) + 
        scale_x_continuous(breaks=lims) + 
        theme_bw()
    
    getGlobals()
    # return both plots
    multiplot(a, plot, cols=1)
    
  })
  
  output$summary1 <- reactive({
    data <- getGlobals()
    v1 <- paste("Mean of games", data$mean)
    v2 <- paste("SD of games", data$sdGame) 
    print(paste(v1, v2, sep='<br/>'))
  })
  
  output$summary2 <- reactive({
    data <- getGlobals()
    v3 <- paste("Mean of all throws", data$meanAll)
    v4 <- paste("SD of all throws ", data$sdAll)
    print(paste(v3, v4, sep='<br/>'))
  })
  
})
