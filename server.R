library(shiny)
library(ggplot2)

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  diceRollsPerGame <- reactive({
    
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
    
    # mean histogram
    a <- ggplot(dfMeans) + aes(x=meanVector) + geom_histogram(binwidth=0.1) + 
        theme_bw()

    dfDiceRolls <- as.data.frame(diceRolls)
    
    # dice roll histogram
    plot <- ggplot(dfDiceRolls) + aes(x=diceRolls) + geom_histogram(binwidth=1) + 
        scale_x_continuous(limits=c(2,12), breaks=c(2,3,4,5,6,7,8,9,10,12,12)) +
      theme_bw()
    
    # return both plots
    multiplot(a, plot, cols=1)
    
  })
  
})
