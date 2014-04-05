#
# visualization functions
#
stockCorrMatrixChart = function(data) {
  # supress 'R CMD check' no visible binding for global variable NOTES
  TICKER1 <- NULL; rm(TICKER1)
  TICKER2 <- NULL; rm(TICKER2)
  CORR <- NULL; rm(CORR)
  # reorder ticker sorted by correlation
  ## ZZZ adjust reorder use other var? rnum doesn't seem to work. good enough for now
  data$rnum <- as.numeric(row.names(data))
  ggplot(data,aes(x=reorder(TICKER1,CORR),y=reorder(TICKER2,CORR),fill=CORR)) +
      theme_bw() +
      geom_tile() + 
      geom_text(aes(label=round(CORR,3)),colour="white") +
      opts(title = "Stocks Correlation Matrix") +
      opts(plot.title = theme_text(size = 16,colour = "blue",face="bold")) +
      xlab("Ticker1") +
      ylab("Ticker2") 
}

stockCorrPairsCartesianChart = function(sMarketData,corrData) {
  # supress 'R CMD check' no visible binding for global variable NOTES
  ACTIVITY_TS <- NULL; rm(ACTIVITY_TS)
  PRICE <- NULL; rm(PRICE)
  TICKER <- NULL; rm(TICKER)
  print("creating correlated stocks data frame")
  sCorr <- data.frame()
  for (i in 1:nrow(corrData)) {
     ticker1 <- corrData$TICKER1[i]
     ticker2 <- corrData$TICKER2[i]
     sTmp <- subset(sMarketData,TICKER == ticker1 | TICKER == ticker2)
     sTmp$CORRGRP <- paste(ticker1,"-",ticker2,sep="")
     sCorr <- rbind(sCorr,sTmp)
  }
  print("drawing stock prices chart group by top correlated stocks")
  ggplot(sCorr,aes(x=ACTIVITY_TS,y=PRICE,group=TICKER,colour=TICKER)) + 
     theme_bw() +
     geom_line() + 
     facet_grid(CORRGRP ~ .,scale="free_y") + 
     opts(strip.text.y = theme_text()) +
     opts(title = "Correlated Stock Prices Chart") +
     opts(plot.title = theme_text(size = 16,colour = "blue",face="bold")) +
     xlab("Date") +
     ylab("Price") 
}

portfolioStocksChart = function(data) {
  # supress 'R CMD check' no visible binding for global variable NOTES
  ACTIVITY_TS <- NULL; rm(ACTIVITY_TS)
  PRICE <- NULL; rm(PRICE)
  TICKER <- NULL; rm(TICKER)
  print("plotting stock prices chart")
  ggplot(data) +
     theme_bw() +
     # add each line
     geom_line(aes(ACTIVITY_TS,PRICE,colour=TICKER)) +
     geom_smooth(aes(ACTIVITY_TS,PRICE,group=TICKER,colour=TICKER)) +
     geom_smooth(aes(ACTIVITY_TS,PRICE)) +
     opts(title = "Portfolio Stock Prices Chart") +
     opts(plot.title = theme_text(size = 16,colour = "blue",face="bold")) +
     xlab("Date") +
     ylab("Price") 
}

portfolioHoldingSummaryChart <- function(hData,pData,xrng=NULL,yrng=NULL,caption=NULL) {
  # supress 'R CMD check' no visible binding for global variable NOTES
  ALGORITHM  <- NULL; rm(ALGORITHM)
  txdate  <- NULL; rm(txdate)
  x  <- NULL; rm(x)
  # combine data
  data <- merge(hData,pData,by.x="id",by.y="PORTFOLIOID")
  # use our own fit
  bestfit <- geom_smooth(method="lm",se=FALSE,colour=alpha("steelblue",0.5),size=2)
  
  print("plotting portfolio holdings summary")
  p <- ggplot(data,aes(x=txdate,y=x,group=id,colour=factor(ALGORITHM))) +
     theme_bw() +
     geom_line() +
     bestfit +
     opts(title = "Portfolio Holdings") +
     opts(plot.title = theme_text(size = 16,colour = "blue",face="bold")) +
     xlab("Date") +
     ylab("Value in US$") +
     labs(colour="Algorithm")
  if (!is.null(caption) & !is.null(xrng) & !is.null(yrng)) {
    println("adding caption",caption)
    p <- p + geom_text(aes(x=xrng[1],y=yrng[2],label=caption),colour="black",legend=F,size=4,vjust=1,hjust=0)
  }
  p
}     
  
portfolioBoxPlot <- function(mData,tData) {
  # supress 'R CMD check' no visible binding for global variable NOTES
  TICKER <- NULL; rm(TICKER)
  PRICE <- NULL; rm(PRICE)
  drawBoxPlot <- function(s,pId) {
    println("plotting boxplot for portfolioID",pId)
    # reorder from low to high stock prices
    ggplot(s,aes(x=reorder(TICKER,PRICE,mean),y=PRICE),colour = pId) +
       geom_boxplot() +
       opts(title = "Stock Prices BoxPlot") +
       opts(plot.title = theme_text(size = 16,colour = "blue",face = "bold")) +
       xlab("TICKER") +
       ylab("PRICE")
  }
  
  pIds <- unique(tData$PORTFOLIOID)
  iteration <- ifelse(is.null(nrow(pIds)),1,nrow(pIds))
  if (iteration > 1) {
    # draw the boxplot one portfolio at a time
    for (i in 1:iteration) {
      pId = pIds[i]
      s = subset(mData,TICKER %in% tData[tData$PORTFOLIOID==pId,"TICKER"])
      drawBoxPlot(s,pId)
      pause()
    }
  } else {
    pId = pIds[1]
    s = subset(mData,TICKER %in% tData[tData$PORTFOLIOID==pId,"TICKER"])
    drawBoxPlot(s,pId)
  } 
}

# generate calendar heat map for a particular stock for deep dive
# code taken from hadley's blog and was slightly modified to fit my needs
calendarHeatMap <- function(mData,ticker,startdate="2009-08-21",enddate="2010-08-20") {
    # supress 'R CMD check' no visible binding for global variable NOTES
    TICKER <- NULL; rm(TICKER)
    ACTIVITY_TS <- NULL; rm(ACTIVITY_TS)
    PRICE <- NULL; rm(PRICE)
    week <- NULL; rm(week)
    wday <- NULL; rm(wday)
    Date <- NULL; rm(Date)
    Adj.Close <- NULL; rm(Adj.Close)
    # get stock data from marketData
    stock.data <- subset(mData,TICKER == ticker)
    charttitle <- paste("Calendar Heat Map for ",ticker,sep="")
    if (length(stock.data$TICKER)>0) {
       stock.data <- transform(stock.data,
       week = as.POSIXlt(ACTIVITY_TS)$yday %/% 7 + 1,
       wday = as.POSIXlt(ACTIVITY_TS)$wday,
       year = as.POSIXlt(ACTIVITY_TS)$year + 1900)
        p <- ggplot(stock.data,aes(week,wday,fill = PRICE)) 
       
    } else {
       # get stock data from yahoo web services
       quote <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
                ticker,
                "&a=",substr(startdate,6,7),
                "&b=",substr(startdate,9,10),
                "&c=",substr(startdate,1,4),
                "&d=",substr(enddate,6,7),
                "&e=",substr(enddate,9,10),
                "&f=",substr(enddate,1,4),
                "&g=d&ignore=.csv",sep="")
       stock.data <- read.csv(quote,as.is=TRUE)
       stock.data <- transform(stock.data,
          week = as.POSIXlt(Date)$yday %/% 7 + 1,
          wday = as.POSIXlt(Date)$wday,
          year = as.POSIXlt(Date)$year + 1900)
       p <- ggplot(stock.data,aes(week,wday,fill = Adj.Close)) 
    }
    p <- p +
      geom_tile(colour = "white") +
      scale_fill_gradientn(colours = c("#D61818","#FFAE63","#FFFFBD","#B5E384")) +
      facet_wrap(~ year,ncol = 2) +
      opts(title = charttitle) +
      opts(plot.title = theme_text(size = 16,colour = "blue",face = "bold"))
    p  
}
