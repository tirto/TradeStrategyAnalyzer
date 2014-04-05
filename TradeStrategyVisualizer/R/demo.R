library(RSQLite)
library(ggplot2)
library(RJSONIO)
library(googleVis)
#library(TradeStrategyVisualizer)

# ============== Dependencies =================
setwd("C:/DATA/ProjectCode/TradeStrategyVisualizer/R")

source("commonUtils.R")
source("dbUtils.R")
source("visUtils.R")
source("gvis2.R")
source("gvisAnnotatedTimeLine2.R")
source("gvisMotionChart2.R")

pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

#
#  TradeVisualiazer demo
#


# initialization
# sqlite database
schemaDbName = "C:/DATA/ProjectCode/TradeStrategyVisualizer/R/sim.db"
schemaStocksTableName = "stocks"
schemaMarketTableName = "market"
schemaPortfoliosTableName = "portfolios"
schemaHoldingsTableName = "holdings"
schemaTransactionsTableName = "transactions"
schemaHoldingsValueTableName = "holdingsvalue"
schemaPairCorrelationInfoTableName = "pairCorrelationInfo"
schemaAnnotationsTableName = "annotations"

# bootstrap data
# read TradeStrategySimulator result from DB, courtesy of Winnie Cheng
stocksData <- readFromDB(schemaStocksTableName)
head(stocksData)
portfoliosData <- readFromDB(schemaPortfoliosTableName)
head(portfoliosData)
marketData <- readFromDB(schemaMarketTableName)
head(marketData)
holdingsData <- readFromDB(schemaHoldingsTableName)
head(holdingsData)
transactionsData <- readFromDB(schemaTransactionsTableName)
head(transactionsData)
pairCorrelationInfoData <- readFromDB(schemaPairCorrelationInfoTableName)
head(pairCorrelationInfoData)
annotationsData <- readFromDB(schemaAnnotationsTableName)
head(annotationsData)
histMarketData <- readFromDB(schemaMarketTableName,where=" where ticker in (select distinct ticker from transactions)")
head(histMarketData)
sumHoldings <- calcSumHoldings(histMarketData,holdingsData)
head(sumHoldings)

#
# visualization
#

# data is from 2009/08/21 - 2010/08/20

## ggplot

# plot holding summary
# with caption
yrng <- range(sumHoldings$x)
xrng <- range(sumHoldings$txdate)
caption <- "Overall Returns:"
for (i in 1:nrow(portfoliosData)) {
   text <- paste(portfoliosData$ALGORITHM[i]," ",round(portfoliosData$PERFORMANCE[i],2),"%",sep="")
   caption <- paste(caption,text,sep="\n")
}
portfolioHoldingSummaryChart(sumHoldings,portfoliosData,xrng,yrng,caption)
pause()

# without caption
portfolioHoldingSummaryChart(sumHoldings,portfoliosData)
pause()


# plot stocks correlation matrix
corrMatrixData <- pairCorrelationInfoData[1:10,]
head(corrMatrixData)
stockCorrMatrixChart(data=corrMatrixData)
pause()

# plot correlated stocks in pairs
sMarketData <- subset(marketData,TICKER %in% corrMatrixData$TICKER1 | TICKER %in% corrMatrixData$TICKER2)
head(sMarketData)
stockCorrPairsCartesianChart(sMarketData,corrMatrixData)
pause()

# cartesian chart of stocks prices that we own
sPrices = subset(marketData,TICKER %in% transactionsData$TICKER)
head(sPrices)
portfolioStocksChart(data=sPrices)
pause()

# boxplot of all the stocks that we own
portfolioBoxPlot(mData=marketData,tData=transactionsData)
pause()

# individual stock price heat map
# stocks from our db
calendarHeatMap(marketData,ticker="A")
pause()
# or any stocks from web services
calendarHeatMap(marketData,ticker="YHOO")
pause()

#
# experimentations with googleVis
#
# set chart title and description
cTitle <- "<h3>Annotated Time Line Portfolio Summary Chart</h3>"
cDesc <- "<br/>Trade Strategy Simulation Result of Mean Reverting vs. Buy Random and Hold Algorithm"
# set chart options
# ZZZ for unknown reasons,min doesn't seem to work as expected
cOpts <- list(displayAnnotations=TRUE,width=800,height=400,min=50000)

gvisATLData <- merge(sumHoldings,portfoliosData,by.x="id",by.y="PORTFOLIOID")
gvisATLData <- merge(x=gvisATLData,y=annotationsData, by=c("id","txdate"), all.x=TRUE)
gvisATLData$title <- NA
head(gvisATLData)
AnnoTimeLine <- gvisAnnotatedTimeLine2(gvisATLData,
   datevar="txdate",
   numvar="x",
   idvar="ALGORITHM",
   titlevar="title",
   annotationvar="annotation",
   options=cOpts,
   charttitle=cTitle,
   chartdesc=cDesc
   )
plot(AnnoTimeLine)
pause()


# motion chart
gvisMCData <- merge(sumHoldings,portfoliosData,by.x="id",by.y="PORTFOLIOID")
gvisMCData$Year <- as.POSIXlt(gvisMCData$txdate)$year + 1900

head(gvisMCData)
state='{"time":"2009-08-21","yZoomedDataMin":98333,"xZoomedDataMax":1282262400000,"sizeOption":"_UNISIZE","orderedByX":false,"dimensions":{"iconDimensions":["dim0"]},"xZoomedIn":false,"yZoomedDataMax":143280,"yZoomedIn":false,"iconType":"BUBBLE","playDuration":15088.88888888889,"showTrails":true,"xAxisOption":"_TIME","iconKeySettings":[{"key":{"dim0":"RANDOM_BUY_HOLD"},"trailStart":"2009-08-21"},{"key":{"dim0":"MEAN_REVERT"},"trailStart":"2009-08-21"}],"yLambda":1,"xZoomedDataMin":1250812800000,"xLambda":1,"nonSelectedAlpha":0.4,"yAxisOption":"3","duration":{"timeUnit":"D","multiplier":1},"orderedByY":false,"uniColorForNonSelected":false,"colorOption":"2"}'

MCtitle <- '<h3>Trade Strategies Comparison Motion Chart</h3>'
MCdesc <- "<br/>Trade Strategy Simulation Result of Mean Reverting vs. Buy Random and Hold Algorithm"
MotionChart <- gvisMotionChart2(gvisMCData, idvar="ALGORITHM", timevar="txdate",date.format = "/%Y-/%m-/%d",options=list(state=state,width=800,height=450),charttitle=MCtitle,chartdesc=MCdesc)
plot(MotionChart)
pause()


# clean ups
rm(stocksData)
rm(portfoliosData)
rm(marketData)
rm(holdingsData)
rm(transactionsData)
rm(pairCorrelationInfoData)
rm(annotationsData)
rm(histMarketData)
rm(sumHoldings)
rm(corrMatrixData)
rm(sMarketData)
rm(sPrices)
rm(xrng,yrng,caption)
rm(cTitle,cDesc,cOpts,gvisATLData)
rm(state,MCtitle,MCdesc,gvisMCData)
