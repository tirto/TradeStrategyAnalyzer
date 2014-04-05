library(RSQLite)
library(ggplot2)
library(RJSONIO)
library(googleVis)
library(TradeStrategyVisualizer)
#
# Main program
#

# initialization
# sqlite database
schemaDbName = "sim.db"
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
portfoliosData <- readFromDB(schemaPortfoliosTableName)
marketData <- readFromDB(schemaMarketTableName)
holdingsData <- readFromDB(schemaHoldingsTableName)
transactionsData <- readFromDB(schemaTransactionsTableName)
pairCorrelationInfoData <- readFromDB(schemaPairCorrelationInfoTableName)
annotationsData <- readFromDB(schemaAnnotationsTableName)
histMarketData <- readFromDB(schemaMarketTableName,where=" where ticker in (select distinct ticker from transactions)")
sumHoldings <- calcSumHoldings(histMarketData,holdingsData)

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
tsvPlot<-portfolioHoldingSummaryChart(sumHoldings,portfoliosData,xrng,yrng,caption)
stopifnot(nrow(sumHoldings)==nrow(tsvPlot$data))

# without caption
tsvPlot<-portfolioHoldingSummaryChart(sumHoldings,portfoliosData)
stopifnot(nrow(sumHoldings)==nrow(tsvPlot$data))

# plot stocks correlation matrix
corrMatrixData <- pairCorrelationInfoData[1:10,]
corrMatrixNumrows <- nrow(corrMatrixData)
tsvPlot<-stockCorrMatrixChart(data=corrMatrixData)
stopifnot(nrow(corrMatrixData)==nrow(tsvPlot$data))

# plot correlated stocks in pairs
sMarketData <- subset(marketData,TICKER %in% corrMatrixData$TICKER1 | TICKER %in% corrMatrixData$TICKER2)
tsvPlot<-stockCorrPairsCartesianChart(sMarketData,corrMatrixData)
tsvTicker1<-sort(unique(sMarketData$TICKER))
tsvTicker2<-sort(unique(tsvPlot$data$TICKER))
stopifnot(tsvTicker1==tsvTicker2)

# cartesian chart of stocks prices that we own
sPrices = subset(marketData,TICKER %in% transactionsData$TICKER)
tsvPlot<-portfolioStocksChart(data=sPrices)
stopifnot(nrow(sPrices)==nrow(tsvPlot$data))

# boxplot of all the stocks that we own
tsvPlot<-portfolioBoxPlot(mData=marketData,tData=transactionsData)
tsvTicker1<-sort(unique(transactionsData$TICKER))
tsvTicker2<-sort(unique(tsvPlot$data$TICKER))
stopifnot(tsvTicker2 %in% tsvTicker1)


# individual stock price heat map
# stocks from our db
tsvPlot<-calendarHeatMap(marketData,ticker="A")
stopifnot(tsvPlot$data$TICKER=="A")
# or any stocks from web services
tsvPlot<-calendarHeatMap(marketData,ticker="YHOO")
stopifnot(tsvPlot$data$TICKER=="YHOO")

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

stopifnot(S3Class(AnnoTimeLine)[1]=="gvis")

# motion chart
gvisMCData <- merge(sumHoldings,portfoliosData,by.x="id",by.y="PORTFOLIOID")
gvisMCData$Year <- as.POSIXlt(gvisMCData$txdate)$year + 1900

state='{"time":"2009-08-21","yZoomedDataMin":98333,"xZoomedDataMax":1282262400000,"sizeOption":"_UNISIZE","orderedByX":false,"dimensions":{"iconDimensions":["dim0"]},"xZoomedIn":false,"yZoomedDataMax":143280,"yZoomedIn":false,"iconType":"BUBBLE","playDuration":15088.88888888889,"showTrails":true,"xAxisOption":"_TIME","iconKeySettings":[{"key":{"dim0":"RANDOM_BUY_HOLD"},"trailStart":"2009-08-21"},{"key":{"dim0":"MEAN_REVERT"},"trailStart":"2009-08-21"}],"yLambda":1,"xZoomedDataMin":1250812800000,"xLambda":1,"nonSelectedAlpha":0.4,"yAxisOption":"3","duration":{"timeUnit":"D","multiplier":1},"orderedByY":false,"uniColorForNonSelected":false,"colorOption":"2"}'

MCtitle <- '<h3>Trade Strategies Comparison Motion Chart</h3>'
MCdesc <- "<br/>Trade Strategy Simulation Result of Mean Reverting vs. Buy Random and Hold Algorithm"
MotionChart <- gvisMotionChart2(gvisMCData, idvar="ALGORITHM", timevar="txdate",date.format = "/%Y-/%m-/%d",options=list(state=state,width=800,height=450),charttitle=MCtitle,chartdesc=MCdesc)
stopifnot(S3Class(MotionChart)[1]=="gvis")


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
rm(tsvPlot,tsvTicker1,tsvTicker2)
