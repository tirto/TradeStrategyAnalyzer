# FUNCTION: GetHistStockPrice
# -INPUT: file name of input stock prices csv file
# -OUTPUT: data frame containing stock prices of companies in columns, over time (rows), sorted by time asc
#
GetHistStockPrice <- function(inputHistStockPrices) {

  df = read.table(inputHistStockPrices, header=TRUE, sep=",", dec=".")

  # Data Cleaning (Need to consider only ones with same number of time samples)
  # - will only consider stocks that have data for full time period
  tickerlist = unique(df["TICKER"])
  timelist = unique(df["TIMESTAMP"])
  ntimeunits = nrow(timelist) 
  pricesFrame = data.frame(row.names=timelist)
  numStocks = 0
  for(i in 1:nrow(tickerlist)) {

    tickersymbol = toString(tickerlist[i,"TICKER"])
    numsamples = length(df[df$TICKER==tickersymbol,"TIMESTAMP"])
  
    if(numsamples == ntimeunits) {
      # a stock we want to use
      prices = data.frame(df[df$TICKER==tickersymbol, "OPEN"], row.names=df[df$TICKER==tickersymbol, "TIMESTAMP"])
    
      if(numStocks == 0) {
        priceFrame = prices
      } else {
        priceFrame = cbind(priceFrame, prices)
      }
      numStocks = numStocks + 1
      names(priceFrame)[numStocks] = tickersymbol
    }
  }
  priceFrame = priceFrame[order(row.names(priceFrame)),]

  # return (sorted by time, opening stock prices of different companies)
  priceFrame
}

MeanRevertCorrelatePairwise <- function(PricesWindow, corrThreshold=0.95) {

  # Compute correlation (NxN matrix)
  corrFrame = cor(PricesWindow, method="pearson")
  # Replace diagonal with zero (some we don't pick the same stock)
  numPairs = 0
  for(i in 2:nrow(corrFrame)) {
    rowTicker = row.names(corrFrame)[i]
    for(j in 1:(i-1)) {
      colTicker = row.names(corrFrame)[j]
      corrValue = corrFrame[i,j]
      absCorrValue = abs(corrValue)
      if(absCorrValue >= corrThreshold) {
        # compute Expected diff
        diff = PricesWindow[,rowTicker] - PricesWindow[,colTicker]
        expDiff = mean(diff)
        sdDiff = sd(diff)
        
        #print(c(rowTicker, colTicker, corrValue, absCorrValue, expDiff, sdDiff))
        pairwise = data.frame(t(c(rowTicker, colTicker, corrValue, absCorrValue, expDiff, sdDiff)))
        names(pairwise)[1] = "TICKER1"
        names(pairwise)[2] = "TICKER2"
        names(pairwise)[3] = "CORR"
        names(pairwise)[4] = "ABSCORR"
        names(pairwise)[5] = "MEANDIFF"
        names(pairwise)[6] = "SDEVDIFF"
        if(numPairs == 0) {
          pairFrame = pairwise
        } else {
          pairFrame = rbind(pairFrame, pairwise)
        }
        numPairs = numPairs + 1
      }
    }
  }

  SortedOrderPairFrame = pairFrame[order(as.matrix(pairFrame$ABSCORR), decreasing=TRUE),]

  # return
  SortedOrderPairFrame  
}


# Plot the highest correlation pair
VisualizeCorrelation <- function(stockPrices, orderPairFrame, topPosition) {
orderPairFrame = SortedOrderPairFrame
stockPrices = HistPricesWindow
topPosition=2

  ticker1 = toString(orderPairFrame[topPosition, "TICKER1"])
  ticker2 = toString(orderPairFrame[topPosition, "TICKER2"])
  ypriceTicker1 = stockPrices[,ticker1]
  ypriceTicker2 = stockPrices[,ticker2]
  xtime = row.names(stockPrices)

  par(mfrow=c(2,1))
  plot(xtime, ypriceTicker1, col="blue", xlab="time", ylab="Price")
  title(ticker1)
  plot(xtime, ypriceTicker2, col="green", xlab="time", ylab="Price")
  title(ticker2)
}

# --------------------- RUN TRAINING -------------------------
HistPricesWindow <- GetHistStockPrice("C:/DATA/ProjectCode/SP500prices.csv")
SortedOrderPairFrame <- MeanRevertCorrelatePairwise(HistPricesWindow)
VisualizeCorrelation(HistPricesWindow, SortedOrderPairFrame, 2)

#------------------------- REAL_TIME SIM PART --------------------
GetStockMarketOverTime <- function(inputStockMarketFile) {
  stockMarket = read.table(inputStockMarketFile, header=TRUE, sep=",", dec=".")
  stockMarket = stockMarket[order(stockMarket$TIMESTAMP),]

  # return (stock info sorted by time)
  stockMarket
}
stockMarketDataset <- GetStockMarketOverTime("C:/DATA/ProjectCode/SP500prices.csv")

PairwiseOfflineCorrelation_Run <- function(stockMarketOverTime) {

  # return data frame(TIMESTAMP, TICKER, NUM_SHARES, ORDER TYPE (BUY,SELL,SHORT SELL), LIMIT)
  orderBook
}


RandomBuyAndHold_Run <- function(portfolioInfo, stockMarketOverTime) {

  numCandidates = 10  

  startBalance = portfolioInfo$BALANCE
  startTimestamp = stockMarketOverTime[1,"TIMESTAMP"]
  lastTimestamp = stockMarketOverTime[nrow(stockMarketOverTime), "TIMESTAMP"]
  buyCandidates = stockMarketOverTime[stockMarketOverTime$TIMESTAMP==startTimestamp,]

  # pick random candidates
  buyCandidates = buyCandidates[sample(1:nrow(buyCandidates), numCandidates, replace=F),]

  # place buy orders

  
}

SimulateBrokerOnPortfolio <- function(portfolioInfo, portfolioOrderBook, stockMarketOverTime) {

  # how to support short sell? allow short sell but with given stop loss limit?
  
  # returned
  
}




