MeanRevertCorrelatePairwise <- function(StockMarketDataset, corrThreshold=0.95) {
	
	# Convert StockMarketDataset into format for computing correlation
	tickerlist = unique(StockMarketDataset["TICKER"])
	pricesFrame = data.frame(row.names=timelist)
	
	for(i in 1:nrow(tickerlist)) {
		
		tickersymbol = toString(tickerlist[i,"TICKER"])
		prices = data.frame(StockMarketDataset[StockMarketDataset$TICKER==tickersymbol, "PRICE"], row.names=StockMarketDataset[StockMarketDataset$TICKER==tickersymbol, "ACTIVITY_TS"])
		
		if(i == 1) {
			pricesFrame = prices
		} else {
			pricesFrame = cbind(pricesFrame, prices)
		}
		names(pricesFrame)[i] = tickersymbol
	}
	pricesFrame = pricesFrame[order(row.names(pricesFrame)),]
	
  	# Compute correlation (NxN matrix)
  	corrFrame = cor(pricesFrame, method="pearson")
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
	        	diff = pricesFrame[,rowTicker] - pricesFrame[,colTicker]
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

	pairCorrelationInfo = pairFrame[order(as.matrix(pairFrame$ABSCORR), decreasing=TRUE),]

  	# return (sorted by highest correlation values)
	pairCorrelationInfo
}


# Plot the highest correlation pair
VisualizeCorrelation <- function(pairCorrelationInfo, StockMarketDataset, topPosition) {

	ticker1 = toString(pairCorrelationInfo[topPosition, "TICKER1"])
	ticker2 = toString(pairCorrelationInfo[topPosition, "TICKER2"])
	ypriceTicker1 = StockMarketDataset[StockMarketDataset$TICKER==ticker1,"PRICE"]
	ypriceTicker2 = StockMarketDataset[StockMarketDataset$TICKER==ticker2,"PRICE"]
	xtime = unique(StockMarketDataset["ACTIVITY_TS"])
	
	par(mfrow=c(2,1))
	plot(xtime, ypriceTicker1, col="blue", xlab="time", ylab="Price")
	title(ticker1)
	plot(xtime, ypriceTicker2, col="green", xlab="time", ylab="Price")
	title(ticker2)
}

DetermineOrders_MeanRevert <- function(portfolioId, PortfoliosInfo, StockMarketDataset) {

	pairCorrelationInfo <- MeanRevertCorrelatePairwise(StockMarketDataset, corrThreshold=0.95)
	
	# pick the top ten
	numPairs <- 10
	
	# TODO:
	# -- need way to determine how much to invest in each bet
	#  -> say pick top ten, allocate 10% of initial cash to each 'pair'
	# -- need way to compute 'delta' for each pair
	# -- need way to determine how to 'short sell'
	# -- need to track state transitions of a pair (there are 3 possible states: Neutral, BuyX-SellY, BuyY-SellX)
	
	timeline <- unique(StockMarketDataset$ACTIVITY_TS)
	numtimeunits <- length(timeline)
	
	for(i in 1:numtimeunits) {
		timestamp <- as.character(timeline[i])
		
		for(p in 1:numPairs) {
			ticker1 <- as.character(pairCorrelationInfo[p, "TICKER1"])
			ticker2 <- as.character(pairCorrelationInfo[p, "TICKER2"])
			priceTicker1 <- StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp & StockMarketDataset$TICKER==ticker1,"PRICE"]
			priceTicker2 <- StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp & StockMarketDataset$TICKER==ticker2,"PRICE"]	
			meanDiff <- as.double(as.matrix(pairCorrelationInfo[p, "MEANDIFF"]))
			sdDiff <- as.double(as.matrix(pairCorrelationInfo[p, "SDEVDIFF"]))
			
			currPriceDiff <- priceTicker1 - priceTicker2
			
			# Detect Sell1-Buy2
			if(currPriceDiff > (meanDiff + (2*sdDiff))) {
				# Sell1-Buy2
				print(paste("[", timestamp, "] ", ticker1, "-", ticker2, ": 1"))
				
			} else if (currPriceDiff < (meanDiff - (2*sdDiff))) {
				# Buy1-Sell2
				print(paste("[", timestamp, "] ", ticker1, "-", ticker2, ": 2"))
			}
		}
	}
	
	# RETURN (order book)

}
