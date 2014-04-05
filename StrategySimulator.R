# ---- STATE MAINTAINED BY STRATEGY SIMULATOR ------------
#   - like private variables

# Data Frame with General Info about Portfolios
#   PortfolioId, Algorithm, StartAmount
PortfoliosInfo <- data.frame(PortfolioID=integer(0), Algorithm=character(0), StartAmount=double(0))
names(PortfoliosInfo)[1] = "PORTFOLIOID"
names(PortfoliosInfo)[2] = "ALGORITHM"
names(PortfoliosInfo)[3] = "STARTAMOUNT"

# Data Frame with Holdings info
HoldingsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), TICKER=character(0), NUMSHARES=integer(0))
names(HoldingsInfo)[1] = "PORTFOLIOID"
names(HoldingsInfo)[2] = "ACTIVITY_TS"
names(HoldingsInfo)[3] = "TICKER"
names(HoldingsInfo)[4] = "NUMSHARES"

# Data Frame with Transactions info
TransactionsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), DECISION=character(0), TICKER=character(0), NUMSHARES=integer(0))
names(TransactionsInfo)[1] = "PORTFOLIOID"
names(TransactionsInfo)[2] = "ACTIVITY_TS"
names(TransactionsInfo)[3] = "DECISION"
names(TransactionsInfo)[4] = "TICKER"
names(TransactionsInfo)[5] = "NUMSHARES"

# CONSTANTS
cashTicker <- "CASH"


#---------------- REAL_TIME STRATEGY SIM PART --------------------
AddPortfolio <- function(PortfoliosInfo, portfolioId, algorithm, startAmount) {

   entry <- data.frame(portfolioId, algorithm, startAmount)
   names(entry)[1] = "PORTFOLIOID"
   names(entry)[2] = "ALGORITHM"
   names(entry)[3] = "STARTAMOUNT"
   
   PortfoliosInfo = rbind(PortfoliosInfo, entry)
   
   # return
   PortfoliosInfo
}

SimulateBroker <- function(portfolioId, PortfoliosInfo, StockMarketDataset, OrderBook, HoldingsInfo, TransactionInfo){
	
	# Temporary data frame for tracking this portfolio changes to holdings
	CurrentHoldingsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), TICKER=character(0), NUMSHARES=integer(0))
	names(CurrentHoldingsInfo)[1] = "PORTFOLIOID"
	names(CurrentHoldingsInfo)[2] = "ACTIVITY_TS"
	names(CurrentHoldingsInfo)[3] = "TICKER"
	names(CurrentHoldingsInfo)[4] = "NUMSHARES"

	# Transaction Fees
	BuyFee <- 0
	SellFee <- 0
	
	# Initial Asset is all cash
	previousTimestamp = "1900/01/01 00:00:00"
	startBalance <- as.double(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId, "STARTAMOUNT"])
	entry <- data.frame(portfolioId, previousTimestamp, cashTicker, as.integer(startBalance))
	names(entry)[1] = "PORTFOLIOID"
	names(entry)[2] = "ACTIVITY_TS"
	names(entry)[3] = "TICKER"
	names(entry)[4] = "NUMSHARES"
    CurrentHoldingsInfo <- rbind(CurrentHoldingsInfo, entry)
	
	timeline <- unique(StockMarketDataset$ACTIVITY_TS)
	numtimeunits <- length(timeline)
	
	# Move sequentially through each time unit
	for(i in 1:numtimeunits) {
		
		# Determine cash available
		balance <- CurrentHoldingsInfo[CurrentHoldingsInfo$TICKER==cashTicker, "NUMSHARES"]
		
		timestamp <- as.character(timeline[i])
		marketInfo <- StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp, ]
		orderInfo <- OrderBook[OrderBook$ACTIVITY_TS==timestamp, ]
		
		# Process orders for the timeunit
		numOrder = nrow(orderInfo)
		if(numOrder > 0) {
			for(o in 1:numOrder) {
				orderType <- as.character(orderInfo[o, "ORDERTYPE"])
				orderTicker <- as.character(orderInfo[o, "TICKER"])
				orderShares <- as.numeric(as.matrix(orderInfo[o, "NUMSHARES"]))   
				if(orderType == "BUY") {
					# This means "buy immediate"
					immediatePrice <- as.numeric(marketInfo[marketInfo$TICKER==orderTicker,"PRICE"])
					transactionTotal <- BuyFee + (immediatePrice*orderShares)
					afterBalance <- balance - transactionTotal
					if(afterBalance >= 0) {
						# Have sufficient funds to place trade
						
					    # Look up current
						if(nrow(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,]) > 0) {
							currShares = as.integer(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"])
							nextShares = orderShares + currShares
							# update
							CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"] = nextShares
						} else {
							# add new entry
							entry = data.frame(portfolioId, timestamp, orderTicker, orderShares)
							names(entry)[1] = "PORTFOLIOID"
							names(entry)[2] = "ACTIVITY_TS"
							names(entry)[3] = "TICKER"
							names(entry)[4] = "NUMSHARES"
							CurrentHoldingsInfo = rbind(CurrentHoldingsInfo, entry)
						}
						
						# Stock Order Transaction
						entry <- data.frame(portfolioId, timestamp, "BUY", orderTicker, orderShares)
						names(entry)[1] = "PORTFOLIOID"
						names(entry)[2] = "ACTIVITY_TS"
						names(entry)[3] = "DECISION"
						names(entry)[4] = "TICKER"
						names(entry)[5] = "NUMSHARES"
						TransactionsInfo = rbind(TransactionsInfo, entry)
						
						# Deduct cash
						CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==cashTicker, "NUMSHARES"] = afterBalance
						balance <- afterBalance
					} else {
						# DECLINED
						print(paste("ERROR in BUY: [", orderTicker, "] Insufficient cash"))
					}
				} else if (orderType == "SELL") {
					# This means "sell immediate"
		
					# Should already hold stock
					if(nrow(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,]) > 0) {
						currShares = as.integer(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"])
						
						if(currShares >= orderShares) {
							immediatePrice <- as.numeric(marketInfo[marketInfo$TICKER==orderTicker,"PRICE"])
							nextShares <- currShares - orderShares
							afterBalance <- (immediatePrice*orderShares) - SellFee
							
							if(afterBalance > 0) {
								# update numshares
								CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"] = nextShares
								# update cash
								CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==cashTicker, "NUMSHARES"] = afterBalance
								balance <- afterBalance
							} else {
								# DECLINED
								print(paste("ERROR in SELL: [", orderTicker, "] Insufficient cash to pay fee"))
							}
						} else {
							# DECLINED
							print(paste("ERROR in SELL: [", orderTicker, "] ", "Hold ", currShares, ", To Sell ", orderShares))
						}
					} else {
						# DECLINED
						print(paste("ERROR in SELL: [", orderTicker, "] Does not hold any"))
					}
				} else {
					# UNDEFINED ORDER TYPE
				}
			}
		} # numOrder > 0
		
		# Update the timestamp of current holdings
		CurrentHoldingsInfo[, "ACTIVITY_TS"] <- timestamp
		
		# Add it to HoldingsInfo dataset
		HoldingsInfo = rbind(HoldingsInfo, CurrentHoldingsInfo)
		
		# prepare for next iteration
		previousTimestamp <- timestamp
		
	}
	
	# RETURN (HoldingsInfo and TransactionsInfo data frames)
	list(HoldingsInfo, TransactionsInfo)
}

GetPortfolioWorthAtTime <- function(timestamp, portfolioId, StockMarketDataset, HoldingsInfo) {
	
	holdingsAtTime <- HoldingsInfo[HoldingsInfo$ACTIVITY_TS==timestamp,]
	numHoldings <- nrow(holdingsAtTime)
	
	total <- 0
	if(numHoldings > 0) {
		
		for(i in 1:numHoldings) {
			holdingTicker = as.character(holdingsAtTime[i,"TICKER"])
			holdingNumShares = as.numeric(holdingsAtTime[i,"NUMSHARES"])
			
			if(holdingTicker == "CASH") {
				total <- total + holdingNumShares	
			} else {
				holdingPrice = StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp & StockMarketDataset$TICKER==holdingTicker,"PRICE"]
				total <- total + (holdingNumShares * holdingPrice)
			}
		}
	}
	
	# RETURN: total amount
	total
	
}