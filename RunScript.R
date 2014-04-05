# ============== Dependencies =================
setwd("C:/DATA/ProjectCode")

source("ManageData.R")
source("StrategySimulator.R")
source("StrategyRandomBuyHold.R")
source("StrategyMeanRevert.R")

# =============================================
StockInfoDataset <- LoadStocksToDB()
StockMarketDataset <- LoadMarketToDB()


# ========== ONE SIMULATION RUN ===============
portfolioId <- 1
algorithmName <- "RANDOM_BUY_HOLD"
startAmount <- 100000

PortfoliosInfo <- AddPortfolio(PortfoliosInfo, portfolioId, algorithmName, startAmount)


# Compute Trade Actions from Strategy
OrderBook <- DetermineOrders_RandomBuyAndHold(portfolioId, PortfoliosInfo, StockMarketDataset)

# Simulate Broker
SimResults <- SimulateBroker(portfolioId, PortfoliosInfo, StockMarketDataset, OrderBook, HoldingsInfo, TransactionInfo)
HoldingsInfo <- SimResults[[1]]
TransactionsInfo <- SimResults[[2]]

# Compute general stats on algorithm performance on portfolio
startPortfolioWorth <- as.double(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId, "STARTAMOUNT"])
lastTimestamp <- as.character(StockMarketDataset[nrow(StockMarketDataset), "ACTIVITY_TS"])
endPortfolioWorth <- GetPortfolioWorthAtTime(lastTimestamp, portfolioId, StockMarketDataset, HoldingsInfo)
overallPortfolioReturn <- 100*(endPortfolioWorth / startPortfolioWorth) - 100
print(paste("Overall Return: ", overallPortfolioReturn, "%"))

# Dump simulation results to database
StorePortfoliosToDB(PortfoliosInfo)
StoreHoldingsToDB(HoldingsInfo)
StoreTransactionsToDB(TransactionsInfo)


