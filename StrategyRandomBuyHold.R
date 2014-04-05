source("C:/DATA/ProjectCode/StrategySimulator.R")

# FUNCTION: DetermineOrders_RandomBuyAndHold
#   OUTPUT: order book
DetermineOrders_RandomBuyAndHold <- function(portfolioId, PortfoliosInfo, StockMarketDataset) {

  numCandidates = 10
  set.seed(portfolioId)

  portfolioId = portfolioId
  startBalance = as.double(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId, "STARTAMOUNT"])
  startTimestamp = as.character(StockMarketDataset[1,"ACTIVITY_TS"])
  buyCandidates = StockMarketDataset[StockMarketDataset$ACTIVITY_TS==startTimestamp,]

  # pick random candidates
  buyCandidates = buyCandidates[sample(1:nrow(buyCandidates), numCandidates, replace=F),]

  # place buy orders
  numCandidates = nrow(buyCandidates)
  cashCandidate = startBalance / numCandidates

  for(i in 1:numCandidates) {
     tickerCandidate = as.character(buyCandidates[i, "TICKER"])
     priceCandidate = as.double(buyCandidates[i,"PRICE"])
     sharesCandidate = floor(cashCandidate / priceCandidate)
     typeCandidate = "BUY"

     # create order
     orderCandidate = data.frame(t(c(startTimestamp, portfolioId, tickerCandidate, sharesCandidate, typeCandidate, priceCandidate)))
     names(orderCandidate)[1] = "ACTIVITY_TS"
     names(orderCandidate)[2] = "PORTFOLIOID"
     names(orderCandidate)[3] = "TICKER"
     names(orderCandidate)[4] = "NUMSHARES"
     names(orderCandidate)[5] = "ORDERTYPE"
     names(orderCandidate)[6] = "ORDERVALUE"

     if(i == 1) {
        # create order book
        orderBook = orderCandidate
     } else {
        orderBook = rbind(orderBook, orderCandidate)
     }
  }

  orderBook = orderBook[order(orderBook$ACTIVITY_TS),]

  # return
  orderBook
}