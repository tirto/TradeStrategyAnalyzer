.onLoad <- function(libname, pkgname) {
  .libPaths("~tadji/RLibrary")
  require(DBI)
  require(RSQLite)
  require(stats)
  require(RJSONIO)
  require(googleVis)
  require(ggplot2)
}

require(DBI)
#
# helper functions
#
# for debugging
println <- function(text, obj) {
   debugMode <- TRUE
   if(debugMode) {
      print(paste(text, '=', obj, sep=""))
   }
}

# allow plot to show up
pause <- function(){  
   invisible(readline("\nPress <return> to continue: ")) 
}

# build a hash lookup to find stock price given a ticker and txdate
getStockPrices <-  function(m) {
   prices <- list()
   for (i in 1:nrow(m)) {
      key <- paste(m$ACTIVITY_TS[i],m$TICKER[i],"")
      prices[[key]] <- m$PRICE[i]
   }
   prices
}


# calculate summary of portfolio holdings
calcSumHoldings  <- function(histMarketData,holdingsData) {
   print("Calculating holding summary. Please wait.....")
   # inner function to calculate the values of holdings for all portfolios
   calcHoldingVal <- function(x, y) {
      holdingVal <- data.frame()
      for (i in 1:nrow(x)) {
         value <- 0
         id <- x$PORTFOLIOID[i]
         txdate <- x$ACTIVITY_TS[i]
         ticker <- x$TICKER[i]
         nshares <- x$NUMSHARES[i]
         dateticker <- paste(txdate,ticker,"")
         price <- y[[dateticker]]
         if (ticker=="CASH") {
            value <- nshares
         }
         else {
            value <- nshares * price
         }
         holdingVal  <- rbind(holdingVal, data.frame(id=id,txdate=txdate,ticker=ticker,value=value))
      }
      holdingVal
   }  
   # build stock prices from historical data
   stockPrices <- getStockPrices(histMarketData)
   # calculate our holdings
   v <- calcHoldingVal(holdingsData,stockPrices)
   sumHoldings <- aggregate(v$value,by=list(id=v$id,txdate=v$txdate),FUN=sum)

   # make dataframe to conform with gvisAnnotatedTimeLine input
   sumHoldings$txdate <- as.Date(sumHoldings$txdate,"%Y-%m-%d")
   writeToDb(tablename="sumHoldings",df=sumHoldings)
   sumHoldings
}
