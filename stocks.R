
# !!!
# NOTE: you need to install the tseries package to use any of this
# !!!

# ----------------------------------------------------------------------
# "df.get.hist.quote()" function
#
# Based on code by A. Trapletti (package tseries)
#
# The main difference is that this function produces a data frame with
# a column containing the dates of the quotes, instead of a time series
# object.
df.get.hist.quote <- function (instrument = "ibm",
                               start, end,
                               quote = c("Open","High", "Low", "Close","Volume"),
                               provider = c("yahoo", "google"), method = "auto",compression = "d") 
{
    if (missing(start)) 
        start <- "1970-01-02"
    if (missing(end)) 
        end <- format(Sys.time(), "%Y-%m-%d")
    provider <- match.arg(provider)
    start <- as.POSIXct(start, tz = "GMT")
    end <- as.POSIXct(end, tz = "GMT")
    if (provider == "yahoo") {
        url <- paste("http://chart.yahoo.com/table.csv?s=", instrument, 
            format(start, paste("&a=", as.character(as.numeric(format(start, 
                "%m")) - 1), "&b=%d&c=%Y", sep = "")), format(end, 
                paste("&d=", as.character(as.numeric(format(end, 
                  "%m")) - 1), "&e=%d&f=%Y", sep = "")), "&g=", 
            compression, "&q=q&y=0&z=", instrument, "&x=.csv", 
            sep = "")
        destfile <- tempfile()
        status <- download.file(url, destfile, method = method)
        if (status != 0) {
            unlink(destfile)
            stop(paste("download error, status", status))
        }
        status <- scan(destfile, "", n = 1, sep = "\n", quiet = TRUE)
        if (substring(status, 1, 2) == "No") {
            unlink(destfile)
            stop(paste("No data available for", instrument))
        }
        x <- read.table(destfile, header = TRUE, sep = ",")
        unlink(destfile)
        nser <- pmatch(quote, names(x))
        if (any(is.na(nser))) 
            stop("This quote is not available")
        n <- nrow(x)
        lct <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME", "C")
        on.exit(Sys.setlocale("LC_TIME", lct))
        dat <- gsub(" ", "0", as.character(x[, 1]))
        dat <- as.POSIXct(strptime(dat, "%Y-%m-%d"), tz = "GMT")
        if (dat[n] != start) 
            cat(format(dat[n], "time series starts %Y-%m-%d+\n"))
        if (dat[1] != end) 
            cat(format(dat[1], "time series ends   %Y-%m-%d\n"))
#        x[,1] <- format(dat,"%Y-%m-%d")
        return(data.frame(cbind(Date=I(format(dat[n:1],"%Y-%m-%d")),x[n:1,nser]),row.names=1:n))
      }
    else if (provider == "google") {
    	url <- paste("http://finance.google.com/finance/historical?q=", instrument, 
            format(start, "&startdate=%b+%d,+%g"), format(end, 
                "&enddate=%b+%d,+%g"), "&output=csv",  
            sep = "")
        destfile <- tempfile()
        status <- download.file(url, destfile, method = method)
        if (status != 0) {
            unlink(destfile)
            stop(paste("download error, status", status))
        }
        status <- scan(destfile, "", n = 1, sep = "\n", quiet = TRUE)
        if (substring(status, 1, 2) == "No") {
            unlink(destfile)
            stop(paste("No data available for", instrument))
        }
        x <- read.table(destfile, header = TRUE, sep = ",")
        unlink(destfile)
        nser <- pmatch(quote, names(x))
        if (any(is.na(nser))) 
            stop("This quote is not available")
        n <- nrow(x)
        lct <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME", "C")
        on.exit(Sys.setlocale("LC_TIME", lct))
        dat <- gsub(" ", "0", as.character(x[, 1]))
        dat <- as.POSIXct(strptime(dat, "%d-%b-%g"), tz = "GMT")
        if (dat[n] != start) 
            cat(format(dat[n], "time series starts %Y-%m-%d+\n"))
        if (dat[1] != end) 
            cat(format(dat[1], "time series ends   %Y-%m-%d\n"))
#        x[,1] <- format(dat,"%Y-%m-%d")
        return(data.frame(cbind(Date=I(format(dat[n:1],"%Y-%m-%d")),x[n:1,nser]),row.names=1:n))
    } else stop("provider not implemented")
}

ma <- function(x,lag) {
  c(rep(NA,lag),apply(embed(x,lag+1),1,mean))
}

ema <- function (x, beta = 0.1, init = x[1]) 
{
  filter(beta*x, filter=1-beta, method="recursive", init=init) 
} 

ma.indicator <- function(x,ma.lag=20) {
  d <- diff(sign(x-c(rep(NA,ma.lag-1),apply(embed(x,ma.lag),1,mean))))
  factor(c(rep(0,ma.lag),d[!is.na(d)]),levels=c(-2,0,2),labels=c('sell','hold','buy'))
}

macd <- function(x,long=26,short=12) {
  ema(x,beta=1/(long+1))-ema(x,beta=1/(short+1))
}

macd.indicator <- function(x,long=26,short=12,signal=9) {
  v <- macd(x,long,short)
  d <- diff(sign(v-ema(v,beta=1/(signal+1))))
  factor(c(0,0,d[-1]),levels=c(-2,0,2),labels=c('sell','hold','buy'))
}

moving.function <- function(x, lag, FUN, ...) {
  FUN <- match.fun(FUN)
  c(rep(NA,lag),apply(embed(x,lag+1),1,FUN,...))
}
rsi.aux <- function(diffs,lag) {
  u <- length(which(diffs > 0))/lag
  d <- length(which(diffs < 0))/lag
  ifelse(d==0,100,100-(100/(1 + u/d)))
}
rsi <- function(x,lag=20) {
  d <- c(0,diff(x))
  moving.function(d,lag,rsi.aux,lag)
}


rsi.indicator <- function(x,lag=20) {
  r <- rsi(x,lag)
  d <- diff(ifelse(r > 70,3,ifelse(r<30,2,1)))
  f <- cut(c(rep(0,lag),d[!is.na(d)]),breaks=c(-3,-2,-1,10),labels=c('sell','buy','hold'),right=T)
  factor(f,levels=c('sell','hold','buy'))
}

ad.line <- function(df) {
  df$Volume*((df$Close-df$Low) - (df$High-df$Close))/(df$High-df$Low)
}

# If the closing price is closer to the low, then it's likely a "sell" signal will be emitted
# If the closing price is closer to the high, then it's likely a "buy" signal will be generated.
# The Chaikin oscillator takes the moving average of these metrics over a long term (10 days, and a short term (3 days), and compares them)
chaikin.oscillator <- function(df,short=3,long=10) {
  ad <- ad.line(df)
  ema(ad,beta=1/(short+1))-ema(ad,beta=1/(long+1))
}

# Emits "buy" when the ema crosses the price from above, emits "sell" when the ema crosses the price from below, hold otherwise.
# Uses a 30 day EMA by default - try a 100 day EMA too.
rdub.indicator <- function(x, days=30) {
	e <- ema(x, beta=2/(days + 1))
		
	d <- x - e
	d <- sign(d)
	d <- diff(d,1,1)
	factor(c(0,0,d[-1]),levels=c(-2,0,2),labels=c('sell','hold','buy'))

}

# This indicator uses the entire market history (not just closing price) to indicate a buy or sell order.
special.indicator <- function(x) {
	c <- chaikin.oscillator(x)
	c <- diff(c)
	d <- sign(c)
	d <- diff(d)
	
		factor(c(0,0,d[-1]),levels=c(-2,0,2),labels=c('sell','hold','buy'))

}

# ----------------------------------------------------------------------
# The artificial trader function
#
trader.eval <- function(market,signals,
                   bet=0.2, exp.prof=0.03, hold.time=10,
                   trans.cost=5, init.cap=10000, init.stocks=0) {
  N.days <- nrow(market)
  res <- list()
  res$trading <- data.frame(Date=market$Date,
                            Close=market$Close,
                            Money=c(init.cap,rep(0.0,N.days-1)),
                            N.Stocks=c(init.stocks,rep(0.0,N.days-1)),
                            Equity=c(init.stocks*market$Close[1] +init.cap,rep(0.0,N.days-1))
                            )
  
  # The number of trades during the period
  res$N.trades <- length(which(signals=='buy'))
  res$Max.P <- res$Max.L <- res$N.obj <- res$Perc.profitable <- res$N.profit <- 0
  sum.p <- sum.l <- sum.pl <- ssum.pl  <- 0

  for(d in 1:(N.days-1)) {
    res$trading[d+1,'Money'] <- res$trading[d+1,'Money']+res$trading[d,'Money']
    res$trading[d+1,'N.Stocks'] <- res$trading[d+1,'N.Stocks']+res$trading[d,'N.Stocks']
    
    # Signal to post a buy order at the end of the day...(but only if enough money!)
    if (signals[d]=='buy' &
        (N.stocks <- as.integer(bet*res$trading[d,'Equity'] / market[d+1,'Open'])) > 0) {
      
      res$trading[d+1,'N.Stocks'] <- res$trading[d+1,'N.Stocks'] + N.stocks
      res$trading[d+1,'Money'] <- res$trading[d+1,'Money'] -
        N.stocks*market[d+1,'Open'] - trans.cost

      # The maximum reached every day of the holding period
      DailyMax.Hold <- as.vector(market[(d+1):min(d+hold.time,N.days),'High'])
      
      # Did we reach the wanted profit within the holding period ?
      Reach.ExpProf <- market[d,'Close']*(1+exp.prof) < DailyMax.Hold
      if (any(Reach.ExpProf)) {
        # Yes, we did :-). Sell, at wanted profit
        sell.price <- market[d,'Close']*(1+exp.prof)
        sell.day <- d+(which(Reach.ExpProf)[1])
        res$N.obj <- res$N.obj + 1
      } else {
        # No, we did not :-(. Sell, at the close of end of holding
        sell.price <- market[min(d+hold.time,N.days),'Close']
        sell.day <- min(d+hold.time,N.days)
      }
      
      # The result of the trade...
      trade.res <- 100*((N.stocks*sell.price-2*trans.cost)/(market[d+1,'Open']*N.stocks)-1)
      if (trade.res > 0) {
        res$N.profit <- res$N.profit + 1
        if (trade.res > res$Max.P) res$Max.P <- trade.res
        sum.p <- sum.p+trade.res
      } else {
        if (abs(trade.res) > res$Max.L) res$Max.L <- abs(trade.res)
        sum.l <- sum.l + abs(trade.res)
      }
      sum.pl <- sum.pl + trade.res

      # Update the money available in the selling day
      res$trading[sell.day,'Money'] <- res$trading[sell.day,'Money'] + sell.price * N.stocks - trans.cost
      # Update the number of stocks available in the selling day
      res$trading[sell.day,'N.Stocks'] <- res$trading[sell.day,'N.Stocks'] - N.stocks
        
    }

    # The capital available at the end of the next day...
    res$trading[d+1,'Equity'] <- res$trading[d+1,'Money'] +
      res$trading[d+1,'N.Stocks']*market[d+1,'Close']

  }
  
  # Percentage of profitable trades
  res$Perc.profitable <- 100*res$N.profit/res$N.trades
  
  # The profit/loss for the trading period
  res$PL <- res$trading[N.days,'Equity']-init.cap
  
  # The maximum drawdown (sucessive losses) experienced
  res$Max.DrawDown <- max(cummax(res$trading[,'Equity'])-res$trading[,'Equity'])
  
  # The average Profit/Loss per profitable/unprofitable trade
  res$Avg.profit <- sum.p/res$N.profit
  res$Avg.loss <- sum.l/(res$N.trades-res$N.profit)
  res$Avg.PL <- sum.pl/res$N.trades
  
  # The Sharpe ratio
  res$Sharpe.Ratio <- mean(h.returns(res$trading[,'Equity']))/sd(h.returns(res$trading[,'Equity']))

  res
}

# This is a better trader-model function
# It attempts to take taxes into account (TODO: Fix for short and long term holdings)
# I say it's better because it doesn't do any silly holding-period crap, any target profit crap, and it will attempt to sell the oldest shares first (TODO: Make it sell the least valuable shares first (least valuable meaning highest cost-basis)).
# It returns a data frame which you can plot if you desire.
# market: output from df.get.hist.quote
# signals: signals from data (try rdub.indicator(aapl$Close) or special.indicator(aapl))
# bet: ratio of capital to spend on "buy" signals
# sell.ratio: (1/f) fraction of shares to sell on "sell" signals
# init.cap: initial cash to model with
# init.stocks: initial stocks you have when the model starts
# hold.off: number of days after a "buy" signal before we'll honor a sell signal
trader2.eval <- function(market,signals,
                   bet=0.2, sell.ratio=3,
                   trans.cost=5, init.cap=10000, init.stocks=0, hold.off=0, tax.rate=0.35) {
  N.days <- nrow(market)
  res <- list()
  res$trading <- data.frame(Date=market$Date,
                            Close=market$Close,
                            Money=c(init.cap,rep(0.0,N.days-1)),
                            N.Stocks=c(init.stocks,rep(0.0,N.days-1)),
                            Equity=c(init.stocks*market$Close[1] +init.cap,rep(0.0,N.days-1))
                            )
	# The number of trades during the period
  
  res$N.trades <- length(which(signals=='sell'))
  
  res$Max.P <- res$Max.L <- res$N.obj <- res$Perc.profitable <- res$N.profit <-  res$taxes <- 0
  sum.p <- sum.l <- sum.pl <- ssum.pl  <- 0
  last.buy <- 0.0
  
  buys <- data.frame(Date=market$Date,
  					Basis=market$Open,
  					Bought=c(rep(0.0,N.days))
  					)

  for(d in 1:(N.days-1)) {
    res$trading[d+1,'Money'] <- res$trading[d+1,'Money']+res$trading[d,'Money']
    res$trading[d+1,'N.Stocks'] <- res$trading[d+1,'N.Stocks']+res$trading[d,'N.Stocks']
    
    # Signal to post a buy order at the end of the day...(but only if enough money!)
    if (signals[d]=='buy' &
        (N.stocks <- as.integer(bet*res$trading[d,'Money'] / market[d+1,'Open'])) > 0) {
      
      res$trading[d+1,'N.Stocks'] <- res$trading[d+1,'N.Stocks'] + N.stocks
      res$trading[d+1,'Money'] <- res$trading[d+1,'Money'] -
        N.stocks*market[d+1,'Open'] - trans.cost
      
      	# Record the purchase
      	buys$Bought[d+1] <- N.stocks
      	
      	cat(
      		paste(as.character(market[d+1, 'Date']),
      			"- Bought", as.character(N.stocks), "@",
      			as.character(market[d+1,'Open']), "=",
      			as.character(N.stocks*market[d+1,'Open']),"\n"
      		)
      	)
        
        if(!last.buy) {
          last.buy <- d+1
        }
	}
	
	# Signal to sell (but only if enough stocks to sell)
	if(signals[d]=='sell' & res$trading[d, 'N.Stocks'] > 0) {
		if(d - last.buy > hold.off) {
		
		  # sell 1/3rd of stock holdings
		  N.stocks <- res$trading[d, 'N.Stocks'] / sell.ratio
		
		  res$trading[d+1,'N.Stocks'] <- res$trading[d+1,'N.Stocks'] - N.stocks
		  res$trading[d+1,'Money'] <- res$trading[d+1,'Money'] + N.stocks*market[d,'Close'] - trans.cost
		  
		  #profit?
		  sold <- 0.0
		  profit <- 0.0
		  tmp.d <- last.buy
		  while(sold < N.stocks & tmp.d < N.days) {
		  	
		  	if(length(buys$Bought[tmp.d]) == 0) {
		  		# assume cost basis is today's price - no profit
		  		sold <- N.stocks
		  		cat(
		  	  	paste(as.character(market[d, 'Date']),
		  	  		"- Sold", as.character(buys$Bought[tmp.d]),
		  	  		" shares (cost basis: unknown) @ ",
		  	  		as.character(market[d,'Close']), " = ",
		  	  		as.character(N.stocks*market[d,'Close']),"\n"))
		  	} else if(buys$Bought[tmp.d] > 0.0) {
		  	  sold <- sold + buys$Bought[tmp.d]
		  	  
		  	  cat(
		  	  	paste(as.character(market[d, 'Date']),
		  	  		"- Sold", as.character(buys$Bought[tmp.d]),
		  	  		" shares (cost basis: ",
		  	  		as.character(buys$Basis[tmp.d]), ") @ ",
		  	  		as.character(market[d,'Close']), " = ",
		  	  		as.character(buys$Bought[tmp.d]*market[d,'Close']),"\n"
		  	  	)
		  	  )
		  	  
		  	  buys$Bought[tmp.d] <- 0.0

		  	
		  	  # profit (is today's sell price/buy.price * number of shares)
		  	  profit <- profit + N.stocks*(market[d,'Close']/buys$Basis[tmp.d])
		  	  last.buy <- tmp.d + 1
		  	}
		  	
		  	tmp.d <- tmp.d + 1
		  }
		  
		  if(profit > 0.0) {
		    # Take out tax
		    tax.value = profit * tax.rate
		    res$taxes <- res$taxes + tax.value
		    res$trading[d+1,'Money'] <- res$trading[d+1,'Money'] - tax.value
		  }
				
	      # The result of the trade...
          trade.res <- 100*((N.stocks*market[d,'Close']-2*trans.cost)/(market[d+1,'Open']*N.stocks)-1)
          if (trade.res > 0) {
            res$N.profit <- res$N.profit + 1
            if (trade.res > res$Max.P) res$Max.P <- trade.res
            sum.p <- sum.p+trade.res
          } else {
            if (abs(trade.res) > res$Max.L) res$Max.L <- abs(trade.res)
            sum.l <- sum.l + abs(trade.res)
          }
          sum.pl <- sum.pl + trade.res
       }
	}
    
    # The capital available at the end of the next day...
    res$trading[d+1,'Equity'] <- res$trading[d+1,'Money'] +
      res$trading[d+1,'N.Stocks']*market[d+1,'Close']
    
  }
  
  # The average Profit/Loss per profitable/unprofitable trade
  res$Avg.profit <- sum.p/res$N.profit
  res$Avg.loss <- sum.l/(res$N.trades-res$N.profit)
  res$Avg.PL <- sum.pl/res$N.trades

	res
}

h.returns <- function(x,h=1) {
  diff(x,lag=h)/x[1:(length(x)-h)]
}

# This method will do 2 main things: 
# 1) Plots the closing price, your number of shares, and your equity (cash value + number of shares * stock price) over time.  Assuming you follow the recommendations of the model to the letter. You may not want to, nor be able to (say, for tax reasons), because I have yet to take them into account.
# 2) Issues today's predictions based on 3 commonly used indicators: Chaikin Oscillator, Exponential Moving Average (MACD), and My own special concoction (rdub.indicator).
# Arguments: 
# x is the stock symbol to model ("palm")
# init.cap is initial capital (aka cash) used to throw at the algorigthm (10000 by default)
# init.stock is the initial number of shares you own at the given start date (0 by default)
# bet is what fraction of your capital you will spend on buying shares when the indicator says "buy" (defaults to 1/4th of your cash)
# sell.ratio is what fraction (1/f) of your shares to sell when the indicator says "sell" (defaults to 3, which will sell 1/3rd of your stock holdings when the "sell" indicator occurs)
# start and end are start and end dates for the model in "Y-M-D" format, they default to 2009-01-01 and today, respectively
predict.do_it <- function(x, init.cap=10000, init.stock=0, bet=0.25, sell.ratio=3, start="2009-01-01", end) {
	if (missing(end)) 
        end <- format(Sys.time(), "%Y-%m-%d")
       
	market <- df.get.hist.quote(x, start=start, end=end)
	
	# Today, we use the chaikon oscillator - it generates A LOT of trades, but keeps you in the black better than most
	# Also try rdub.indicator(market$Close) for less trades
	t <- trader2.eval(market, special.indicator(market), init.cap=init.cap, init.stock=init.stock, bet=bet, sell.ratio=sell.ratio)
	plot(ts(t$trading[,c(2,4,5)]))
	
	cat(paste("Today's predictions: Chaikon Oscillator:", as.character(special.indicator(market)[nrow(market)-1]),", rdub:",
	as.character(rdub.indicator(market$Close)[nrow(market)]),
	", MACD:", as.character(macd.indicator(market$Close)[nrow(market)])))
}

# This method plots a pretty graph of the stock's closing price, 
# a moving average over @ma.days number of days in blue, and 
# an exponential moving average over @ema.days number of days in red.
# The EMA, when it crosses the closing price from below to above, indicates a sell
# signal.  when it crosses from above to below, it indicates a buy.
graph.do_it <- function(x, start, end, ma.days=21, ema.days=15) {
	if(missing(start)) 
		start <- "2008-01-01"
	if(missing(end)) 
    	end <- format(Sys.time(), "%Y-%m-%d")
    	
    	
    d <- df.get.hist.quote(x, start=start, end=end)
    plot(d$Close, type="l")
    lines(ma(d$Close, lag=ma.days), col="blue", lty=2)
    lines(ema(d$Close, beta=2/(ema.days + 1)), col="red", lty=3)
    legend(1.18, y=max(d$Close), c("Close", paste("MA[", as.character(ma.days), "]"), paste("EMA[", as.character(ema.days), "]")), col = c("black", "blue", "red"), lty= c(1,2,3))
}


cat("stocks.R: Predictions and graphs.\n\n")
cat("Try graph.do_it() for a pretty graph of the closing price, and moving averages of a stock ticker.\n")
cat("Try predict.do_it() for a graph of the results of the predictor model for a given ticker.\n")

