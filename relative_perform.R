require(quantmod)
require(PerformanceAnalytics)
options("getSymbols.warning4.0"=FALSE)

# not active: HDGB, MULT, YPRO, HOLD
tickers <- c("^GSPC",
             "HDGE","HDGI",
             "AGLS","QEH",
             "GTAA","MATH","DBIZ","GIVE","VEGA",
             "FWDD","TTFS",
             "AADR","FWDI",
             "ACCU","EPRO",
             "HYLD","MINC","FWDB")
series.env <- new.env()
start.date <- "2010-07-12"
col=tim12equal

getSymbols(tickers, from=start.date, env=series.env)
series <- Ad(do.call(merge, as.list(series.env)))
colnames(series) <- sub("^(.*)[.].*", "\\1", colnames(series))
series.return <- Return.calculate(series)

# put back into tickers order
tickers[1] <- "GSPC" # recovering fetch substitution
series <- series[,tickers]
series.return <- series.return[,tickers]

