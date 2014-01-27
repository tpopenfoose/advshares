require(quantmod)
require(PerformanceAnalytics)
require(RColorBrewer)
require(gplots)
require(ggplot2)
options("getSymbols.warning4.0"=FALSE)

# not active: HDGB, MULT, YPRO, HOLD, GEUR, GGBP, GYEN, GLDE
tickers_short = c("HDGE","HDGI")
tickers_long_short = c("AGLS","QEH")
tickers_multi_asset = c("GTAA","MATH","DBIZ","GIVE","VEGA")
tickers_dom_equity = c("FWDD","TTFS")
tickers_intl_equity = c("AADR","FWDI")
tickers_global_equity = c("ACCU","EPRO")
tickers_income = c("HYLD","MINC","FWDB")
tickers_reference = c("^GSPC","TLT")
tickers_mbcbehf = c("CFT","HYG","LQD","MUNI","PSK","RSP")

tickers <- c(tickers_short,tickers_long_short,tickers_multi_asset,
             tickers_dom_equity,tickers_intl_equity,tickers_global_equity,
             tickers_income,tickers_reference,tickers_mbcbehf)

series.env <- new.env()
start.date <- "2010-07-12"
palette.name <- "Dark2"
global.colorset = tim6equal

getSymbols(tickers, from=start.date, env=series.env)
volume <- Vo(do.call(merge, as.list(series.env)))
series <- Ad(do.call(merge, as.list(series.env)))
colnames(volume) <- sub("^(.*)[.].*", "\\1", colnames(volume))
colnames(series) <- sub("^(.*)[.].*", "\\1", colnames(series))

series.return <- Return.calculate(series)
series.cumulative <- Return.cumulative(series.return)
series.zeroed <- series.return
series.zeroed[is.na(series.zeroed)] <- 0
series.cumret <- cumprod(1+series.zeroed)

# recover an S&P-500 label
tickers[which(tickers=="^GSPC")] <- "SPX"

#series <- series[,tickers]
#volume <- volume[,tickers]
#series.return <- series.return[,tickers]

series.cumret.df <- data.frame(series.cumret)
series.cumret.df$date <- as.Date(rownames(series.cumret.df))

# color set generator
gencolor <- function(n) {
  brewer.pal(n,palette.name)
}

# summary performance plots
p_summary <- function(t,title) {
  charts.PerformanceSummary(series.return[,t],
                            colorset=gencolor(length(t)),
                            methods="StdDev",
                            wealth.index=TRUE,
                            main=title)
}

p_summary(tickers_short,"Short Alternative Strategy")
p_summary(tickers_long_short,"Long/Short Alternative Strategy")
p_summary(tickers_multi_asset,"Multi-Asset Alternative Strategy")
p_summary(tickers_dom_equity,"Domestic Equity Core Strategy")
p_summary(tickers_intl_equity,"International Equity Core Strategy")
p_summary(tickers_global_equity,"Global Equity Core Strategy")
p_summary(tickers_income,"Income Core Strategy")
p_summary(tickers_mbcbehf,"Alternative Portfolio")

# boxplot
chart.Boxplot(series.return,sort.by="mean",colorset=rich.colors(ncol(series.return)))


# risk histograms
p_histogram <- function(t) {
  nonNAindex <- which(!is.na(series.return[,t]))
  chart.Histogram(series.return[nonNAindex,t],
                  main=paste(t,"Risk Measures"),
                  methods=c("add.density","add.rug","add.normal","add.qqplot","add.risk")
                  )

}
sapply(tickers,p_histogram) # one ticker per plot

# risk/return scatter
p_scatter <- function(t,title) {
  chart.RiskReturnScatter(series.return[,t],
                          Rf=0,
                          colorset=gencolor(length(t)),
                          main=title,
                          add.sharpe=c(1,2,3),
                          add.boxplots=TRUE
                          )
}
tickers_reference[which(tickers_reference=="^GSPC")] <- "GSPC"
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
p_scatter(c(tickers_short,tickers_reference),"Short Alternative Strategy")
p_scatter(c(tickers_long_short,tickers_reference),"Long/Short Alternative Strategy")
p_scatter(c(tickers_multi_asset,tickers_reference),"Multi-Asset Alternative Strategy")
p_scatter(c(tickers_dom_equity,tickers_reference),"Domestic Equity Core Strategy")
#
p_scatter(c(tickers_intl_equity,tickers_reference),"International Equity Core Strategy")
p_scatter(c(tickers_global_equity,tickers_reference),"Global Equity Core Strategy")
p_scatter(c(tickers_income,tickers_reference),"Income Core Strategy")
p_scatter(tickers_reference,"Reference Indices/Products")
p_scatter(tickers_mbcbehf,"Alternative Portfolio")
par(op)

# rolling performance
p_rolling <- function(t,title) {
  charts.RollingPerformance(series.return[,t],
                           width=30, # days
                           Rf=0,
                           main=title,
                           colorset=gencolor(length(t)),
                           legend.loc="topleft"
                           )
}
p_rolling(c(tickers_short,tickers_reference),"Short Alternative Strategy")
p_rolling(c(tickers_long_short,tickers_reference),"Long/Short Alternative Strategy")
p_rolling(c(tickers_multi_asset,tickers_reference),"Multi-Asset Alternative Strategy")
p_rolling(c(tickers_dom_equity,tickers_reference),"Domestic Equity Core Strategy")
p_rolling(c(tickers_intl_equity,tickers_reference),"International Equity Core Strategy")
p_rolling(c(tickers_global_equity,tickers_reference),"Global Equity Core Strategy")
p_rolling(c(tickers_income,tickers_reference),"Income Core Strategy")
p_rolling(c(tickers_mbcbehf,tickers_reference),"Alternative Portfolio")

# relative performance
p_relative <- function(t,ref,title) {
  chart.RelativePerformance(series.return[,t],
                            series.return[,ref],
                            main=paste(title,"Performance Relative to",ref),
                            colorset=gencolor(length(t)+1),
                            legend.loc="topleft"
  )
}

p_relative(tickers_short,"GSPC","Short Alternative Strategy")
p_relative(tickers_long_short,"GSPC","Long/Short Alternative Strategy")
p_relative(tickers_multi_asset,"GSPC","Multi-Asset Alternative Strategy")
p_relative(tickers_dom_equity,"GSPC","Domestic Equity Core Strategy")
p_relative(tickers_intl_equity,"GSPC","International Equity Core Strategy")
p_relative(tickers_global_equity,"GSPC","Global Equity Core Strategy")
p_relative(tickers_income,"TLT","Income Core Strategy")
p_relative(tickers_mbcbehf,"GSPC","Alternative Portfolio")

# TBD
# table.CAPM(series.return[,tickers_multi_asset],Rb=series.return[,"GSPC"])
# chart.RollingCorrelation
# table.DownsideRisk





#op <- par(no.readonly = TRUE)
#par(mfrow=c(2,1))
#par(op)

