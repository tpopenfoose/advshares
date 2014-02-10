require(quantmod)
require(PerformanceAnalytics)
require(RColorBrewer)
require(gplots)
require(ggplot2)
options("getSymbols.warning4.0"=FALSE)

# set up a color list generation function dependent on number of items to plot
# qualitative color schemes by Paul Tol
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
colorlist <- list(tol1qualitative,tol2qualitative,tol3qualitative,tol4qualitative,tol5qualitative,tol6qualitative,
                  tol7qualitative,tol8qualitative,tol9qualitative,tol10qualitative,tol11qualitative,tol12qualitative)

# color set generator
gencolor <- function(n) {
  # brewer.pal(ifelse(n<3,3,n),palette.name)
  n <- min(length(colorlist),n)
  colorlist[[n]]
}

# modify a PA chart to use correlation instead of bar VaR
charts.myPerformanceSummary <- function (R, Rb, Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
          width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
          gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
          p = 0.95, ...) 
{
  begin = begin[1]
  x = checkData(R)
  colnames = colnames(x)
  ncols = ncol(x)
  length.column.one = length(x[, 1])
  start.row = 1
  start.index = 0
  while (is.na(x[start.row, 1])) {
    start.row = start.row + 1
  }
  x = x[start.row:length.column.one, ]
  if (ncols > 1) 
    legend.loc = legend.loc
  else legend.loc = NULL
  if (is.null(main)) 
    main = paste(colnames[1], "Performance", sep = " ")
  if (ylog) 
    wealth.index = TRUE
  op <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1.3), widths = 1)
  par(mar = c(1, 4, 4, 2))
  chart.CumReturns(x, main = main, xaxis = FALSE, legend.loc = legend.loc, 
                   event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, 
                   begin = begin, geometric = geometric, ylab = "Cumulative Return", 
                   ...)
  par(mar = c(1, 4, 0, 2))
  freq = periodicity(x)
  switch(freq$scale, seconds = {
    date.label = "Second"
  }, minute = {
    date.label = "Minute"
  }, hourly = {
    date.label = "Hourly"
  }, daily = {
    date.label = "Daily"
  }, weekly = {
    date.label = "Weekly"
  }, monthly = {
    date.label = "Monthly"
  }, quarterly = {
    date.label = "Quarterly"
  }, yearly = {
    date.label = "Annual"
  })
  #chart.BarVaR(x, main = "", xaxis = FALSE, width = width, 
  #             ylab = paste(date.label, "Return"), methods = methods, 
  #             event.labels = NULL, ylog = FALSE, gap = gap, p = p, 
  #             ...)
  chart.RollingCorrelation(Ra=x,Rb=Rb,main="",xaxis=FALSE,width=width,
                           ylab=paste(as.character(width),date.label,"Correlation",colnames(Rb)[1]),event.labels=NULL,...)
  par(mar = c(5, 4, 0, 2))
  chart.Drawdown(x, geometric = geometric, main = "", ylab = "Drawdown", 
                 event.labels = NULL, ylog = FALSE, ...)
  par(op)
}

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

# create output temp
pdf(file="assessment.pdf")

# summary performance plots
# always uses GPSC as correlation benchmark
p_summary <- function(t,title) {
  charts.myPerformanceSummary(series.return[,t],
                              series.return[,which(colnames(series)=="GSPC")],
                              width=21,
                              colorset=gencolor(length(t)),
                              methods="HistoricalES",
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
chart.Boxplot(series.return,sort.by="mean",
              colorset=gencolor(length(colnames(series.return)))#rich.colors(ncol(series.return))
              )


# risk histograms
p_histogram <- function(t) {
  nonNAindex <- which(!is.na(series.return[,t]))
  chart.Histogram(series.return[nonNAindex,t],
                  main=paste(t,"Risk Measures"),
                  methods=c("add.density","add.rug","add.normal","add.qqplot") # ,"add.risk")
                  )

}
tickers[which(tickers=="SPX")] <- "GSPC"
sapply(tickers,p_histogram) # one ticker per plot

# risk/return scatter
p_scatter <- function(t,title) {
  chart.RiskReturnScatter(series.return[,t],
                          Rf=0,
                          colorset=gencolor(length(t)),
                          main=title,
                          add.sharpe=c(1,2,3),
                          add.boxplots=TRUE,
                          cex.lab=1.1,
                          cex.main=1.2
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

dev.off()


# lookback ="4 weeks"
# charts.PerformanceSummary(last(series.return[,1:6],lookback))
# charts.PerformanceSummary(last(series.return[,c(1,7:12)],lookback))
# charts.PerformanceSummary(last(series.return[,c(1,13:19)],lookback))
# chart.Boxplot(series.return)
# chart.CaptureRatios(series.return[,-1],series.return[,1])
# chart.Correlation(series.return)
# chart.RelativePerformance(series.return[,2:6],series.return[,1],legend.loc="topleft")
# chart.RelativePerformance(series.return[,7:12],series.return[,1],legend.loc="topleft")
# chart.RelativePerformance(series.return[,13:19],series.return[,1],legend.loc="topleft")
# chart.RiskReturnScatter(series.return)

