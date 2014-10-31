Performance Summary - Advisor Shares Portfolios
===============================================





```r
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

# risk histograms
p_histogram <- function(t) {
  nonNAindex <- which(!is.na(series.return[,t]))
  chart.Histogram(series.return[nonNAindex,t],
                  main=paste(t,"Risk Measures"),
                  methods=c("add.density","add.rug","add.normal","add.qqplot") # ,"add.risk")
                  )

}

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

# relative performance
p_relative <- function(t,ref,title,lastp="10 years") {
  chart.RelativePerformance(last(series.return[,t],lastp),
                            last(series.return[,ref],lastp),
                            main=paste(title,"Performance Relative to",ref),
                            colorset=gencolor(length(t)+1),
                            legend.loc="topleft"
  )
}
```



```r
# not active: HDGB, MULT, YPRO, HOLD, GEUR, GGBP, GYEN, GLDE
tickers_slipstream = c("MINC", "HYLD", "ROOF", "FWDB", "FWDI", "QEH", "AADR", 
    "RSP")
tickers_short = c("HDGE", "HDGI")
tickers_long_short = c("AGLS", "QEH")
tickers_multi_asset = c("GTAA", "MATH", "DBIZ", "GIVE", "VEGA")
tickers_dom_equity = c("FWDD", "TTFS")
tickers_intl_equity = c("AADR", "FWDI")
tickers_global_equity = c("ACCU", "EPRO")
tickers_income = c("HYLD", "MINC", "FWDB")
tickers_reference = c("^GSPC", "TLT")
tickers_mbcbehf = c("CFT", "HYG", "LQD", "MUNI", "PSK", "RSP")

tickers <- unique(c(tickers_short, tickers_long_short, tickers_multi_asset, 
    tickers_dom_equity, tickers_intl_equity, tickers_global_equity, tickers_income, 
    tickers_reference, tickers_mbcbehf, tickers_slipstream))

series.env <- new.env()
start.date <- "2010-07-12"
palette.name <- "Dark2"
global.colorset = tim6equal

getSymbols(tickers, from = start.date, env = series.env)
```

```
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
```

```
##  [1] "HDGE" "HDGI" "AGLS" "QEH"  "GTAA" "MATH" "DBIZ" "GIVE" "VEGA" "FWDD"
## [11] "TTFS" "AADR" "FWDI" "ACCU" "EPRO" "HYLD" "MINC" "FWDB" "GSPC" "TLT" 
## [21] "CFT"  "HYG"  "LQD"  "MUNI" "PSK"  "RSP"  "ROOF"
```

```r
volume <- Vo(do.call(merge, as.list(series.env)))
series <- Ad(do.call(merge, as.list(series.env)))
colnames(volume) <- sub("^(.*)[.].*", "\\1", colnames(volume))
colnames(series) <- sub("^(.*)[.].*", "\\1", colnames(series))

series.return <- Return.calculate(series)
series.cumulative <- Return.cumulative(series.return)
series.zeroed <- series.return
series.zeroed[is.na(series.zeroed)] <- 0
series.cumret <- cumprod(1 + series.zeroed)

# recover an S&P-500 label
tickers[which(tickers == "^GSPC")] <- "SPX"

series.cumret.df <- data.frame(series.cumret)
series.cumret.df$date <- as.Date(rownames(series.cumret.df))
```




```r
p_summary(tickers_slipstream, "Slipstream Component Strategy")
```

![plot of chunk summary](figure/summary1.png) 

```r
p_summary(tickers_short, "Short Alternative Strategy")
```

![plot of chunk summary](figure/summary2.png) 

```r
p_summary(tickers_long_short, "Long/Short Alternative Strategy")
```

![plot of chunk summary](figure/summary3.png) 

```r
p_summary(tickers_multi_asset, "Multi-Asset Alternative Strategy")
```

![plot of chunk summary](figure/summary4.png) 

```r
p_summary(tickers_dom_equity, "Domestic Equity Core Strategy")
```

![plot of chunk summary](figure/summary5.png) 

```r
p_summary(tickers_intl_equity, "International Equity Core Strategy")
```

![plot of chunk summary](figure/summary6.png) 

```r
p_summary(tickers_global_equity, "Global Equity Core Strategy")
```

![plot of chunk summary](figure/summary7.png) 

```r
p_summary(tickers_income, "Income Core Strategy")
```

![plot of chunk summary](figure/summary8.png) 

```r
p_summary(tickers_mbcbehf, "Alternative Portfolio")
```

![plot of chunk summary](figure/summary9.png) 



```r
# boxplot
chart.Boxplot(series.return,sort.by="mean",
              colorset=gencolor(length(colnames(series.return))) 
              #rich.colors(ncol(series.return))
              )
```

![plot of chunk distribution](figure/distribution1.png) 

```r


tickers[which(tickers=="SPX")] <- "GSPC"
sapply(tickers,p_histogram) # one ticker per plot
```

![plot of chunk distribution](figure/distribution2.png) ![plot of chunk distribution](figure/distribution3.png) ![plot of chunk distribution](figure/distribution4.png) ![plot of chunk distribution](figure/distribution5.png) ![plot of chunk distribution](figure/distribution6.png) ![plot of chunk distribution](figure/distribution7.png) ![plot of chunk distribution](figure/distribution8.png) ![plot of chunk distribution](figure/distribution9.png) ![plot of chunk distribution](figure/distribution10.png) ![plot of chunk distribution](figure/distribution11.png) ![plot of chunk distribution](figure/distribution12.png) ![plot of chunk distribution](figure/distribution13.png) ![plot of chunk distribution](figure/distribution14.png) ![plot of chunk distribution](figure/distribution15.png) ![plot of chunk distribution](figure/distribution16.png) ![plot of chunk distribution](figure/distribution17.png) ![plot of chunk distribution](figure/distribution18.png) ![plot of chunk distribution](figure/distribution19.png) ![plot of chunk distribution](figure/distribution20.png) ![plot of chunk distribution](figure/distribution21.png) ![plot of chunk distribution](figure/distribution22.png) ![plot of chunk distribution](figure/distribution23.png) ![plot of chunk distribution](figure/distribution24.png) ![plot of chunk distribution](figure/distribution25.png) ![plot of chunk distribution](figure/distribution26.png) ![plot of chunk distribution](figure/distribution27.png) ![plot of chunk distribution](figure/distribution28.png) 

```
## $HDGE
## NULL
## 
## $HDGI
## NULL
## 
## $AGLS
## NULL
## 
## $QEH
## NULL
## 
## $GTAA
## NULL
## 
## $MATH
## NULL
## 
## $DBIZ
## NULL
## 
## $GIVE
## NULL
## 
## $VEGA
## NULL
## 
## $FWDD
## NULL
## 
## $TTFS
## NULL
## 
## $AADR
## NULL
## 
## $FWDI
## NULL
## 
## $ACCU
## NULL
## 
## $EPRO
## NULL
## 
## $HYLD
## NULL
## 
## $MINC
## NULL
## 
## $FWDB
## NULL
## 
## $GSPC
## NULL
## 
## $TLT
## NULL
## 
## $CFT
## NULL
## 
## $HYG
## NULL
## 
## $LQD
## NULL
## 
## $MUNI
## NULL
## 
## $PSK
## NULL
## 
## $RSP
## NULL
## 
## $ROOF
## NULL
```



```r
tickers_reference[which(tickers_reference == "^GSPC")] <- "GSPC"
op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
p_scatter(c(tickers_slipstream, tickers_reference), "Slipstream Component Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter1.png) 

```r
p_scatter(c(tickers_short, tickers_reference), "Short Alternative Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter2.png) 

```r
p_scatter(c(tickers_long_short, tickers_reference), "Long/Short Alternative Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter3.png) 

```r
p_scatter(c(tickers_multi_asset, tickers_reference), "Multi-Asset Alternative Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter4.png) 

```r
p_scatter(c(tickers_dom_equity, tickers_reference), "Domestic Equity Core Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter5.png) 

```r
# 
p_scatter(c(tickers_intl_equity, tickers_reference), "International Equity Core Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter6.png) 

```r
p_scatter(c(tickers_global_equity, tickers_reference), "Global Equity Core Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter7.png) 

```r
p_scatter(c(tickers_income, tickers_reference), "Income Core Strategy")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter8.png) 

```r
p_scatter(tickers_reference, "Reference Indices/Products")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter9.png) 

```r
p_scatter(tickers_mbcbehf, "Alternative Portfolio")
```

```
## Warning: graphical parameter "cin" cannot be set
## Warning: graphical parameter "cra" cannot be set
## Warning: graphical parameter "csi" cannot be set
## Warning: graphical parameter "cxy" cannot be set
## Warning: graphical parameter "din" cannot be set
## Warning: graphical parameter "page" cannot be set
```

![plot of chunk scatter](figure/scatter10.png) 

```r
par(op)
```



```r
p_rolling(c(tickers_slipstream, tickers_reference), "Slipstream Component Strategy")
```

![plot of chunk rolling](figure/rolling1.png) 

```r
p_rolling(c(tickers_short, tickers_reference), "Short Alternative Strategy")
```

![plot of chunk rolling](figure/rolling2.png) 

```r
p_rolling(c(tickers_long_short, tickers_reference), "Long/Short Alternative Strategy")
```

![plot of chunk rolling](figure/rolling3.png) 

```r
p_rolling(c(tickers_multi_asset, tickers_reference), "Multi-Asset Alternative Strategy")
```

![plot of chunk rolling](figure/rolling4.png) 

```r
p_rolling(c(tickers_dom_equity, tickers_reference), "Domestic Equity Core Strategy")
```

![plot of chunk rolling](figure/rolling5.png) 

```r
p_rolling(c(tickers_intl_equity, tickers_reference), "International Equity Core Strategy")
```

![plot of chunk rolling](figure/rolling6.png) 

```r
p_rolling(c(tickers_global_equity, tickers_reference), "Global Equity Core Strategy")
```

![plot of chunk rolling](figure/rolling7.png) 

```r
p_rolling(c(tickers_income, tickers_reference), "Income Core Strategy")
```

![plot of chunk rolling](figure/rolling8.png) 

```r
p_rolling(c(tickers_mbcbehf, tickers_reference), "Alternative Portfolio")
```

![plot of chunk rolling](figure/rolling9.png) 



```r
p_relative(tickers_slipstream, "GSPC", "Slipstream Component Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative1.png) 

```r
p_relative(tickers_slipstream, "TLT", "Slipstream Component Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative2.png) 

```r
p_relative(tickers_slipstream, "GSPC", "Slipstream Component Strategy", "3 months")
```

![plot of chunk relative](figure/relative3.png) 

```r
p_relative(tickers_slipstream, "TLT", "Slipstream Component Strategy", "3 months")
```

![plot of chunk relative](figure/relative4.png) 

```r

p_relative(tickers_short, "GSPC", "Short Alternative Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative5.png) 

```r
p_relative(tickers_long_short, "GSPC", "Long/Short Alternative Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative6.png) 

```r
p_relative(tickers_multi_asset, "GSPC", "Multi-Asset Alternative Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative7.png) 

```r
p_relative(tickers_dom_equity, "GSPC", "Domestic Equity Core Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative8.png) 

```r
p_relative(tickers_intl_equity, "GSPC", "International Equity Core Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative9.png) 

```r
p_relative(tickers_global_equity, "GSPC", "Global Equity Core Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative10.png) 

```r
p_relative(tickers_income, "TLT", "Income Core Strategy")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative11.png) 

```r
p_relative(tickers_mbcbehf, "GSPC", "Alternative Portfolio")
```

```
## Warning: requested length is greater than original
## Warning: requested length is greater than original
```

![plot of chunk relative](figure/relative12.png) 


M.R.Barry Sun Jun 15 12:08:42 2014


