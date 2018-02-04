if(!exists(".ib", mode="environment")) .ib <- new.env()

.ib$finddates = function(x, type='BID') { 
  if(class(x)=='character') x <- twsEquity(x)
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  if(type=='BID') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','BID')))
  if(type=='OPT') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','OPTION_IMPLIED_VOLATILITY')))
  if(type=='TRADES') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','TRADES')))
  if(Sys.Date()-last(dates)>7) { # IF NOT UNTIL TODAY
    print(last(dates))
    dates <- seq(dates[1],last(dates)+7,by=1) # in case of missing data points, make consistent series
  }
  if(Sys.Date()-last(dates)<7) {
    dates <- seq(dates[1],Sys.Date(),by=1) # in case of missing data points, make consistent series
  }
  dates[weekdays(dates) %in% 'Sunday']
}
