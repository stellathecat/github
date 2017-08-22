finddates = function(x, for='BID') {
today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
if(for=='BID') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','BID')))
if(for=='OPT') dates <- as.Date(index(reqHistoricalData(tws,x,today,'1 week','15 Y','0','OPTION_IMPLIED_VOLATILITY')))
dates <- seq(dates[1],Sys.Date(),by=1)
dates[weekdays(dates) %in% 'Sunday']
}
