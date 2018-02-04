# 2018
if(!exists(".ib", mode="environment")) .ib <- new.env()

source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/ib/finddates.R")), local = .ib)
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/ib/counter.R")), local = .ib)
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/ib/savedl.R")), local = .ib)

.ib$bidask <- function(x, # first argument = contract
                       DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),
                       barSize='1 min',duration='5 D',Sys.sleep=1,option='NOSAVE') {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  if(class(x)=='character') x <- twsEquity(x)
  print(paste(x$symbol, substr(DateTime, 1, 8), format(Sys.time(), "%X"))) # x$ticker -> x$symbol
  # where is the sys.sleep?
  a <- b <- NULL
  if(option!='NOSAVE') a <- savedl(x,DateTime,whatToShow='BID',Sys.sleep=Sys.sleep)[,c(1:4)]
  if(option=='NOSAVE') a <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration,useRTH='0',whatToShow='BID')[,c(1:4)]
  counter()
  Sys.sleep(Sys.sleep)
  if(option!='NOSAVE') b <- savedl(x,DateTime,whatToShow='ASK',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
  if(option=='NOSAVE') b <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration,useRTH='0',whatToShow='ASK')[,c(1:4)]
  counter()
  Sys.sleep(Sys.sleep)
  yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
  if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
  if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
  if(is.null(a) & is.null(b)) yy <- NULL
  return(yy) 
}

.ib$indexdl <- function(x,DateTime,Sys.sleep=2) {
  DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  print(paste(x$ticker, substr(DateTime, 1, 8), format(Sys.time(), "%X")))
  a <- savedl(x,DateTime,whatToShow='TRADES',Sys.sleep=Sys.sleep); counter()
  return(a) 
}

.ib$seconddl <- function(x,DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),Sys.sleep=2,save='TRUE') {
  DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S")) # always reformat
  print(paste(DateTime, format(Sys.time(), "%X"))) # x$ticker here incorrect
  if(is.null(save)) {
    a <- reqHistoricalData(tws, x, DateTime,whatToShow='BID',barSize='1 secs', duration='600 S')[,c(1:4)]; Sys.sleep(Sys.sleep); counter()
    b <- reqHistoricalData(tws, x, DateTime,whatToShow='ASK',barSize='1 secs', duration='600 S')[,c(1:4)]; Sys.sleep(Sys.sleep); counter()
  }
  # 
  if(!is.null(save)) {
  a <- savedl(x,DateTime,whatToShow='BID',barSize='1 secs', duration='600 S',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
  b <- savedl(x,DateTime,whatToShow='ASK',barSize='1 secs', duration='600 S',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
  }
  yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
  if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
  if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
  if(is.null(a) & is.null(b)) yy <- NULL
  return(yy) 
}

.ib$getTrades <- function(x,DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),Sys.sleep=2) {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  if(class(x)=='character') x <- twsEquity(x)
  print(paste(x$symbol, substr(DateTime, 1, 8), format(Sys.time(), "%X"))) # x$ticker -> x$symbol
  # a <- savedl(x,DateTime,whatToShow='BID',Sys.sleep=Sys.sleep) # [,c(1:4)]
  a <- reqHistoricalData(tws, x, DateTime, '1 min', '5 D', '0', 'TRADES')
  counter()
  Sys.sleep(Sys.sleep)
  return(a) 
}

.ib$getVol <- function(x, DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")), barSize='1 min', duration='5 D', Sys.sleep=2) {
  if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
  # if(class(x)=='character') x <- twsEquity(x)
  print(paste(x$symbol, substr(DateTime, 1, 8), format(Sys.time(), "%X"))) # x$ticker -> x$symbol
  a <- reqHistoricalData(tws,Contract=x,endDateTime=DateTime,barSize=barSize,duration=duration, useRTH='0',whatToShow='OPTION_IMPLIED_VOLATILITY')
  counter()
  Sys.sleep(Sys.sleep)
  return(a) 
}

### OLD DO NOT USE (passing function call instead of arguments)
# .ib$bidask <- function(y,x) {
#   timexx <<- gsub('-','',format(y, "%Y-%m-%d %H:%M:%S")); print(paste(timexx,format(Sys.time(), "%X")))
#   savexx <<- paste('BA',which(dates==y))
#   a <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','BID')[,c(1:4)]); Sys.sleep(5); counter()
#   b <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','ASK')[,c(1:4)]); Sys.sleep(5); counter()
#   yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
#   if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
#   if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
#   if(is.null(a) & is.null(b)) yy <- NULL
#   return(yy) 
# }

