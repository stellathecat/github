source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/counter.R")))
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/xdates.R")))  
# source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/savedl.R")))
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/savedl_new.R")))
bidask <- function(y,x) {
    timexx <<- gsub('-','',format(y, "%Y-%m-%d %H:%M:%S")); print(paste(timexx,format(Sys.time(), "%X")))
    savexx <<- paste('BA',which(dates==y))
    a <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','BID')[,c(1:4)]); Sys.sleep(5); counter()
    b <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','ASK')[,c(1:4)]); Sys.sleep(5); counter()
    yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
    if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
    if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
    if(is.null(a) & is.null(b)) yy <- NULL
    return(yy) 
}

bidask2 <- function(x,DateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")),Sys.sleep=2) {
    if(class(DateTime)=='Date') DateTime <- gsub('-','',format(DateTime, "%Y-%m-%d %H:%M:%S"))
    print(paste(x$ticker, substr(DateTime, 1, 8), format(Sys.time(), "%X")))
    a <- savedl(x,DateTime,whatToShow='BID',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
    b <- savedl(x,DateTime,whatToShow='ASK',Sys.sleep=Sys.sleep)[,c(1:4)]; counter()
    yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
    if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
    if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
    if(is.null(a) & is.null(b)) yy <- NULL
    return(yy) 
}
