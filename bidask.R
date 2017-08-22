  bidask <- function(y,x) {
    timexx <<- gsub('-','',format(y, "%Y-%m-%d %H:%M:%S")); print(paste(timexx,format(Sys.time(), "%X")))
    # savexx <<- paste('BA',which(timex==y))
    # a <- reqHistoricalData(tws,x,time,'1 min','5 D','0','BID')[,c(1:4)]; Sys.sleep(8); counter()
    # b <- reqHistoricalData(tws,x,time,'1 min','5 D','0','ASK')[,c(1:4)]; Sys.sleep(8); counter()
    # a <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','BID')[,c(1:4)]); counter()
    # b <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','ASK')[,c(1:4)]); counter()
    a <- reqHistoricalData(tws,x,timexx,'1 min','5 D','0','BID')[,c(1:4)]
    b <- reqHistoricalData(tws,x,timexx,'1 min','5 D','0','ASK')[,c(1:4)]
    # if(is.null(a)) a<-NA; if(is.null(b)) b<-NA; yy <- cbind(a,b); return(yy) } # SET NA to NA and CBIND
    yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
    if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
    if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
    if(is.null(a) & is.null(b)) yy <- NULL
    Sys.sleep(6)
    return(yy) 
  }
