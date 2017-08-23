source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/counter.R")))  
bidask <- function(y,x) {
    timexx <<- gsub('-','',format(y, "%Y-%m-%d %H:%M:%S")); print(paste(timexx,format(Sys.time(), "%X")))
    # savexx <<- paste('BA',which(timex==y))
    a <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','BID')[,c(1:4)]); counter()
    b <- savedl(reqHistoricalData(tws,x,timexx,'1 min','5 D','0','ASK')[,c(1:4)]); counter()
    yy <- cbind(a,b) # works because a and b do EXIST (but maybe NULL)
    if(is.null(a)) yy <- cbind(NA,NA,NA,NA,b)
    if(is.null(b)) yy <- cbind(a,NA,NA,NA,NA)
    if(is.null(a) & is.null(b)) yy <- NULL
    return(yy) 
}
