download_daily <- function(x,option=NULL,sys.sleep=NULL,save=NULL) {
  # if(!exists('warn')) warn <<- NULL
  if(class(x)=='character') x <- twsSTK(x)
  data1 <- data2 <- data3 <- data4 <- NA
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  data1 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='OPTION_IMPLIED_VOLATILITY', Sys.sleep=0)[,4]
  data2 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='HISTORICAL_VOLATILITY', Sys.sleep=0)[,4]
  data3 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='MIDPOINT')[,c(1:4)] # MIDPOINT / TRADES
  data4 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='TRADES')[,c(1:4)] # MIDPOINT / 
  try(colnames(data1) <- paste0(x$symbol, '.Implied'), silent = TRUE)
  try(colnames(data2) <- paste0(x$symbol, '.Historical'), silent = TRUE)
  try(colnames(data4) <- paste0(colnames(data4), '.T'), silent = TRUE)
  data <- cbind(data1,data2,data3,data4)
  index(data) <- as.Date(index(data), tz='Europe/Berlin')
  return(data)
}

download_daily_vol <- function(x,option=NULL,sys.sleep=NULL,save=NULL) {
  # if(!exists('warn')) warn <<- NULL
  if(class(x)=='character') x <- twsSTK(x)
  today <- gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S"))
  data1 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='OPTION_IMPLIED_VOLATILITY', Sys.sleep=0)[,4]
  data2 <- savedl(Contract=x, barSize='1 day', duration='15 Y', whatToShow='HISTORICAL_VOLATILITY', Sys.sleep=0)[,4]
  try(colnames(data1) <- paste0(x$symbol, '.Implied'), silent = TRUE)
  try(colnames(data2) <- paste0(x$symbol, '.Historical'), silent = TRUE)
  if(is.null(data1)) data1 <- NA
  if(is.null(data2)) data2 <- NA
  data <- cbind(data1,data2)
  try(index(data) <- as.Date(index(data), tz='Europe/Berlin'))
  return(data)
}
