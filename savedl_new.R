savedl <- function(Contract, endDateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")), 
                   barSize='1 min', duration='5 D', useRTH='0', whatToShow='TRADES', Sys.sleep=0) {
  success <- FALSE
  while (!success) {
    success <- TRUE 
    xx <- tryCatch(reqHistoricalData(tws, Contract, endDateTime, barSize, duration='5 D', useRTH='0', whatToShow), 
                   warning = function(c) { 
      print(paste(endDateTime, c$message))
      if(!exists('warn')) warn <<- NULL
      warn <<- append(warn, paste(substr(endDateTime, 1, 8), whatToShow, c$message))
      if(grepl('Connectivity',c$message)) success <<- FALSE
      if(grepl('No security definition',c$message)) success <<- FALSE
    })
    Sys.sleep(Sys.sleep)
  }
  return(xx)
}
