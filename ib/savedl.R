# if(!exists(".ib", mode="environment")) .ib <- new.env() # MUST exist?
if(!exists(".hidden", mode="environment")) .hidden <- new.env()
if(!exists('warn', envir = .hidden)) .hidden$warn <- NULL

.ib$savedl <- function(Contract, endDateTime=gsub('-','',format(Sys.Date(), "%Y-%m-%d %H:%M:%S")), 
                   barSize='1 min', duration='5 D', useRTH='0', whatToShow='TRADES', Sys.sleep=0) {
  success <- FALSE
  while (!success) {
    success <- TRUE 
    xx <- tryCatch(reqHistoricalData(tws, Contract, endDateTime, barSize, duration, useRTH, whatToShow), 
                   warning = function(w) { 
      print(paste(substr(endDateTime, 1, 8), w$message))
      .hidden$warn <- append(.hidden$warn, paste(substr(endDateTime, 1, 8), whatToShow, w$message))
      if(grepl('Connectivity',w$message)) { 
        success <<- FALSE; Sys.sleep(20); twsDisconnect(tws); Sys.sleep(20); tws <<- ibgConnect() }
      if(grepl('No security definition',w$message)) { 
        success <<- FALSE; Sys.sleep(20); twsDisconnect(tws); Sys.sleep(20); tws <<- ibgConnect() }
      # if(grepl('Historical Market Data Service',w$message)) {  }
      if(grepl('Error processing request',w$message)) { 
        success <<- FALSE; Sys.sleep(20); twsDisconnect(tws); Sys.sleep(20); tws <<- ibgConnect() }
      Sys.sleep(1)
    #}, error = function(e) { print('fritz'); success <<- FALSE; Sys.sleep(20); twsDisconnect(tws); Sys.sleep(20); tws <- ibgConnect() })
      })
      Sys.sleep(Sys.sleep)
    gc()
  }
  return(xx)
}

### OLD VERSION WITH PASSING FUNCTION CALL --- no need anymore
# savedl <- function(func) {
#   # asdasd <<- as.list(match.call())
#   # test <<- match.call()
#   # test <<- deparse(substitute(func))
#   # test2 <<- get('time', mode='character') # does not work
#   success <- FALSE
#   store <- deparse(match.call(), width.cutoff = 500L, nlines=1)
#   store <- gsub('"','',substr(store, 38, nchar(store)-2))
#   while (!success) {
#     success <- TRUE 
#     xx <- tryCatch(func, warning = function(c) { 
#       # print(paste(store, c$message))
#       # warn <<- append(warn, paste(store, c$message))
#       print(paste(timexx, c$message))
#       warn <<- append(warn, paste(savexx, timexx, c$message)) # savexx defined in bidask and reqhis 
#       if(grepl('Connectivity',c$message)) success <<- FALSE
#       if(grepl('No security definition',c$message)) success <<- FALSE
#       # Historical Market Data Service error message
#       # Error processing request
#     })
#     Sys.sleep(6)
#   }
#   return(xx)
# }
