savedl <- function(func) {
  # asdasd <<- as.list(match.call())
  # test <<- match.call()
  # test <<- deparse(substitute(func))
  # test2 <<- get('time', mode='character') funktioniert nicht
  success <- FALSE
  store <- deparse(match.call(), width.cutoff = 500L, nlines=1)
  store <- gsub('"','',substr(store, 38, nchar(store)-2))
  while (!success) {
    success <- TRUE 
    xx <- tryCatch(func, warning = function(c) { 
      # print(paste(store, c$message))
      # warn <<- append(warn, paste(store, c$message))
      print(paste(timexx, c$message))
      warn <<- append(warn, paste(savexx, timexx, c$message))
      if(grepl('Connectivity',c$message)) success <<- FALSE
      if(grepl('No security definition',c$message)) success <<- FALSE
    })
    Sys.sleep(6)
  }
  return(xx)
}
