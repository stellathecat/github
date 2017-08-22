  counter <- function() { if(!exists('count')) { count <<- 0; j <<- Sys.time() }
    count <<- count+1; print(count) # Making more than 60 requests within any ten minute period.
    # if(count==60) { print(paste('time elapsed for 60 requests (in mins): ',Sys.time()-j)); count <<- 0; j <<- Sys.time() } }
    if(count==60) { print(Sys.time()-j); count <<- 0; j <<- Sys.time() } }
