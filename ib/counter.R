if(!exists(".ib", mode="environment")) .ib <- new.env()
if(!exists(".hidden", mode="environment")) .hidden <- new.env()

.ib$counter <- function() { 
  if(!exists('count', envir = .hidden)) { .hidden$count <- 0; .hidden$j <- Sys.time() }
  .hidden$count <- .hidden$count+1; print(.hidden$count) # Making more than 60 requests within any ten minute period.
  if(.hidden$count==60) { print(Sys.time()-.hidden$j); .hidden$count <- 0; .hidden$j <- Sys.time() } }

# https://www.r-bloggers.com/environments-in-r/
# here too: counter

# counter <- function() { if(!exists('count')) { count <<- 0; j <<- Sys.time() }
#   attach(.GlobalEnv)
#   count <<- count+1
#   # count <<- get('count', envir = .GlobalEnv)+1
#   print(count) # Making more than 60 requests within any ten minute period.
#   # print(get('count', envir = .GlobalEnv)) # getAnywhere
#   # if(count==60) { print(paste('time elapsed for 60 requests (in mins): ',Sys.time()-j)); count <<- 0; j <<- Sys.time() } }
#   if(count==60) { print(Sys.time()-j); count <<- 0; j <<- Sys.time() } }
# 
# test <- function() { hitler <<- 88
# print(environment())
# print(parent.env(environment()))
# print(parent.env(parent.env(environment())))
# assign('count', 1, envir = environment())
# assign('count', 1, envir = 'myfuns')
# }
# print(environment())