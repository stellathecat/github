combine_weekly_rds <- function(loc='C:/Users/felix.dietrich/Documents/GitHub/QCAM/QCAM/') {
  # loc <- 'C:/Users/felix.dietrich/Dropbox/Public/2020/ToDo/'
  a <- list.files(loc)
  a <- a[grepl('.rds', a)]
  by <- unlist(lapply(strsplit(a, '_'), function(x) x[1]))
  c <- split(a, by)
  
  for (i in names(c)) {
    print(i)
    d <- lapply(c[[i]], function(x) {
      readRDS(paste0(loc, x))
    })
    e <- do.call(rbind, d)
    saveRDS(e, paste0(i, '.rds'))
  }
}

a <- readRDS('C:/Users/felix.dietrich/Dropbox/Public/2020/Without_Attributes/14321016.rds')
b <- readRDS('C:/Users/felix.dietrich/Dropbox/Public/2020/Without_Attributes/14321016n.rds')
head(a)
head(b)
