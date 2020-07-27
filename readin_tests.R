loc <- 'C:/Users/felix.dietrich/Dropbox/Public/2020/'
a <- list.files(loc)
b <- a # [1:1000]

by <- unlist(lapply(strsplit(b, '_'), function(x) x[1]))
c <- split(a, by)

# c[[1]]
# c[['110616560']]
# c[[names(c)[1]]]

for (i in names(c)) {
  print(i)
  d <- lapply(c[[i]], function(x) {
    readRDS(paste0(loc, x))
  })
  e <- do.call(rbind, d)
  saveRDS(e, paste0(i, '.rds'))
}

d <- lapply(c[[1]], function(x) {
  readRDS(paste0(loc, x))
})
e <- do.call(rbind, d)

require(IBrokers)
require(IBFelix)
tws <- ibgConnect()
asd <- sapply(names(c), function(x) getContract(x, local=T))
# (USD), (EUR), (GBP), (JPY), (AUD), (NZD), (CAD), (CHF), (NOK), (SEK)

getContract <- function(x,sectype='FUT',details=FALSE,local=FALSE,include_expired=1) {
  # this function sometimes does not work immediately...
  options(warn=-1) # suppress warnings due to is.na
  contract <- twsContract()
  # https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
  if(!is.na(as.numeric(x))) contract$conId <- paste(x)
  if(is.na(as.numeric(x))) {
    contract$sectype = sectype
    contract$local <- paste(x)
  }
  contract$include_expired <- as.character(include_expired)
  # if(expired==0) contract$include_expired <- "0"
  options(warn=0)
  if(details) return(try(reqContractDetails(tws,contract)[[1]]))
  if(local) return(try(reqContractDetails(tws,contract)[[1]]$contract)$local)
  return(try(reqContractDetails(tws,contract)[[1]]$contract))
}

ticker <- c(110616605, 12087792, 12087797, 12087807, 12087817,
            12087820, 12087826, 14321010, 14321015, 14321016, 14433401, 15016059, 15016062, 15016065, 15016068, 15016075, 15016078,
            15016125, 15016133, 15016138, 15016234, 15016241,
            28027113, 28027122,
            37890923, 37893486, 37893488, 37893493,
            37943440, 37943445, 37971206, 39453424, 39453434, 39453441, 39453444, 46189223, 46189224, 47101302,
            47101305)
asd <- sapply(as.character(ticker), function(x) getContract(x, local=T))
asd
asd[as.character(ticker)]

loc <- 'C:/Users/felix.dietrich/Dropbox/Public/2020/'
x <- readRDS(paste0(loc, as.character(ticker)[1], '.rds')) 
x <- readRDS(paste0(loc, '12087817.rds')) # EURCHF

y <- readRDS('C:/Users/felix.dietrich/Dropbox/Public/1MIMPL.rds')
# y[['NOKJPYV1M Curncy']]['2019-12-30/2020-01-02']

# dae795b7de32ba7f3ef2680c670c56f71b297b88 
source(url("https://raw.githubusercontent.com/felixdietrich/main/master/maschine_func.R?token=ABPDJHDA6R7NTD5JG6U7SJ27D2YRM"))
source(url("https://raw.githubusercontent.com/felixdietrich/main/master/maschine_func.R?token=dae795b7de32ba7f3ef2680c670c56f71b297b88"))

require(QCAM)
fdb_bdp('NOKUSD Curncy', 'PX_LAST')
fdb_bdp('USDNOK Curncy', 'PX_LAST')
fdb_reset()
martingale
# 1.10*10/10000*
1.10*5/10000

l1
l1 <- median(xts::first(x))
#
l2 <- median(y[['NOKJPYV1M Curncy']]['2019-12-30/2020-01-02'])
median(y[['EURCHFV1M Curncy']]['2019-12-30/2020-01-02'])

l1*l2/10000
l1
require(xts)
check <- martingale(x, '2020-01-01', '2020-06-31', 0.02, 0.02, 90000, 0, 0)
check <- martingale(x, '2020-01-01', '2020-06-31', 0.001, 0.001, 10000, 0, 0)

xtsAttributes(x) <- list('currency'='JPY')
attributes(x)
coredata
head(check[[1]])
attributes(check[[1]])
reshape2::melt(data.frame(index(check[[1]]), check[[1]][,c('realized pnl','unrealized pnl','total')]))
check2 <- broom::tidy(check[[1]][,c('realized pnl','unrealized pnl','total')]) 
check2 <- broom::tidy(check[[1]][,c('realized pnl','unrealized pnl','total')]/110)
ggplot(check2, aes(x=index, y=value, color=series)) + geom_line() + theme(legend.title = element_blank(), legend.position="top")
plot.zoo(check[[1]][,1])
range(check[[1]][,1])
diff(log(range(check[[1]][,1])))
1.051347/1.087322

###

for (i in ticker) {
  print(i)
  x <- readRDS(paste0(loc, as.character(i), '.rds')) 
  if(is.null(x)) next
  base <- unique(unlist(lapply(strsplit(colnames(x), "\\."), function(x) x[1])))
  quote <- unique(unlist(lapply(strsplit(colnames(x), "\\."), function(x) x[2])))
  xtsAttributes(x) <- list('base_currency'=base, 'quote_currency'=quote)
  saveRDS(x, paste0(as.character(i), '.rds'))
}

x <- readRDS(paste0(loc, as.character(12087797), '.rds')) 
x
loc
x <- readRDS('C:/Users/felix.dietrich/Dropbox/Public/2020/12087817.rds') # EURCHF
x <- readRDS('C:/Users/felix.dietrich/Dropbox/Public/2020/Without_Attributes/12087817.rds') # EURCHF

xtsAttributes()

###

listed <- list()

lapply(ticker)
for (i in ticker) {
  print(i)
  x <- try(readRDS(paste0(loc, as.character(i), '.rds')))
  if(!exists('x')) next
  listed[i] <- xtsAttributes(x)[c('base_currency','quote_currency')]
  rm(x)
}
listed
exists
