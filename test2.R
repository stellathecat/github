closeAllConnections()
require(xts)
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/martingale.R")))
test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) 
test2=readRDS('~/Dropbox/Public/AAPLmincorrecttime.rds')

# function(v1,v2,v3,increment_position,increment_profit,betsize,trade_com,version,plot)
martingale(test,'2016-01','2016-01',0.0005,0.0040,100000,0,version=1,plot=1)

1.08525*0.001 % 0.1%
check <- martingale(test,'2016-01','2016-01','0.001%','0.001%',100000,0,plot=1)
sum(check$realized_profit != 0)
check <- martingale(test2,'2016-01','2016-01','0.001%','0.001%',100000,0,version=2,plot=1) # biased because of overnight
sum(check$realized_profit != 0)

test <- test['2016-05']
test2 <- test2['2016-05']

### OLD ----
# changed function to return a list
check <- lapply(split(test, 'days'), function(x) martingale(x,'2016','2016','0.001%','0.001%',100000,0,plot=0)) # version=2,
# check <- do.call(rbind, check)
# sum(check$realized_profit != 0)

check <- lapply(split(test2, 'days'), function(x) martingale(x,'2016','2016','0.001%','0.001%',100000,0,plot=0)) # version=2,
lapply(check, checkplot)
check[[14]][2] # not realisable, falls from high to close
check[[16]][2]
check <- do.call(rbind, check)
sum(check$realized_profit != 0)
###

check <- martingale(test,'2016-01','2016-01',0.0011,0.0011,100000,0,plot=0)
check <- martingale(test,'2016-01','2016-01','0.001%','0.001%',100000,0,plot=0)

checkplot = function(x) { 
main <- index(first((x[[2]][,4]+x[[2]][,8])/2))
plot.zoo((x[[2]][,4]+x[[2]][,8])/2, main=main, ylab='', xlab='')
trade_history <- x[[4]]
trade_history$datetime <- as.POSIXct(as.character(trade_history$datetime))
points(trade_history$datetime[trade_history$operation=="buy"], trade_history$price[trade_history$operation=="buy"], pch='-', col='blue')
points(trade_history$datetime[trade_history$operation=="sell"], trade_history$price[trade_history$operation=="sell"], pch='-', col='red')
}
