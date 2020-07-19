require(xts)
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/martingale.R")))
# test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) # OLD FROM 2017
# test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/sf54jeg4m4awj8u/EURCHF2016.rds"))) # error
test <- readRDS(url('https://www.dropbox.com/s/sf54jeg4m4awj8u/EURCHF2016.rds?dl=1')) # funktioniert (manchmal?)
test <- readRDS('~/Dropbox/stellathecat/data/EURCHF2016.rds')

# martingale(test,'2015','2016',0.0163,100000,0,2,1)
martingale(test,'2015','2016','0.015%',100000,0,2,1)
options(scipen = 999)
x <- martingale(test,'2015','2016',0.001,0.001,100000,0,0)


# little test
# for (i in c(1:30)) {
# interval <- paste(seq(0.0010,by=0.0005,length.out=30),'%',sep='')[i]
# checkhowmuch <- martingale(test,'2015','2016',interval,100000,0,2,0)
# ret <- sum(checkhowmuch$realized_profit != 0)/length(unique(as.Date(index(test))))
# print(ret)
# }

# miau

str(x)
x1 = x[[1]]
x2 = x[[2]]
x3 = x[[3]]

require(xts)
data <- readRDS("C:/Users/Felix/Dropbox/EURCHF_2020.rds")
x <- martingale(data, v2='2020-01-01', v3='2021-01-01', 
                increment_position=0.0010,increment_profit=0.0010,betsize=10000,trade_com=0,plot=1)

test[[1]]

# PLOT WEEKLY TRADE as PDF (like in Vol...R)
# PLOT TWO OPTIONS INCL UNREALIZED AND INCL POSITION
plot_machine <- function(x) {
  
}

  plot(as.zoo(M[,1]), col='red', ylab='Underlying', main='Machine')
  mtext(paste('from',index(first(M)),'to',index(last(M))))
  par(new = T)
  plot(as.zoo(M[,8]), yaxt='n', ylab='', col='blue', lty='dotted', lwd=2)
  axis(side = 4)
  legend('topleft', c('Underlying','Machine'), cex=0.6, lty=c('solid','dotted'), col=c('red','blue'))
  
  trade_history$datetime <- as.POSIXct(as.character(trade_history$datetime))
  plot.zoo((data[,4]+data[,8])/2, main='', ylab='', xlab='')
  points(trade_history$datetime[trade_history$operation=="buy"], trade_history$price[trade_history$operation=="buy"], pch='-', col='blue')
  points(trade_history$datetime[trade_history$operation=="sell"], trade_history$price[trade_history$operation=="sell"], pch='-', col='red')

