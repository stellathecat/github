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
x4 = x[[4]]

require(xts)
data <- readRDS("C:/Users/Felix/Dropbox/EURCHF_2020.rds")
x <- martingale(data, v2='2020-01-01', v3='2021-01-01', 
                increment_position=0.0010,increment_profit=0.0010,betsize=10000,trade_com=0,plot=1)

test[[1]]

x2$buy <- diff(x2$outstanding)>0
x2$sell <- diff(x2$outstanding)<0
luk <- x2['/2016-01-08']

plot.zoo(luk$EURCHF.Close.1)
tail(index(luk))
unique(weekdays(index(luk)))
tzone(luk) <- 'EST5EDT'
x4
abline(h = index(x2$buy), col='red')
index(x2$buy)[100]
abline(v = index(luk[which(luk$sell==1),]), col='red')
abline(v = index(luk[which(luk$buy==1),]), col='green')

x4
abline
sell <- index(luk[which(luk$sell==1),])
buy <- index(luk[which(luk$buy==1),])
autoplot(luk$EURCHF.Close.1) + xlab('') + ylab('') + geom_vline(xintercept = buy, col='green') + geom_vline(xintercept = sell, col='red')

dev.off()
par(mfrow=c(1,2))
require(gridExtra)
x <- data.frame(row.names=paste("Name",1:10))
x[,1] <- 1:10
x[,2] <- sample(1:100,10)
x[,3] <- sample(LETTERS[1:26],10)
colnames(x) <- c("Value 1", "Value 2", "Label")
grid.table(x) # https://stackoverflow.com/questions/30794273/plot-a-data-frame-as-a-table

require(plotrix)
addtable2plot(0.7 ,8, x4[1:10,], bty="o",display.rownames=TRUE,hlines=TRUE,
              vlines=TRUE,title="The table") # braucht plotrix chart?

# PLOT WEEKLY TRADE as PDF (like in Vol...R)
# PLOT TWO OPTIONS INCL UNREALIZED AND INCL POSITION

ll <- x4[1:87,1:3]
rownames(ll) <- NULL

# , linetype='dashed'
a1 <- autoplot(luk$EURCHF.Close.1) + xlab('') + ylab('') + geom_vline(xintercept = buy, col='green', linetype='dashed', size = 0.3) + 
  geom_vline(xintercept = sell, col='red', linetype='dashed', size = 0.3)
a1

a2 <- tableGrob(x4[1:87,1:3], rows = NULL, theme=ttheme_minimal(base_size = 3, padding = unit(c(1, 1), "mm")))
grid.arrange(a1,a2, nrow=1)  
grid.arrange(a1,a2, widths = c(4,2))  
ll
pdf(width = 8, 'miautest2b.pdf')
grid.arrange(a1,a3, widths = c(6,2))  
# grid.arrange(a1,a2, widths = c(4,2))  
dev.off()

pdf(width = 8, 'miautest3.pdf')
a1
dev.off()

justify <- function(x, hjust="center", vjust="center", draw=TRUE){
  w <- sum(x$widths)
  h <- sum(x$heights)
  xj <- switch(hjust,
               center = 0.5,
               left = 0.5*w,
               right=unit(1,"npc") - 0.5*w)
  yj <- switch(vjust,
               center = 0.5,
               bottom = 0.5*h,
               top=unit(1,"npc") - 0.5*h)
  x$vp <- grid::viewport(x=xj, y=yj)
  if(draw) grid::grid.draw(x)
  return(x)
}

a3 <- justify(a2, "center", "top", draw=F)

# GEOM_POINTS
points(trade_history$datetime[trade_history$operation=="buy"], trade_history$price[trade_history$operation=="buy"], pch='-', col='blue')
points(trade_history$datetime[trade_history$operation=="sell"], trade_history$price[trade_history$operation=="sell"], pch='-', col='red')

autoplot(luk$EURCHF.Close.1) + xlab('') + ylab('') # + geom_point(data=buy, color='red',size=3) 
ggplot(luk$EURCHF.Close.1, aes(x = Index, y = EURCHF.Close.1)) + geom_line() + xlab('') + ylab('') +
  geom_point(data=luk[which(luk$buy==1),8], pch='-', aes(x = Index, y = EURCHF.Close.1), colour="green", size=8) +
  geom_point(data=luk[which(luk$sell==1),8], pch='-', aes(x = Index, y = EURCHF.Close.1), colour="red", size=8)

buy <- index(luk[which(luk$buy==1),])
luk[which(luk$buy==1),4]
buy
# geom_point(data=mydata[10:13, ], aes(x=a, y=b), colour="red", size=5)


plot_machine <- function(x) {
  
}

  # plot(as.zoo(M[,1]), col='red', ylab='Underlying', main='Machine')
  # mtext(paste('from',index(first(M)),'to',index(last(M))))
  # par(new = T)
  # plot(as.zoo(M[,8]), yaxt='n', ylab='', col='blue', lty='dotted', lwd=2)
  # axis(side = 4)
  # legend('topleft', c('Underlying','Machine'), cex=0.6, lty=c('solid','dotted'), col=c('red','blue'))
  # 
  # trade_history$datetime <- as.POSIXct(as.character(trade_history$datetime))
  # plot.zoo((data[,4]+data[,8])/2, main='', ylab='', xlab='')
  # points(trade_history$datetime[trade_history$operation=="buy"], trade_history$price[trade_history$operation=="buy"], pch='-', col='blue')
  # points(trade_history$datetime[trade_history$operation=="sell"], trade_history$price[trade_history$operation=="sell"], pch='-', col='red')

autoplot(x2$outstanding)
autoplot(cbind(x1[,c('realized pnl','unrealized pnl')], x1[,c('total pnl')]-1000000), facet = NULL) + xlab('') + ylab('') +
  theme(legend.title = element_blank(), legend.position="right")

colnames(x2)
ggplot(x1$outstanding, aes(x = Index, y = outstanding)) + geom_area() + xlab('') + ylab('')

x1$outstanding
### LEARN
# GGHIGHLIGHT

require(QCAM)
plot_double(x1$last, x1$outstanding)
# legend('top',c('EURCHF','Outstanding'),lty=c(1,1),col=1:2) # outside
plot.zoo(x1[,c('outstanding','unrealized pnl')], plot.type = 'single', col=1:2)
plot_double(x1[,c('outstanding','unrealized pnl')])
3000000*0.030/2
x1
x1
