require(xts)
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/martingale.R")))
test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) 

# martingale(test,'2015','2016',0.0163,100000,0,2,1)
martingale(test,'2015','2016','0.015%',100000,0,2,1)

# little test
# for (i in c(1:30)) {
# interval <- paste(seq(0.0010,by=0.0005,length.out=30),'%',sep='')[i]
# checkhowmuch <- martingale(test,'2015','2016',interval,100000,0,2,0)
# ret <- sum(checkhowmuch$realized_profit != 0)/length(unique(as.Date(index(test))))
# print(ret)
# }

# miau