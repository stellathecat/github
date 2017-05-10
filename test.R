require(xts)
source(gzcon(url("https://raw.githubusercontent.com/stellathecat/github/master/martingale.R")))
test=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uo19beaq2lw0lii/EURCHF2016.rds"))) 

martingale(test,'2015','2016',0.0015,100000,0,2,1)
