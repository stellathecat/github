# http://www.cboe.com/micro/vix/vixwhite.pdf

library(quantmod)
library(data.table) # ?
# getSymbols("^VIX") # does not work anymore?
VIX=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/na18uqtjd6f5dm6/VIX.rds")))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read options data
spx_data = read.csv("SPX_data.csv", stringsAsFactors=FALSE)
#spx_data = read.csv("https://www.dropbox.com/s/bucxejlv2attaa0/SPX_data.csv?dl=1", stringsAsFactors=FALSE)
#If you're on windows try readr::read_csv() instead of read.csv() 

spx_data = spx_data[spx_data$days==30, ]
# spx_data = spx_data[complete.cases(spx_data), ] # 0 obs. ?
spx_data = spx_data[!is.na(spx_data$impl_volatility), ] # added May 2017
spx_data$date = as.Date(as.character(spx_data$date), "%Y%m%d")
all_dates = unique(spx_data$date)

# test
spx_data2a = spx_data[spx_data$delta==(-50), ] # ca. 20 years times 252 obs = 5000
spx_data2b = spx_data[spx_data$delta==(50), ]
test=cbind(xts(spx_data2a$impl_volatility, spx_data2a$date),VIX)
test=cbind(xts(spx_data2b$impl_volatility, spx_data2b$date),VIX)
plot.zoo(test[,1])
par(new = T)
plot.zoo(test[,2], col='red')
# end test

#some constants
Texp = 30/365 #time to expiration
R = 0.000305 #risk-free interest rate to expiration

#run in a loop for every date
VIX_replicated = sapply(all_dates, function(current_date) {
# VIX_replicated = sapply(all_dates[1], function(current_date) {
  spx_data_today = spx_data[spx_data$date==current_date, ]
  spx_data_today = spx_data_today[order(spx_data_today$delta), ]
  # print(spx_data_today)
  
  #calculate dK
  spx_data_today$delta_K = (shift(spx_data_today$delta, 1, type="lead")-shift(spx_data_today$delta, 1))/2
  spx_data_today$delta_K[1] = spx_data_today$delta[2]-spx_data_today$delta[1]
  spx_data_today$delta_K[length(spx_data_today$delta_K)] = spx_data_today$delta[length(spx_data_today$delta_K)]-spx_data_today$delta[length(spx_data_today$delta_K)-1]
  
  # test, page 9: The contribution of the near-term 1370 Put is given by
  # options(scipen = 999)
  # (5/(1370^2))*exp(0.000305*0.0683486)*0.2 # ok
  
  #calculate weight
  spx_data_today$weight = spx_data_today$delta_K/(spx_data_today$impl_strike^2)*exp(R*Texp)*spx_data_today$impl_premium 
  # added *spx_data_today$impl_premium 
  
  # print(spx_data_today)
  # print(sum(spx_data_today$weight))
  
  sigma_squared = 2/Texp*sum(spx_data_today$weight*spx_data_today$impl_volatility)
  
  vix_value = 100*sqrt(sigma_squared)
  # vix_value = 100*sqrt(sigma_squared)*sqrt(12)
  # vix_value = 100*sigma_squared
})

#create resulting dataframe
VIX_replicated = data.frame(Date=all_dates, VIX=NA, VIX_replicated=VIX_replicated)
plot(VIX_replicated[,3])

test=cbind(xts(VIX_replicated[,3], VIX_replicated[,1]),VIX)
plot.zoo(test[,1])
par(new = T)
plot.zoo(test[,2], col='red')

VIX_replicated$VIX[all_dates %in% index(VIX)] = VIX$VIX.Close[all_dates]

#some plotting
VIX_to_plot = VIX_replicated[VIX_replicated$Date>="2015-01-01", ]

png('plot.png')
plot(VIX_to_plot$Date, VIX_to_plot$VIX, type="l", col="green", xlab="", ylab="", ylim=c(0, 50))
lines(VIX_to_plot$Date, VIX_to_plot$VIX_replicated, type="l", col="blue")
dev.off()
