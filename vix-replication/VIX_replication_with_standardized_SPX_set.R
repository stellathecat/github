library(quantmod)
getSymbols("^VIX")

#read options data
spx_data = read.csv("SPX_data.csv", stringsAsFactors=FALSE)
spx_data = spx_data[spx_data$days==30, ]
spx_data = spx_data[complete.cases(spx_data), ]
spx_data$date = as.Date(as.character(spx_data$date), "%Y%m%d")
all_dates = unique(spx_data$date)

#some constants
Texp = 30/365 #time to expiration
R = 0.000305 #risk-free interest rate to expiration

#run in a loop for every date
VIX_replicated = sapply(all_dates, function(current_date) {
  spx_data_today = spx_data[spx_data$date==current_date, ]
  spx_data_today = spx_data_today[order(spx_data_today$delta), ]
  
  #calculate dK
  spx_data_today$delta_K = (shift(spx_data_today$delta, 1, type="lead")-shift(spx_data_today$delta, 1))/2
  spx_data_today$delta_K[1] = spx_data_today$delta[2]-spx_data_today$delta[1]
  spx_data_today$delta_K[length(spx_data_today$delta_K)] = spx_data_today$delta[length(spx_data_today$delta_K)]-spx_data_today$delta[length(spx_data_today$delta_K)-1]
  
  #calculate weight
  spx_data_today$weight = spx_data_today$delta_K/(spx_data_today$impl_strike^2)*exp(R*Texp)
  
  sigma_squared = 2/Texp*sum(spx_data_today$weight*spx_data_today$impl_volatility)
  vix_value = 100*sqrt(sigma_squared)
  vix_value
})

#create resulting dataframe
VIX_replicated = data.frame(Date=all_dates, VIX=NA, VIX_replicated=VIX_replicated)
VIX_replicated$VIX[all_dates %in% index(VIX)] = VIX$VIX.Close[all_dates]

#some plotting
VIX_to_plot = VIX_replicated[VIX_replicated$Date>="2015-01-01", ]
plot(VIX_to_plot$Date, VIX_to_plot$VIX, type="l", col="green", xlab="", ylab="", ylim=c(0, 50))
lines(VIX_to_plot$Date, VIX_to_plot$VIX_replicated, type="l", col="blue")
