# to-do:
# change transaction costs
# change profit taking
# change type: fx vs. stocks (transaction costs)

martingale = function(v1,v2,v3,increment,betsize,trade_com,version,plot)
{
  data=v1[paste(v2, "/", v3, sep = "")]
  #data=get(x)
  #data=data[paste(input$dates[1], "/", input$dates[2], sep = "")] # new
  data$outstanding <- NA
  data$realized_profit <- 0
  N <- nrow(data)
  level <- coredata(data[1,4]+data[1,8])/2
  initial <- level
  if(class(increment)=='character') increment<-round(level*as.numeric(gsub('%','',increment)),4)
  print(initial); print(increment)
  outstanding <- 0
  data <- as.data.frame(data)
  ind <- as.list(rownames(data))
  data <- as.list(data)
  
  # insert new trade data to stack
  stack_push <- function(time, operation, price, act_price)
  {
    new_stack <- trade_stack
    n <- nrow(new_stack)
    new_stack <- rbind(new_stack, data.frame(datetime = time, operation = operation, price = price, act_price = act_price))
    trade_stack <<- new_stack
  }
  # pop trade data from stack
  stack_pop <- function()
  {
    new_stack <- trade_stack
    n <- nrow(new_stack)
    pop_trade <- as.matrix(new_stack[n, -(1:2)])
    trade_stack <<- new_stack[-n, ]
    pop_trade
  }
  # calculate unrealized profit when necessary
  stack_unrealized_profit <- function(last_price)
  {
    n <- nrow(trade_stack)
    if (n>0) {
      trade_prices <- sum(trade_stack$price)
      profit <- ifelse(trade_stack$operation[1] == "buy", -betsize*(trade_prices-n*last_price), betsize*(trade_prices-n*last_price)) - n*trade_com
    } else
      profit <- 0
    profit
  }
  # stack of current unrealized operations
  trade_stack <- data.frame(datetime = numeric(0), operation = character(0), price = numeric(0), act_price = numeric(0), stringsAsFactors=F)
  
  # if ask close < indicative level - increment, then buy and update indicative level
  # if bid close > indicative level + increment, then sell and update indicative level
  # running backtesting
  for (i in 1:N)
  {
    #incProgress(1/N, detail = paste(round(i/N, 2)*100, '%', sep=''))
    if (data[[7]][i]<=(level-increment)) { # buy
      
      count_trades <- floor((level-data[[7]][i])/increment) # how many times to buy
      realized_profit <- 0 # initialize
      if (count_trades > 0) { # process for each buy operation
        for (j in 1:count_trades) {
          if (outstanding>=0) { # if no sell oprations, then put to portfolio
            stack_push(ind[[i]], "buy", level-j*increment, data[[7]][i])
          } else {
            last_trade <- stack_pop() # get last sell operation
            realized_profit <- realized_profit + betsize*(last_trade[1]-(level-j*increment)) - 2*trade_com
          }
          outstanding <- outstanding + betsize # update oustanding
        }
      level <- level-count_trades*increment # update indicative level
      }
      data[[9]][i] <- outstanding # update data
      if (realized_profit > 0) data[[10]][i] <- realized_profit
    } 
    else if (data[[2]][i] >= (level+increment)) { # sell
      
      count_trades <- floor((data[[2]][i]-level)/increment) # how many times to sell
      realized_profit <- 0 # initialize
      if (count_trades > 0) { # process for each sell operation
        for (j in 1:count_trades) {
          if (outstanding>0) { # get last buy operation
            last_trade <- stack_pop() # calculate profit
            realized_profit <- realized_profit + betsize*((level+j*increment)-last_trade[1]) - 2*trade_com
          } else {
            trade_stack <- stack_push(ind[[i]], "sell", level+j*increment, data[[2]][i])
            # if no buy oprations, then put to protfolio
          }
          outstanding <- outstanding - betsize # update oustanding
        }
      level <- level+count_trades*increment # update indicative level
      }
      data[[9]][i] <- outstanding # update data
      if (realized_profit > 0) data[[10]][i] <- realized_profit
    } else { # do nothing
    }
    # do some processing in the end of the day
    if (strftime(ind[[i]], format="%H:%M:%S") == "23:59:00") { # process after day end
    }
  }
  
  # convert back to ts
  data <- xts(do.call(cbind, data), order.by=as.POSIXct(unlist(ind)))
  # update outstanding for all timestamps
  if (is.na(data$outstanding[1])) data$outstanding[1] <- 0
  data$outstanding <- na.locf(data$outstanding, na.rm=F)
  
  options(scipen = 999)
  M1 <- apply.daily((data[, 4] + data[, 8]) / 2, mean) # before: last
  #   M1=apply.daily((data[,4]+data[,8])/2, last)
  M2 <- M1 - as.numeric(initial)
  
  # realized profits
  M3 <- apply.daily(data$realized_profit, sum) / M1 # to get p&l in original currency
  # M3 <- apply.daily(data$realized_profit, sum) #/120 # yen
  M4 <- cumsum(M3)
  M4b <- as.numeric(last(M4))
  
  # unrealized profits // this is only an approximation
  M5 <- M2 / 2
  M6 <- apply.daily(data$outstanding, last)
  M7 <- M5 * M6 / M1 # to get p&l in original currency // attention only use for fx
  #   M7=M5*M6 #/120 # yen
  
  # equity curve
  M8 <- M4 + M7 + 1000000
  M9 <- diff(log(M8))
  M <- cbind(M1, M2, M3, M4, M5, M6, M7, M8, M9)
  
  index(M) <- as.Date(index(M))
  colnames(M) <- c('last','spot difference','daily realized','realized pnl',
                   'average spot difference','outstanding','unrealized pnl','total pnl','drets')
  
  if (plot == 1) {
  plot(as.zoo(M[,1]), col='red', ylab='Underlying', main='Machine')
  mtext(paste('from',index(first(M)),'to',index(last(M))))
  par(new = T)
  plot(as.zoo(M[,8]), yaxt='n', ylab='', col='blue', lty='dotted', lwd=2)
  axis(side = 4)
  legend('topleft', c('Underlying','Machine'), cex=0.6, lty=c('solid','dotted'), col=c('red','blue'))
  }
  
  if(version==1) return(M) # return daily summary of everything
  if(version==2) return(data) # return all data
  if(version==3) return(M4b) # return sum of realized profit
}  
