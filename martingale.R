# to-do:
# change transaction costs
# change profit taking
# change type: fx vs. stocks (transaction costs)

martingale = function(v1,v2,v3,increment_position,increment_profit,betsize,trade_com,plot=0)
{
  data=v1[paste(v2, "/", v3, sep = "")]
  #data=get(x)
  #data=data[paste(input$dates[1], "/", input$dates[2], sep = "")] # new
  data$outstanding <- NA
  data$realized_profit <- 0
  #data$at <- NA
  
  N <- nrow(data)
  level <- coredata(data[1,4]+data[1,8])/2
  initial <- level
  if(class(increment_position)=='character') increment_position<-round(level*as.numeric(gsub('%','',increment_position)),4)
  if(class(increment_profit)=='character') increment_profit<-round(level*as.numeric(gsub('%','',increment_profit)),4)
  print(initial); print(increment_position); print(increment_profit)
  outstanding <- 0
  data <- as.data.frame(data)
  ind <- as.list(rownames(data))
  data <- as.list(data)
  
  # insert new trade data to stack
  stack_push <- function(time, operation, price, act_price)
  {
    new_stack <- trade_stack
    new_data <- data.frame(datetime = time, operation = operation, price = price, act_price = act_price)
    colnames(new_data) <- c("datetime", "operation", "price", "act_price")
    new_stack <- rbind(new_stack, new_data)
    trade_stack <<- new_stack
  }
  # insert new trade data to history
  history_push <- function(time, operation, price, act_price)
  {
    new_history <- trade_history
    new_data <- data.frame(datetime = time, operation = operation, price = price, act_price = act_price)
    colnames(new_data) <- c("datetime", "operation", "price", "act_price")
    new_history <- rbind(new_history, new_data)
    trade_history <<- new_history
  }
  # get trade data from stack
  stack_get_last <- function()
  {
    if (nrow(trade_stack)>0) {
      trade_stack[nrow(trade_stack), ]
    } else {
      data.frame(datetime = 0, operation = "0", price = 0, act_price = 0)
    }
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
  trade_history <- trade_stack
  
  # if ask low < indicative level - increment_profit, then close sell positions and update indicative level
  # if ask low < indicative level - increment, then buy and update indicative level
  # if bid high > indicative level + increment_profit, then close buy positions and update indicative level
  # if bid high > indicative level + increment, then sell and update indicative level
  # running backtesting
  for (i in 1:N)
  {
    #incProgress(1/N, detail = paste(round(i/N, 2)*100, '%', sep=''))
    if (data[[7]][i] <= (level-increment_profit) && stack_get_last()[, 2]=="sell") { # close 'sell' positions and maybe buy
      
      realized_profit <- 0 # initialize
      
      while (stack_get_last()[, 2]=="sell" && stack_get_last()[, 3]-increment_profit>=data[[7]][i]) {
        last_trade <- stack_pop() # get last sell operation
        realized_profit <- realized_profit + betsize*increment_profit - 2*trade_com
        history_push(ind[[i]], "buy", last_trade[1]-increment_profit, data[[7]][i])
        outstanding <- outstanding + betsize # update oustanding
      }
      if (stack_get_last()[, 2]=="sell") {
        level <- stack_get_last()[, 3] # update indicative level
      } else {
        if (data[[7]][i] < (last_trade[1]-increment_profit)) { # after closing all 'sell' positions, new 'buy' position may be opened
          stack_push(ind[[i]], "buy", data[[7]][i], data[[7]][i])
          history_push(ind[[i]], "buy", data[[7]][i], data[[7]][i])
          outstanding <- outstanding + betsize # update oustanding
        }
        level <- data[[7]][i] # update indicative level
      }
      data[[9]][i] <- outstanding # update data
      if (realized_profit > 0) data[[10]][i] <- realized_profit
    }
    else if (data[[7]][i] <= (level-increment_position) && stack_get_last()[, 2]!="sell") { # buy
      
      count_trades <- floor((level-data[[7]][i])/increment_position) # how many times to buy
      realized_profit <- 0 # initialize
      if (count_trades > 0) { # process for each buy operation
        for (j in 1:count_trades) {
          if (outstanding>=0) { # if no sell oprations, then put to portfolio
            stack_push(ind[[i]], "buy", level-j*increment_position, data[[7]][i])
            history_push(ind[[i]], "buy", level-j*increment_position, data[[7]][i])
          } else {
            last_trade <- stack_pop() # get last sell operation
            realized_profit <- realized_profit + betsize*(last_trade[1]-(level-j*increment_position)) - 2*trade_com
            history_push(ind[[i]], "buy", level-j*increment_position, data[[7]][i])
          }
          outstanding <- outstanding + betsize # update oustanding
        }
        level <- level-count_trades*increment_position # update indicative level
      }
      data[[9]][i] <- outstanding # update data
      if (realized_profit > 0) data[[10]][i] <- realized_profit
    }
    else if (data[[2]][i] >= (level+increment_profit) && stack_get_last()[, 2]=="buy") { # close 'buy' positions and maybe sell
      
      realized_profit <- 0 # initialize
      
      while (stack_get_last()[, 2]=="buy" && stack_get_last()[, 3]+increment_profit<=data[[2]][i]) {
        last_trade <- stack_pop() # get last sell operation
        realized_profit <- realized_profit + betsize*increment_profit - 2*trade_com
        history_push(ind[[i]], "sell", last_trade[1]+increment_profit, data[[2]][i])
        outstanding <- outstanding - betsize # update oustanding
      }
      if (stack_get_last()[, 2]=="buy") {
        level <- stack_get_last()[, 3] # update indicative level
      } else {
        if (data[[2]][i] > (last_trade[1]+increment_profit)) { # after closing all 'buy' positions, new 'sell' position may be opened
        stack_push(ind[[i]], "sell", data[[2]][i], data[[2]][i])
        history_push(ind[[i]], "sell", data[[2]][i], data[[2]][i])
        outstanding <- outstanding - betsize # update oustanding
        }
        level <- data[[2]][i] # update indicative level
      }
      data[[9]][i] <- outstanding # update data
      if (realized_profit > 0) data[[10]][i] <- realized_profit
    } 
    else if (data[[2]][i] >= (level+increment_position) && stack_get_last()[, 2]!="buy") { # sell
      
      count_trades <- floor((data[[2]][i]-level)/increment_position) # how many times to sell
      realized_profit <- 0 # initialize
      if (count_trades > 0) { # process for each sell operation
        for (j in 1:count_trades) {
          if (outstanding>0) { # get last buy operation
            last_trade <- stack_pop() # calculate profit
            realized_profit <- realized_profit + betsize*((level+j*increment_position)-last_trade[1]) - 2*trade_com
            history_push(ind[[i]], "sell", level+j*increment_position, data[[2]][i])
          } else {
            trade_stack <- stack_push(ind[[i]], "sell", level+j*increment_position, data[[2]][i])
            history_push(ind[[i]], "sell", level+j*increment_position, data[[2]][i])
            # if no buy oprations, then put to protfolio
          }
          outstanding <- outstanding - betsize # update oustanding
        }
      level <- level+count_trades*increment_position # update indicative level
      }
      data[[9]][i] <- outstanding # update data
      if (realized_profit > 0) data[[10]][i] <- realized_profit
    } else { # do nothing
    }
    
    #data[[11]][i] <- level # add level (sold/bought at) for plotting later
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
  
  trade_history$datetime <- as.POSIXct(as.character(trade_history$datetime))
  plot.zoo((data[,4]+data[,8])/2, main='', ylab='', xlab='')
  points(trade_history$datetime[trade_history$operation=="buy"], trade_history$price[trade_history$operation=="buy"], pch='-', col='blue')
  points(trade_history$datetime[trade_history$operation=="sell"], trade_history$price[trade_history$operation=="sell"], pch='-', col='red')
  }
  
  # if(version==1) return(M) # return daily summary of everything
  # if(version==2) return(data) # return all data
  # if(version==3) return(M4b) # return sum of realized profit
  return(list(M,data))
}
