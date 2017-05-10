r <- 0.02

BS <-
  function(S, K, T, r, sig, type="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    if(type=="C"){
      value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    }
    if(type=="P"){
      value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
    }
    return(value)
  }

implied.vol <-
  function(S, K, T, r, market, type){
    sig <- 0.20
    sig.up <- 1
    sig.down <- 0.001
    count <- 0
    err <- BS(S, K, T, r, sig, type) - market 
    
    ## repeat until error is sufficiently small or counter hits 1000
    while(abs(err) > 0.00001 && count<1000){
      if(err < 0){
        sig.down <- sig
        sig <- (sig.up + sig)/2
      }else{
        sig.up <- sig
        sig <- (sig.down + sig)/2
      }
      err <- BS(S, K, T, r, sig, type) - market
      count <- count + 1
    }
    
    ## return NA if counter hit 1000
    if(count==1000){
      return(NA)
    }else{
      return(sig)
    }
  }


VXXdata <- read.csv("VXX_2014.csv", stringsAsFactors=FALSE)
VXXdata$expiration <- as.Date(VXXdata$expiration, "%m/%d/%Y")
VXXdata$quotedate <- as.Date(VXXdata$quotedate, "%m/%d/%Y")
VXXdata$maturity <- as.numeric(VXXdata$expiration-VXXdata$quotedate)/365
VXXdata_filtered <- VXXdata[VXXdata$exchange=="*", ]

dates <- unique(VXXdata_filtered$quotedate)

imp_vol <- lapply(dates, function(dat) {
  
  VXXdata_ind <- VXXdata_filtered[VXXdata_filtered$quotedate==dat, ]
  best_maturity_1m <- min(abs(VXXdata_ind$maturity-30/365))
  VXXdata_best_1m <- VXXdata_ind[abs(VXXdata_ind$maturity-30/365)==best_maturity_1m, ]
  best_maturity_1m <- VXXdata_best_1m$maturity[1]*365
  VXXdata_best_1m_call <- VXXdata_best_1m[VXXdata_best_1m$type=="call", ]
  VXXdata_best_1m_put <- VXXdata_best_1m[VXXdata_best_1m$type=="put", ]
  best_maturity_2m <- min(abs(VXXdata_ind$maturity-60/365))
  VXXdata_best_2m <- VXXdata_ind[abs(VXXdata_ind$maturity-60/365)==best_maturity_2m, ]
  best_maturity_2m <- VXXdata_best_2m$maturity[1]*365
  VXXdata_best_2m_call <- VXXdata_best_2m[VXXdata_best_2m$type=="call", ]
  VXXdata_best_2m_put <- VXXdata_best_2m[VXXdata_best_2m$type=="put", ]
  best_maturity_3m <- min(abs(VXXdata_ind$maturity-90/365))
  VXXdata_best_3m <- VXXdata_ind[abs(VXXdata_ind$maturity-90/365)==best_maturity_3m, ]
  best_maturity_3m <- VXXdata_best_3m$maturity[1]*365
  VXXdata_best_3m_call <- VXXdata_best_3m[VXXdata_best_3m$type=="call", ]
  VXXdata_best_3m_put <- VXXdata_best_3m[VXXdata_best_3m$type=="put", ]
  
  if (nrow(VXXdata_best_1m_call)>0) {
    imp_vol_1m_call <- sapply(1:nrow(VXXdata_best_1m_call), function(i) {
      implied.vol(VXXdata_best_1m_call$underlying_last[i], VXXdata_best_1m_call$strike[i], VXXdata_best_1m_call$maturity[i], 
                  r, (VXXdata_best_1m_call$bid[i]+VXXdata_best_1m_call$ask[i])/2, "C")
    })
  } else imp_vol_1m_call <- NA
  if (nrow(VXXdata_best_1m_put)>0) {
    imp_vol_1m_put <- sapply(1:nrow(VXXdata_best_1m_put), function(i) {
      implied.vol(VXXdata_best_1m_put$underlying_last[i], VXXdata_best_1m_put$strike[i], VXXdata_best_1m_put$maturity[i], 
                  r, (VXXdata_best_1m_put$bid[i]+VXXdata_best_1m_put$ask[i])/2, "P")
    })
  } else imp_vol_1m_put <- NA
  if (nrow(VXXdata_best_2m_call)>0) {
    imp_vol_2m_call <- sapply(1:nrow(VXXdata_best_2m_call), function(i) {
      implied.vol(VXXdata_best_2m_call$underlying_last[i], VXXdata_best_2m_call$strike[i], VXXdata_best_2m_call$maturity[i], 
                  r, (VXXdata_best_2m_call$bid[i]+VXXdata_best_2m_call$ask[i])/2, "C")
    })
  } else imp_vol_2m_call <- NA
  if (nrow(VXXdata_best_2m_put)>0) {
    imp_vol_2m_put <- sapply(1:nrow(VXXdata_best_2m_put), function(i) {
      implied.vol(VXXdata_best_2m_put$underlying_last[i], VXXdata_best_2m_put$strike[i], VXXdata_best_2m_put$maturity[i], 
                  r, (VXXdata_best_2m_put$bid[i]+VXXdata_best_2m_put$ask[i])/2, "P")
    })
  } else imp_vol_2m_put <- NA
  if (nrow(VXXdata_best_3m_call)>0) {
    imp_vol_3m_call <- sapply(1:nrow(VXXdata_best_3m_call), function(i) {
      implied.vol(VXXdata_best_3m_call$underlying_last[i], VXXdata_best_3m_call$strike[i], VXXdata_best_3m_call$maturity[i], 
                  r, (VXXdata_best_3m_call$bid[i]+VXXdata_best_3m_call$ask[i])/2, "C")
    })
  } else imp_vol_3m_call <- NA
  if (nrow(VXXdata_best_3m_put)>0) {
    imp_vol_3m_put <- sapply(1:nrow(VXXdata_best_3m_put), function(i) {
      implied.vol(VXXdata_best_3m_put$underlying_last[i], VXXdata_best_3m_put$strike[i], VXXdata_best_3m_put$maturity[i], 
                  r, (VXXdata_best_3m_put$bid[i]+VXXdata_best_3m_put$ask[i])/2, "P")
    })
  } else imp_vol_3m_put <- NA
  
  list(mean(imp_vol_1m_call, na.rm=TRUE), mean(imp_vol_1m_put, na.rm=TRUE), best_maturity_1m,
       mean(imp_vol_2m_call, na.rm=TRUE), mean(imp_vol_2m_put, na.rm=TRUE), best_maturity_2m,
       mean(imp_vol_3m_call, na.rm=TRUE), mean(imp_vol_3m_put, na.rm=TRUE), best_maturity_3m)
})

VXX_implied_volatilities <- data.frame(date=dates, 
                                       call_1m=unlist(lapply(imp_vol, "[[", 1)),
                                       put_1m=unlist(lapply(imp_vol, "[[", 2)),
                                       mat_1m=unlist(lapply(imp_vol, "[[", 3)),
                                       call_2m=unlist(lapply(imp_vol, "[[", 4)),
                                       put_2m=unlist(lapply(imp_vol, "[[", 5)),
                                       mat_2m=unlist(lapply(imp_vol, "[[", 6)),
                                       call_3m=unlist(lapply(imp_vol, "[[", 7)),
                                       put_3m=unlist(lapply(imp_vol, "[[", 8)),
                                       mat_3m=unlist(lapply(imp_vol, "[[", 9)),
                                       stringsAsFactors=FALSE)
