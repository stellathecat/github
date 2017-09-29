getContract = function(x,y='contract') {
contract <- twsContract()
contract$conId <- paste(x)
contract$include_expired <- "1" 
if(y=='details') return(reqContractDetails(tws,contract)[[1]])
return(reqContractDetails(tws,contract)[[1]]$contract)
}
