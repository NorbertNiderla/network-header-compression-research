tANS_coding_table_yann_precise <- function(symbol_distribution){
  
  count_sum = sum(symbol_distribution$count)
  output <- vector("character",count_sum)
  symbol_distribution$period <- count_sum/symbol_distribution$count
  symbol_distribution$val <- symbol_distribution$period
  
  for(x in 1:count_sum){
    symbol_distribution<-symbol_distribution[order(symbol_distribution$val),] #symbols need to be sorted by val and symbol(if counts are likely to be the same)
    symbol_distribution$val[1] <- symbol_distribution$val[1] + symbol_distribution$period[1]
    output[x] <- symbol_distribution$symbol[1]
  }
  
  return(output)
  
}
