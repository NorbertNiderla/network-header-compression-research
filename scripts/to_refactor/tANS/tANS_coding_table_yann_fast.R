source("coders/ANS/tANS_coding_table.R")


#sum of count cant be 8

tANS_coding_table_yann_fast <- function(symbol_distribution){
  
  count_sum <- sum(symbol_distribution$count)
  basic_table <- tANS_coding_table_generate_basic(symbol_distribution)
  output<-vector("character", count_sum)
  
  current_state = 0
  for(x in 1:count_sum){
    output[current_state+1]<-basic_table$symbol[x]
    current_state <- (current_state + (1/8)*count_sum + 3) %% count_sum
  }
  
  return(output)
  
}