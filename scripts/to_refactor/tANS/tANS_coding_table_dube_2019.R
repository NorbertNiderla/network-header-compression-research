source("coders/ANS/tANS_coding_table.R")

# INPUT: "symbol_distribution" where "$symbol" is the symbol, "$count" is the count
# OUTPUT: "coding_table" with "$symbol", "$input_state" and "$output_state"
tANS_coding_table_dube_2019 <- function(symbol_distribution){
     sequence_string <- rANS_sequence_string(symbol_distribution)
     
     n_states <- length(sequence_string)
     
     while(TRUE){
          # print(sequence_string)
          coding_table <- tANS_coding_table_from_string(sequence_string)
          
          stationary_distribution <- tANS_coding_table_stationary_distribution(coding_table)
          
          # sort
          new_state_order_idx <- order(-stationary_distribution)
          if(all(new_state_order_idx == c(1:n_states)) == TRUE)               # we are done
               break
          sequence_string <- sequence_string[new_state_order_idx]
          
          # reiterate
     }
     
     return(coding_table)
}