source("coders/ANS/tANS_coding_table.R")

# Adapted from the "actual sorting" procedure described in C++ language at
# http://cbloomrants.blogspot.com/2014/02/02-06-14-understanding-ans-8.html
tANS_coding_table_duda_precise <- function(symbol_distribution, bias = 0.5){
     output_sequence <- data.frame()
     
     for(idx in 1:nrow(symbol_distribution)){
          s <- symbol_distribution$symbol[idx]
          count <- symbol_distribution$count[idx]
          invp <- 1.0/count
          base <- bias * invp
          for(c in 1:count){
               symbol_sequence <- data.frame(
                    symbol = s,
                    rank = base + c * invp
               )
               output_sequence <- rbind(output_sequence, symbol_sequence)
          }
     }
     
     sorted_idx <- order(output_sequence$rank)     

     return(output_sequence$symbol[sorted_idx])
}