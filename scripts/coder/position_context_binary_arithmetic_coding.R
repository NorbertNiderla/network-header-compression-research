source("./scripts/coder/arithmetic_coding.R")

position_context_binary_arithmetic_encode <- function(stream, prob_0_table){
      stopifnot(length(stream) %% 8 == 0)
      stopifnot(length(prob_0_table) == length(stream))
      
      high <- 2147483647
      low <- 0
      scale <- 0
      out <- c()
      context <- 1
      
      for(symbol in stream){
            step = high - low + 1
            if( symbol == 0){
                  high = low + (ceiling(step * prob_0_table[context])) - 1
                  low = low
            } else if(symbol == 1){
                  high = low + step - 1
                  low = low + floor(step * prob_0_table[context])
            }
            
            context <- context + 1
            
            ac_state <- arithmetic_encode_main_loop(high, low, scale)
            high <- ac_state$high
            low <- ac_state$low
            scale <- ac_state$scale
            out <- c(out, ac_state$out)
            
      }
      
      ac_state <- arithmetic_encode_ending_loop(high, low, scale)
      out <- c(out, ac_state$out)
      
      return(out);
}

position_context_binary_arithmetic_decode <-function(stream, prob_0_table, target_size){
      low <- 0
      high <- 2147483647
      value <- 0
      idx <- 1
      stream <- c(stream, integer(60))
      for(x in 1:31){
            value <- value * 2 + stream[idx]
            idx <- idx + 1
      }
      
      data <- c()
      context <- 1
      
      for(i in 1:target_size){
            step <- high - low + 1
            cum <- (value - low) / step
            if(cum <= prob_0_table[context]){
                  data[i] = 0
            } else {
                  data[i] = 1
            }
            
            if(data[i] == 0){
                  high = low + (ceiling(step * prob_0_table[context])) - 1
                  low = low
            } else if(data[i] == 1){
                  high = low + step - 1
                  low = low + floor(step * prob_0_table[context])
            }
            
            context <- context + 1
            
            ac_state <- arithmetic_decode_main_loop(high, low, value, stream, idx)
            high <- ac_state$high
            low <- ac_state$low
            value <- ac_state$value
            idx <- ac_state$idx
            
      }
      return(data)
}