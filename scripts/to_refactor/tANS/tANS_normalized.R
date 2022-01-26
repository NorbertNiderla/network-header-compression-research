library(dplyr)
library(bit)

source("coders/bit_utils.R")

tANS_normalized_coding <- function(tANS_normalized_coder_state, coding_table, new_symbol){
 
     min_n_bits <- (tANS_normalized_coder_state$min_n_bits %>% filter(symbol == new_symbol))$n_bits
     threshold <- (tANS_normalized_coder_state$thresholds %>% filter(symbol == new_symbol))$threshold
     n_bits <- min_n_bits 
     if(tANS_normalized_coder_state$state >= threshold)
          n_bits <- n_bits + 1
     
     output_val <- tANS_normalized_coder_state$state %% (2^n_bits)
     output_val_bits <- as.bit(as.integer(intToBits(output_val)))[1:n_bits]
     print(output_val_bits)
     
     I_s <- tANS_normalized_coder_state$state + tANS_normalized_coder_state$L
     I_s <- I_s %/% 2^n_bits
     
     output_state <- coding_table %>% filter(symbol == new_symbol & input_state == I_s) %>% select(output_state)
     
     tANS_normalized_coder_state$state <- output_state$output_state
     return(tANS_normalized_coder_state)
}

tANS_normalized_decoding <- function(tANS_normalized_decoder_state, coding_table){
     state <- tANS_normalized_decoder_state$state

     while(length(tANS_normalized_decoder_state$input_bits) > 0){
          coding_table_entry <- coding_table %>% filter(output_state == state)
          decoded_symbol <- coding_table_entry$symbol
          print(decoded_symbol)
          state <- coding_table_entry$input_state
          
          min_n_bits <- (tANS_normalized_decoder_state$min_n_bits %>% filter(symbol == decoded_symbol))$n_bits
          while(state < tANS_normalized_decoder_state$L){
               state <- state * (2^min_n_bits)
               decoding_input_last_idx <- length(tANS_normalized_decoder_state$input_bits)
               decoding_input_first_idx <- decoding_input_last_idx - min_n_bits + 1
               decoding_input_bits <- tANS_normalized_decoder_state$input_bits[decoding_input_first_idx:decoding_input_last_idx]
               state <- state + bitToInt(decoding_input_bits)
               
               if(decoding_input_first_idx == 1){
                    tANS_normalized_decoder_state$input_bits <- bit(0)
                    break
               }
               tANS_normalized_decoder_state$input_bits <- tANS_normalized_decoder_state$input_bits[1:(decoding_input_first_idx - 1)]
               min_n_bits <- 1
          }
          state <- state - tANS_normalized_decoder_state$L
     }
}

coding_table_example <- rbind(
     data.frame(symbol = "A", input_state = c(0:5), output_state = c(NA, NA, NA, 0, 3, 6)),
     data.frame(symbol = "B", input_state = c(0:5), output_state = c(NA, NA, NA, 1, 4, 7)),
     data.frame(symbol = "C", input_state = c(0:5), output_state = c(NA, NA, 2, 5, NA, NA))
)

tANS_normalized_coder_state_example <- list(
     state = 0, 
     L = 8, 
     b = 2, 
     min_n_bits = data.frame(
          symbol = c("A", "B", "C"),
          n_bits = c(1, 1, 2)
     ),
     thresholds = data.frame(
          symbol = c("A", "B", "C"),
          threshold = c(4, 4, 8)
     ))

test_string <- c("B", "A", "C", "A")
tANS_normalized_decoder_state_example <- list(
     state = 6,
     L = 8,
     b = 2,
     min_n_bits = data.frame(
          symbol = c("A", "B", "C"),
          n_bits = c(1, 1, 2)
     ),
     input_bits = as.bit(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
)

# test_string <- c("A", "B", "B", "A")
# tANS_normalized_decoder_state_example <- list(
#      state = 3, 
#      L = 8,
#      b = 2,
#      min_n_bits = data.frame(
#           symbol = c("A", "B", "C"),
#           n_bits = c(1, 1, 2)
#      ),
#      input_bits = as.bit(c(FALSE, TRUE, TRUE, TRUE, TRUE))
# )

for(symbol in test_string)
     tANS_normalized_coder_state_example <- tANS_normalized_coding(tANS_normalized_coder_state_example, coding_table_example, symbol)

tANS_normalized_decoding(tANS_normalized_decoder_state_example, coding_table_example)