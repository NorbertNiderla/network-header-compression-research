library(dplyr)
library(bit)

source("tANS/bit_utils.R")

tANS_coding_symbol <- function(tANS_coder_state, coding_table, new_symbol){
        
        browser()
        
        I_s <- tANS_coder_state$state
        all_I_s <- coding_table %>% filter(symbol == new_symbol & is.na(output_state) == FALSE) %>% select(input_state)
        I_s_max <- max(all_I_s)
        output_bits <- tANS_coder_state$output_bits
        while(I_s > I_s_max){
                output_val <- I_s %% tANS_coder_state$b
                nBits <- log2(tANS_coder_state$b)
                output_val_bits <- as.bit(as.integer(intToBits(output_val)))[1:nBits]
                # output_bit <- I_s %% 2
                pos <- length(output_bits) + 0
                for(idx in 1:nBits)
                        output_bits[pos + idx] <- output_val_bits[idx]
                I_s <- I_s %/% tANS_coder_state$b
        }
        
        output_state <- coding_table %>% filter(symbol == new_symbol & input_state == I_s) %>% select(output_state)
        tANS_coder_state$output_bits <- output_bits
        
        tANS_coder_state$state <- output_state$output_state
        return(tANS_coder_state)
}

tANS_coding <- function(tANS_coder_state, coding_table, input_stream){
        for(symbol in input_stream)
                tANS_coder_state <- tANS_coding_symbol(tANS_coder_state, coding_table, round(symbol))
        
        return(tANS_coder_state)
}

tANS_decoding <- function(tANS_decoder_state, coding_table){
        output_string <- c()
        state <- tANS_decoder_state$state
        min_new_state <- min((coding_table %>% filter(is.na(output_state) == FALSE))$output_state)
        
        while(length(tANS_decoder_state$input_bits) > 0){
                coding_table_entry <- coding_table %>% filter(output_state == state)
                decoded_symbol <- coding_table_entry$symbol
                output_string <- c(output_string, decoded_symbol)
                state <- coding_table_entry$input_state
                while(state < min_new_state){
                        state <- state * tANS_decoder_state$b
                        decoding_input_last_idx <- length(tANS_decoder_state$input_bits)
                        decoding_input_first_idx <- decoding_input_last_idx - log2(tANS_decoder_state$b) + 1
                        decoding_input_bits <- tANS_decoder_state$input_bits[decoding_input_first_idx:decoding_input_last_idx]
                        state <- state + bitToInt(decoding_input_bits)
                        
                        if(decoding_input_first_idx == 1){
                                tANS_decoder_state$input_bits <- bit(0)
                                break
                        }
                        tANS_decoder_state$input_bits <- tANS_decoder_state$input_bits[1:(decoding_input_first_idx - 1)]
                }
        }
        
        tANS_decoder_state$output_string <- output_string
        return(tANS_decoder_state)
}