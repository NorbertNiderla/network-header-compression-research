library(bit)
source("coders/ANS/tANS_coding_table.R")
source("coders/ANS/tANS_generate_symbol_distribution.R")

tANS_example1 <-function(test_input_string){
        coding_table_example <- rbind(
             data.frame(symbol = "A", input_state = c(3:5), output_state = c(8, 11, 14)),
             data.frame(symbol = "B", input_state = c(3:5), output_state = c(9, 12, 15)),
             data.frame(symbol = "C", input_state = c(2:3), output_state = c(10, 13))
        )
        
        tANS_coder_state_example <- list(state = 8, L = 8, b = 2, output_bits = bit(0))
        
        tANS_coder_state_output <- tANS_coding(tANS_coder_state_example, coding_table_example, test_input_string)
        n_output_bits <- length(tANS_coder_state_output$output_bits)
        
        tANS_decoder_state_example <- list(
             state = tANS_coder_state_output$state, 
             b = tANS_coder_state_output$b, 
             input_bits = as.bit(c(tANS_coder_state_output$output_bits[1:n_output_bits]))
        )
        
        tANS_decoder_state_output <- tANS_decoding(tANS_decoder_state_example, coding_table_example)
        output_test_string <- tANS_decoder_state_output$output_string
        return(output_test_string[length(output_test_string):1])
}
                
tANS_example2 <- function(test_input_string){
        symbol_distribution = tANS_generate_normal_symbol_distribution(64,0,4096,0,10,20)
        coding_table = tANS_coding_table_generate_basic(symbol_distribution)
        
        tANS_coder_state <- list(state = 4096, L = 4096, b = 2, output_bits = bit(0))
        tANS_coder_output <- tANS_coding(tANS_coder_state, coding_table, test_input_string)
        n_output_bits <- length(tANS_coder_output$output_bits)
        
        tANS_decoder_state <- list(
                state = tANS_coder_output$state, 
                b = tANS_coder_output$b, 
                input_bits = as.bit(c(tANS_coder_output$output_bits[1:n_output_bits]))
        )
        
        tANS_decoder_output <- tANS_decoding(tANS_decoder_state, coding_table)
        output <- tANS_decoder_output$output_string
        return(output[length(output):1])
}

tANS_w_normal_distribution <- function(data, set_size){
        if(missing(set_size)){
                symbol_distribution = tANS_generate_normal_symbol_distribution(255,0,2048,0,sd(data),20)
                coding_table = tANS_coding_table_generate_basic(symbol_distribution)
                
                tANS_coder_state <- list(state = 2048, L = 2048, b = 2, output_bits = bit(0))
                tANS_coder_output <- tANS_coding(tANS_coder_state, coding_table, data)
                n_output_bits <- length(tANS_coder_output$output_bits)
                return(n_output_bits)
        }
        else{
                n_output_bits <- 0
                for(i in seq(1,length(data),set_size)){
                        symbol_distribution = tANS_generate_normal_symbol_distribution(255,0,2048,0,sd(data[i:(i+set_size-1)]),20)
                        coding_table = tANS_coding_table_generate_basic(symbol_distribution)
                
                        tANS_coder_state <- list(state = 2048, L = 2048, b = 2, output_bits = bit(0))
                        tANS_coder_output <- tANS_coding(tANS_coder_state, coding_table, data[i:(i+set_size-1)])
                        n_output_bits <- n_output_bits + length(tANS_coder_output$output_bits)
                }
                return(n_output_bits)
        }
}

tANS_w_zero_based_distribution <- function(data, set_size){
        if(missing(set_size)){
                symbol_distribution = tANS_generate_zero_based_symbol_distribution(data, 2048)
                coding_table = tANS_coding_table_generate_basic(symbol_distribution)
                
                tANS_coder_state <- list(state = 2048, L = 2048, b = 2, output_bits = bit(0))
                tANS_coder_output <- tANS_coding(tANS_coder_state, coding_table, data)
                n_output_bits <- length(tANS_coder_output$output_bits)
                return(n_output_bits)
        }else{
                n_output_bits <- 0
                for(i in seq(1,length(data),set_size)){
                        symbol_distribution = tANS_generate_zero_based_symbol_distribution(data[i:(i+set_size-1)], 2048)
                        coding_table = tANS_coding_table_generate_basic(symbol_distribution)
                        tANS_coder_state <- list(state = 2048, L = 2048, b = 2, output_bits = bit(0))
                        tANS_coder_output <- tANS_coding(tANS_coder_state, coding_table, data[i:(i+set_size-1)])
                        n_output_bits <- n_output_bits + length(tANS_coder_output$output_bits)
                }
                return(n_output_bits)
                
        }
}