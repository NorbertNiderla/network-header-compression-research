source("./scripts/tools/bit_operations.R")

#data is array of 8 8-bit symbols
bpc_encode <- function(data, bit_width, debug = FALSE){
   if(debug == TRUE){
      browser()
   }
      size = length(data)
      
      data = c(data[1],diff(data))
      
      data[2:length(data)] = zigzag(data[2:length(data)])
      data = c(data[1],transpose_bit_matrix(data[2:length(data)], bit_width+1))
      
      for(x in seq(from = length(data), to = 3, by = -1)){
            data[x] = bitwXor(data[x], data[x-1])
      }
      
      return(data)
}

bpc_decode <- function(data, bit_width, original_size, debug = FALSE){
      if(debug == TRUE){
            browser()
      }
      
      for(x in seq(from = 3, to = length(data), by = 1)){
            data[x] = bitwXor(data[x-1], data[x])
      }
      
      data = c(data[1], transpose_bit_matrix(data[2:length(data)], original_size - 1))
      data[2:length(data)] = zigzag_decode(data[2:length(data)])
      
      return(cumsum(data))
}

#data is 32-bit integers
bp_encode_calc <-function(data){
      n_bits = 0
      for(x in 1:length(data)){
         if(data[x] == 0){
            n_bits <- n_bits + 2
         } else if(sum(as.numeric(int_to_bit(data[x],32))) == 1) { #to daje ostrzezenia
            n_bits <- n_bits + 2
         }else{
            n_bits <- n_bits + 33
         }
      }
      return(n_bits)
}
