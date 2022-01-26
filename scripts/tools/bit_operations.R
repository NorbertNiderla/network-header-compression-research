library(bit)

int_to_bit <-function(int, n_bits){
            return(as.bit(tail(rev(as.integer(intToBits(int))),n_bits)))      
}

bit_to_int <- function(bits, lsbf = FALSE){
      l <- length(bits)
      if(lsbf == TRUE)
            powers <- 2^(c(0:(l-1)))
      else
            powers <- 2^(c((l-1):0))                
      return(sum(powers*as.numeric(bits)))
}

bit_vector_to_int <- function(bits, bit_width){
      ret = c()
      for(x in seq(from = 1, to = length(bits), by = bit_width)){
            ret = c(ret, bit_to_int(bits[x:(x+bit_width-1)]))
      }
      return(ret)
}

int_vector_to_bit <- function(ints, bit_width){
   ret = bit(0)
   for(x in 1:length(ints)){
      ret = c(ret, int_to_bit(ints[x], bit_width))
   }
   return(ret)
}

zeros_volume <- function(bit_stream){
   size = length(bit_stream)
   zr = 0;
   for(x in 1:size){
      if(bit_stream[x]==FALSE){
         zr <- zr + 1
      }
   }
   return(zr/size)
}

transpose_bit_matrix <- function(data, bit_width){
   
   mat = matrix(ncol=bit_width, nrow = length(data))
      for(x in 1:length(data)){
            mat[x,] = tail(rev(as.integer(intToBits(data[x]))),bit_width)
      }
      
      mat = t(mat)
      ret = c()
      for(x in 1:bit_width){
            ret[x] = bit_to_int(mat[x,],length(data))
      }
      
      return(ret)
}

zigzag <- function(data){
   return(abs(data)*2 - (data < 0))
}

zigzag_decode <- function(data){
   data[data%%2==1] <- (data[data%%2==1]+1)/(-2)
   data[data>=0] <- data[data>=0]/2
   return(data)
}
