
# 'lsbf': set to TRUE if the first position in the vector 'bits' denotes the least significant bit of the represented integer
bitToInt <- function(bits, lsbf = TRUE){
     l <- length(bits)
     if(lsbf == TRUE)
          powers <- 2^(c(0:(l-1)))
     else
          powers <- 2^(c((l-1):0))                
     out <- 0
     for (x in 1:length(powers)){
             out <- out + powers[x]*as.numeric(bits[x]) 
     }
     return(sum(out))
}

intToBit <- function(number, noBits) {
        binary_vector = rev(as.numeric(intToBits(number)))
        if(missing(noBits)) {
                return(binary_vector)
        } else {
                binary_vector[-(1:(length(binary_vector) - noBits))]
        }
}