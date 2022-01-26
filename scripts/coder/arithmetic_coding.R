arithmetic_encode_main_loop <- function(high, low, scale){
      top_value <- 2147483647
      first_qtr <- 536870912
      half <- 1073741824
      third_qtr <- 1610612736
      
      out <- c()
      
      while((high < half) | (low >= half)) {
            if( high < half ){
                  low <- low*2
                  high <-high*2 + 1
                  out <- c(out, 0)
                  while(scale > 0){
                        out <- c(out, 1)
                        scale <- scale - 1
                  }
            } else if( low >= half ) {
                  low = ( low - half ) * 2
                  high = ( high - half ) * 2 + 1
                  out <- c(out, 1)
                  while(scale > 0){
                        out <- c(out, 0)
                        scale <- scale - 1
                  }
            }
      }
      
      while( ( low >= first_qtr ) & ( high < third_qtr ) ){
            scale <- scale + 1
            low = ( low - first_qtr ) * 2
            high = (( high - first_qtr ) * 2) + 1
      }
      
      return(list("high" = high, "low" = low, "scale" = scale, "out" = out))
}

arithmetic_encode_ending_loop <- function(high, low, scale){
      top_value <- 2147483647
      first_qtr <- 536870912
      half <- 1073741824
      third_qtr <- 1610612736
      out <- c()
      
      if( low < first_qtr ) {
            out <- c(out, 0)
            while(scale >= 0){
                  out <- c(out, 1)
                  scale <- scale - 1
            }
      } else {
            out <- c(out, 1)
      }
      
      return(list("high" = high, "low" = low, "scale" = scale, "out" = out))
}

arithmetic_decode_main_loop <- function(high, low, value, stream, stream_idx){
      top_value <- 2147483647
      first_qtr <- 536870912
      half <- 1073741824
      third_qtr <- 1610612736
      idx <- stream_idx
      
      while((high < half) | (low >= half)) {
            if( high < half ){
                  low <- low * 2
                  high <- high * 2 + 1
                  value <- value * 2 + stream[idx]
                  idx <- idx + 1
            } else if( low >= half ) {
                  low = ( low - half )*2
                  high = ( high - half )*2+1
                  value <- (value - half)*2 + stream[idx]
                  idx <- idx + 1
            }
      }
      
      while( ( low >= first_qtr ) & ( high < third_qtr ) ){
            low = ( low - first_qtr ) *2
            high = ( high - first_qtr )*2+1
            value <- (value - first_qtr) *2 + stream[idx]
            idx <- idx + 1
      }
      
      return(list("high" = high, "low" = low, "value" = value, "idx" = idx))
}