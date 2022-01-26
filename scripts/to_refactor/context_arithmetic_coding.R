#second order
#input data:
#data - integer 0/1 stream
#counts - 8 counts of events
context_arithmetic_encode <- function(data, counts){
        top_value <- 2147483647
        first_qtr <- 536870912
        half <- 1073741824
        third_qtr <- 1610612736
        
        high <- top_value
        low <- 0
        scale <- 0
        out <- c()
        
        bounds_0 <- c(0, cumsum(counts[1:2]))
        bounds_1 <- c(0, cumsum(counts[3:4]))
        bounds_2 <- c(0, cumsum(counts[5:6]))
        bounds_3 <- c(0, cumsum(counts[7:8]))
        context <- 0
        
        for(i in 1:length(data)){
                if(context == 0){
                        bounds <- bounds_0
                } else if(context == 1){
                        bounds <- bounds_1
                } else if(context == 2){
                        bounds <- bounds_2
                } else if(context == 3){
                        bounds <- bounds_3
                }
                step = ceiling((high - low + 1) / bounds[length(bounds)])
                high = low + (step * bounds[context*2 + data[i] + 2]) - 1
                low = low + (step * bounds[context*2 + data[i]+1])
                context <- (context%%2)*2 +  data[i]
                
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
        }
        
        if( low < first_qtr ) {
                out <- c(out, 0)
                while(scale >= 0){
                        out <- c(out, 1)
                        scale <- scale - 1
                }
        } else {
                out <- c(out, 1)
        }
        
        return(out);
}

decode_symbol <- function(cum, cum_count){
        if(cum == 0){
                symbol <- 0
        } else {
                symbol <- -999
                for(x in 1:(length(cum_count)-1)){
                        if(cum_count[x]<cum & cum_count[x+1]>=cum){
                                symbol <- x - 1
                                break
                        }
                }
        }
        if(symbol %% 2 == 0){
                return(0)
        } else {
                return(1)
        }
}

context_arithmetic_decode <-function(stream, counts, size){
        top_value <- 2147483647
        first_qtr <- 536870912
        half <- 1073741824
        third_qtr <- 1610612736
        
        low <- 0
        high <- top_value
        value <- 0
        idx <- 1
        stream <- c(stream, integer(30))
        for(x in 1:31){
                value <- value * 2 + stream[idx]
                idx <- idx + 1
        }
        
        bounds_0 <- c(0, cumsum(counts[1:2]))
        bounds_1 <- c(0, cumsum(counts[3:4]))
        bounds_2 <- c(0, cumsum(counts[5:6]))
        bounds_3 <- c(0, cumsum(counts[7:8]))
        context <- 0
        data <- c()
        
        for(i in 1:size){
                if(context == 0){
                        bounds <- bounds_0
                } else if(context == 1){
                        bounds <- bounds_1
                } else if(context == 2){
                        bounds <- bounds_2
                } else if(context == 3){
                        bounds <- bounds_3
                }
                step <- ceiling((high - low + 1)/bounds[length(bounds)])
                cum <- ceiling((value - low) / step)
                data[i] = decode_symbol(cum, bounds)
                
                high = low + (step * bounds[context*2 + data[i] + 2]) - 1
                low = low + (step * bounds[context*2 + data[i] + 1])
                context <- (context%%2)*2 +  data[i]
                
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
        }
        return(data)
}

