source("./scripts/to_refactor/own_bit_library.R")

get_coap_data <- function(){
        n = scan("./source/coap_dataset.txt", what=double())
        header <- list()
        payload <- list()
        i <- 1
        header_i <- 1
        payload_i <- 0
        last_payload_i <-0 
        while(i < length(n)){
                if(n[i] == 85){
                        header[[header_i]] <- n[i:(i+8)]
                        header_i <- header_i + 1
                        i <- i + 9
                        payload_i <- payload_i + 1
                } else {
                        if(last_payload_i != payload_i){
                                payload[[payload_i]] <- n[i]
                                last_payload_i <- payload_i
                        } else {
                                payload[[payload_i]] <- c(payload[[payload_i]], n[i])
                        }
                        i <- i + 1
                }
        }
        
        header_bit <- list()
        for(x in 1:19){
                header_bit[[x]] <- int_vector_to_bit(header[[x]], 8)
        }
        
        payload_bit <- list()
        for(x in 1:19){
                payload_bit[[x]] <- int_vector_to_bit(payload[[x]], 8)
        }
        
        return(list(header_bit, payload_bit))
}

get_coap_header <- function(){
        n = scan("./source/ring_coap_raw_data.txt", what=double())
        header <- list()
        i <- 1
        for(x in seq(from=1,by=4,to=length(n))){
                header[[i]] <- n[x:(x+3)]
                i <- i + 1
        }
        
        header_bit <- list()
        for(x in 1:length(header)){
                header_bit[[x]] <- as.double(int_vector_to_bit(header[[x]], 8))
        }
        
        return(header_bit)
}
