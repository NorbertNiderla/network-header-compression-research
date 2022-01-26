xor <- function(stream, s){
        if(length(stream)%%s!=0){
                stop("xor: stream is not byte alligned")
        }
        
        out <- c()
        for(x in seq(from = 1, to = length(stream)-s, by = s)){
                out <- c(out, bitwXor(stream[x:(x+s-1)], stream[(x+s):(x+s+s-1)])) 
        }
        out <- c(out, stream[(length(stream)-s+1):length(stream)])
        
        return(as.numeric(out))
}

position_transform <- function(stream){
        out <- c(stream[seq(from=1,to=length(stream),by=2)],stream[seq(from=2,to=length(stream),by=2)])
        return(out)
}

calc_eff <- function(p0){
        p0[p0 < 0.5] <- 1 - p0[p0 < 0.5]
        return(sum(p0)/length(p0))
}
source("./frame_generation.R")

test <- function(xor_depth, markov_order){
        stream <- c()
        stream_transformed <- c()
        for(x in 1:100){
                f <- as.numeric(generate_coap_header_example())
                stream <- c(stream, f)
                stream_transformed <- c(stream_transformed, (position_transform(f)))
        }
        
        p0 <- context_prob_0_counts(stream,3)
        p0_transformed <- context_prob_0_counts(stream_transformed, 3)
        
        eff_p0 <- calc_eff(p0)
        eff_p0_transformed <- calc_eff(p0_transformed)
        return(list("eff_p0"=eff_p0, "eff_p0_transformed"=eff_p0_transformed))
}

test_dec <- function(xor_depth, markov_order){
        r <- test(xor_depth, markov_order)
        if(r$eff_p0 < r$eff_p0_transformed){
                print(TRUE)
        } else {
                print(FALSE)
        }
}
