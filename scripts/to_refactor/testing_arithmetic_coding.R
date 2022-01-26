setwd("/home/norbert/git/network-header-compression/R/")
source("./frame_generation.R")
source("./markov.R")
source("./arithmetic_coding.R")
source("./context_arithmetic_coding.R")


context_prob_0_counts <- function(stream, order){
        context <- 0
        prob_0_count <- vector(mode = "numeric", length = 2^order)
        prob_1_count <- vector(mode = "numeric", length = 2^order)
        for(bit in stream){
                if(bit == 0){
                        prob_0_count[context + 1] <- prob_0_count[context + 1] + 1 
                } else if(bit == 1) {
                        prob_1_count[context + 1] <- prob_1_count[context + 1] + 1
                }
                context <- context%%(2^(order-1))*2 + bit       
        }
        prob_0 <- prob_0_count/(prob_0_count+prob_1_count)
        return(prob_0)
}

c <- second_order_markov()
counts_abs <- round(c$probability * 1000)
counts_markov <- round(c$markov * 1000)

data_bit <- c()
data_int <- c()
for (x in 1:10000) {
        data_bit <- c(data_bit, as.numeric(generate_coap_header_example()))
}

for(x in seq(from=1, by = 2, to=length(data_bit))){
        data_int <- c(data_int, data_bit[x]*2 + data_bit[x+1])
}

t_abs <- arithmetic_encode(data_int, counts_abs)
t_con <- context_arithmetic_encode(data_bit, counts_markov)
t_bin <- context_binary_arithmetic_encode(data_bit, context_prob_0_counts(data_bit, 1), 1)

length(t_abs)
length(t_con)
length(t_bin)


byte_xor <- function(stream){
        if(length(stream)%%8!=0){
                stop("byte_xor: stream is not byte alligned")
        }
        
        out <- c()
        for(x in seq(from = 1, to = length(stream)-8, by = 8)){
                out <- c(out, bitwXor(stream[x:(x+7)], stream[(x+8):(x+15)])) 
        }
        out <- c(out, stream[(length(stream)-7):length(stream)])

        return(out)
}