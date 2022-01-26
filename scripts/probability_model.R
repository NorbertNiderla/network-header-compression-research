source("./scripts/coap_data.R")
source("./scripts/to_refactor/own_bit_library.R")
library(ggpubr)

get_coap_position_probability_model_2 <- function(){
      coap_header <- get_coap_header()
      count_0 <- vector(mode="numeric", length = 32)
      count_1 <- vector(mode="numeric", length = 32)
      
      for (x in coap_header) {
            for(i in 1:length(x)){
                  if(x[i]==0){
                        count_0[i] <- count_0[i] + 1
                  }else {
                        count_1[i] <- count_1[i] + 1
                  }
            }
      }
      
      count_0[count_0==0] <- 1
      count_1[count_1==0] <- 1
      prob_0 <- count_0/(count_0+count_1)
      
      m <- matrix(nrow=32, ncol=2)
      m[,1] <- count_0
      m[,2] <- count_1
      return(m)
}

get_coap_position_probability_model_4 <- function(){
      coap_header <- get_coap_header()
      m <- matrix(nrow=16, ncol=4, data = 1)
      for (x in coap_header) {
            k <- 1
            for(i in seq(from=1,by=2,to=length(x))){
                  val <- bit_to_int(x[i:(i+1)])
                  m[k,val] <- m[k,val] + 1
                  k <- k + 1
            }
      }
      
      return(m)
}

get_coap_position_probability_model_16 <- function(){
      coap_header <- get_coap_header()
      m <- matrix(nrow=8, ncol=16, data=1)
      for (x in coap_header) {
            k <- 1
            for(i in seq(from=1,by=4,to=length(x))){
                  val <- bit_to_int(x[i:(i+3)])
                  m[k,val] <- m[k,val] + 1
                  k <- k + 1
            }
      }
      
      return(m)
}

get_coap_position_probability_model_256 <- function(){
      coap_header <- get_coap_header()
      count <- m <- matrix(nrow=4, ncol=256, data=1)
      for (x in coap_header) {
            k <- 1
            for(i in seq(from=1,by=8,to=length(x))){
                  val <- bit_to_int(x[i:(i+7)])
                  m[k,val] <- m[k,val] + 1
                  k <- k + 1
            }
      }
      
      return(m)
}

first_order_markov <- function(){
      coap_header <- get_coap_header() 
      markov <- vector(mode = "numeric", length = 4)
      count <- vector(mode = "numeric", length = 2)
      order <- 1
      context <- 0
      
      for(x in coap_header){
            k <- 1
            for(i in seq(from=1, to=length(x))){
                  val <- bit_to_int(x[i])
                  k <- k + 1
                  markov[context*2 + val + 1] <- markov[context*2 + val + 1] + 1
                  context <- val
                  count[val + 1] <- count[val + 1] + 1
            }
      }
      
      return(list("markov"=markov, "count"=count))
}

second_order_markov <- function(){
      coap_header <- get_coap_header() 
      markov <- vector(mode = "numeric", length = 8)
      count <- vector(mode = "numeric", length = 4)
      order <- 1
      context <- 0
      
      for(x in coap_header){
            k <- 1
            for(i in seq(from=1, to=length(x))){
                  val <- bit_to_int(x[i])
                  k <- k + 1
                  markov[context*2 + val + 1] <- markov[context*2 + val + 1] + 1
                  context <- (context%%2)*2 +  val
                  count[context + 1] <- count[context + 1] + 1
            }
      }
      
      return(list("markov"=markov, "count"=count))
}

third_order_markov <- function(){
      coap_header <- get_coap_header() 
      markov <- vector(mode = "numeric", length = 16)
      count <- vector(mode = "numeric", length = 8)
      order <- 1
      context <- 0
      
      for(x in coap_header){
            k <- 1
            for(i in seq(from=1, to=length(x))){
                  val <- bit_to_int(x[i])
                  k <- k + 1
                  markov[context*2 + val + 1] <- markov[context*2 + val + 1] + 1
                  context <- (context%%4)*2 +  val
                  count[context + 1] <- count[context + 1] + 1
            }
      }
      
      return(list("markov"=markov, "count"=count))
}

fourth_order_markov <- function(){
      coap_header <- get_coap_header() 
      markov <- vector(mode = "numeric", length = 32)
      count <- vector(mode = "numeric", length = 16)
      order <- 1
      context <- 0
      
      for(x in coap_header){
            k <- 1
            for(i in seq(from=1, to=length(x))){
                  val <- bit_to_int(x[i])
                  k <- k + 1
                  markov[context*2 + val + 1] <- markov[context*2 + val + 1] + 1
                  context <- (context%%8)*2 +  val
                  count[context + 1] <- count[context + 1] + 1
            }
      }
      
      return(list("markov"=markov, "count"=count))
}

fifth_order_markov <- function(){
      coap_header <- get_coap_header() 
      markov <- vector(mode = "numeric", length = 64)
      count <- vector(mode = "numeric", length = 32)
      order <- 1
      context <- 0
      
      for(x in coap_header){
            k <- 1
            for(i in seq(from=1, to=length(x))){
                  val <- bit_to_int(x[i])
                  k <- k + 1
                  markov[context*2 + val + 1] <- markov[context*2 + val + 1] + 1
                  context <- (context%%16)*2 +  val
                  count[context + 1] <- count[context + 1] + 1
            }
      }
      
      return(list("markov"=markov, "count"=count))
}

standard_model <- function(bitwidth){
      coap_header <- get_coap_header()
      count <- vector(mode="numeric",length=2^bitwidth)
      
      for(x in coap_header){
            for(i in seq(from=1,by=bitwidth,to=length(x))){
                  val <- bit_to_int(x[i:(i+(bitwidth-1))])
                  count[val + 1] <- count[val + 1] + 1
            }
      }
      
      return(count)
}
