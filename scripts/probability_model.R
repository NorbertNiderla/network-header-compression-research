source("./scripts/tools/bit_operations.R")
library(ggpubr)

get_position_probability_model <- function(data, symbol_bitwidth){
   m <- matrix(ncol = 2^symbol_bitwidth,
               nrow = ceiling(max(lengths(data))/symbol_bitwidth),
               data = 1)
   for(x in data) {
      k <- 1
      for(i in seq(from=1,by=symbol_bitwidth,to=length(x))){
         val <- bit_to_int(x[i:(i+symbol_bitwidth-1)])
         m[k,val + 1] <- m[k,val + 1] + 1
         k <- k + 1
      }
   }
   return(m)
}

first_order_markov <- function(data){
      markov <- vector(mode = "numeric", length = 4)
      count <- vector(mode = "numeric", length = 2)
      order <- 1
      context <- 0
      
      for(x in data){
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

second_order_markov <- function(data){
      markov <- vector(mode = "numeric", length = 8)
      count <- vector(mode = "numeric", length = 4)
      order <- 1
      context <- 0
      
      for(x in data){
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

third_order_markov <- function(data){
      markov <- vector(mode = "numeric", length = 16)
      count <- vector(mode = "numeric", length = 8)
      order <- 1
      context <- 0
      
      for(x in data){
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

fourth_order_markov <- function(data){
      markov <- vector(mode = "numeric", length = 32)
      count <- vector(mode = "numeric", length = 16)
      order <- 1
      context <- 0
      
      for(x in data){
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

fifth_order_markov <- function(data){
      markov <- vector(mode = "numeric", length = 64)
      count <- vector(mode = "numeric", length = 32)
      order <- 1
      context <- 0
      
      for(x in data){
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

standard_model <- function(data, symbol_bitwidth){
      count <- vector(mode="numeric",length=2^symbol_bitwidth)
      for(x in data){
            for(i in seq(from=1,by=symbol_bitwidth,to=length(x))){
                  val <- bit_to_int(x[i:(i+symbol_bitwidth-1)])
                  count[val + 1] <- count[val + 1] + 1
            }
      }
      
      return(count)
}
