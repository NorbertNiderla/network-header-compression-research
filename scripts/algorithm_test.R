source("./scripts/probability_table.R")
source("./scripts/coder/position_context_binary_arithmetic_coding.R")
source("./scripts/figures.R")

get_random_stream <- function(prob_table){
      size <- length(prob_table)
      out <- integer(size)
      for(x in 1:size){
            out[x] <- sample(c(0,1),
                        size=1,
                        replace=TRUE,
                        prob=c(prob_table[x], 1-prob_table[x]))
      }
      
      return(out)
}

algorithm_test <- function(input_size, prob_mean, prob_sigma) {
      stream_size = input_size
      mean = prob_mean
      sigma = prob_sigma
        
      prob_table <- get_probability_table_normal(stream_size, mean, sigma)
      stream <- get_random_stream(prob_table)
      t <- position_context_binary_arithmetic_encode(stream, prob_table)
      dd <- position_context_binary_arithmetic_decode(t, prob_table, stream_size)
        
      test = identical(stream, dd)
      stopifnot(test == TRUE)
        
      return(length(t))    
}

mean <- seq(from = 0.01, to = 0.99, by = 0.02)
d <- data.frame()
lp <- 1
while(lp < 30){
      for(x in mean){
            d <- rbind(d, data.frame(algorithm_test(160, x, 0.2) / 160, x)) 
      }
      lp <- lp + 1
}

colnames(d) <- c("ratio", "mean")
p <- figure_algorithm_test_results(d, save = TRUE)
print(p)
