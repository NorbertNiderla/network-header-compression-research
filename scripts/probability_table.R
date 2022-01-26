get_probability_table_normal <- function(size, mean, sigma){
      out <- rnorm(size, mean, sigma)
      out[out<=0.01] <- 0.01
      out[out>= 0.99] <- 0.99
      
      return(out)
}
