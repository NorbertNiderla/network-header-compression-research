entropy <- function(probability){
      en <- -sum(probability*log2(probability))
      return(en)
}

get_total_entropy <- function(count_matrix){
      r <- nrow(count_matrix)
      c <- ncol(count_matrix)
      e <- 0
      for(r in 1:nrow(count_matrix)){
            row <- count_matrix[r,]
            row <- row / sum(row)
            e <- e + entropy(row)
      }
      return(e)
}

get_standard_model_entropy <- function(count){
      count <- count / sum(count)
      en <- entropy(count)
      return(en)
}

get_markov_chain_entropy <- function(data){
      markov <- data$markov
      count <- data$count
      
      for(x in seq(from=1,by=2,to=length(markov))){
            markov[x:(x+1)] <- markov[x:(x+1)] / sum(markov[x:(x+1)])
      }
      
      count <- count / sum(count)
      
      markov <- matrix(data=markov, ncol=2)
      e <- 0
      
      for(x in 1:nrow(markov)){
            e <- e + count[x]*entropy(markov[x,])
      }
      
      return(e)
}



