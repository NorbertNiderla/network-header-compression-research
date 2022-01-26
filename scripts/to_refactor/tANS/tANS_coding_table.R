source("tANS/tANS.R")

rANS_sequence_string <- function(symbol_distribution){
     sequence_string <- c()
     
     sd_order_idx <- order(-symbol_distribution$count)
     for(idx in sd_order_idx){
          s <- symbol_distribution$symbol[idx]
          F_s <- symbol_distribution$count[idx]
          sequence_string <- c(
               sequence_string,
               rep(s, F_s)
          )
     }
     
     return(sequence_string)
}

# INPUT: "symbol_distribution" where "$symbol" is the symbol, "$count" is the count, sum of "$count" should be a power of 2
tANS_coding_table_generate_basic <- function(symbol_distribution){
     out <- data.frame()
     sd_order_idx <- order(-symbol_distribution$count)
     I <- sum(symbol_distribution$count)
     for(idx in sd_order_idx){
          s <- symbol_distribution$symbol[idx]
          F_s <- symbol_distribution$count[idx]
          new_out <- data.frame(
               symbol = s,
               input_state = c(F_s:(2*F_s - 1)),
               output_state = c(I:(I + F_s - 1))
          )
          out <- rbind(out, new_out)
          I <- I + F_s
     }
     
     return(out)
}

# "sequence_string" a vector of symbols
tANS_coding_table_from_string <- function(sequence_string){
     out <- data.frame()
     I <- length(sequence_string)
     symbol_distribution <- as.data.frame(table(sequence_string))
     names(symbol_distribution) <- c("symbol", "count")
     symbol_distribution$I_s <- symbol_distribution$count
     
     for(s in sequence_string){
          symbol_distribution_idx <- which(symbol_distribution$symbol == s)
          I_s <- symbol_distribution$I_s[symbol_distribution_idx]
          new_out <- data.frame(
               symbol = s,
               input_state = I_s,
               output_state = I
          )
          out <- rbind(out, new_out)
          I <- I + 1
          symbol_distribution$I_s[symbol_distribution_idx] <- I_s + 1
     }
     
     return(out)
}

tANS_coding_table_stationary_distribution <- function(coding_table){
     states <- unique(coding_table$output_state)
     symbols <- unique(coding_table$symbol)
     n_states <- length(states)
     
     # Probability distribution of state transitions
     pd_st <- matrix(data = 0, nrow = n_states, ncol = n_states)
     
     # Calculating the input/output transition matrix
     for(x in states){
          for(s in symbols){
               tANS_coder_state <- list(state = x, L = n_states, b = 2)
               new_tANS_coder_state <- tANS_coding_symbol(tANS_coder_state, coding_table, s)
               x_p <- new_tANS_coder_state$state
               i <- x - n_states + 1
               j <- x_p - n_states + 1
               pd_st[i,j] <- pd_st[i,j] + 1
          }
     }
     
     # Normalizing the matrix
     for(i in 1:n_states){
          pd_st[i,] <- pd_st[i,] / sum(pd_st[i,])
     }

     pd_st_transpose <- t(pd_st)

     # Eigenvector
     ev <- eigen(pd_st_transpose)
     ev_values <- round(ev$values, 10)
     idx <- which(ev_values == 1)
     
     out <- ev$vectors[,idx]
     out <- out / sum(out)
     
     return(out)
}

