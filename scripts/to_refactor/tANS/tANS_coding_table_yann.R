source("coders/ANS/tANS_coding_table.R")

# Adapted from the "simpler sorts" procedure described in C++ language at
# http://cbloomrants.blogspot.com/2014/02/02-06-14-understanding-ans-8.html
tANS_coding_table_yann <- function(symbol_distribution, sort_count = NA){
     F <- sum(symbol_distribution$count)
     output_string <- rep(NA, F)
     
     if(is.na(sort_count)){
          sort_count <- F
     }
     
     fse_step <- (sort_count/2) + (sort_count/8) + 1;
     # Quick-fix: if step_count is 8, then fse_step is even
     if((fse_step %% 2) == 0)
          fse_step <- fse_step - 1
     # End quick-fix
     fse_pos <- 0
     
     for(idx in 1:nrow(symbol_distribution)){
          s <- symbol_distribution$symbol[idx]
          count <- symbol_distribution$count[idx]
          for(c in 1:count){
               output_string[fse_pos + 1] <- s
               fse_pos <- (fse_pos + fse_step) %% sort_count;
          }
     }
     
     return(output_string)
}