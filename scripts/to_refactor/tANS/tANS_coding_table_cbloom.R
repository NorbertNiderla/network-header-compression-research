source("coders/ANS/tANS_coding_table.R")

# URL: http://cbloomrants.blogspot.com/2014/02/02-06-14-understanding-ans-8.html
# One very simple way to get a decent sort is to bit-reverse the rANS indexes. 
# That is, start from a rANS/alphabetical order string ("AABB..") and take the index of each element, bit-reverse that index (so 0001 -> 1000), 
# and put the symbol in the bit reversed slot. While this is not competitive with the proper sort, it is simple and one pass.
tANS_coding_table_cbloom_bit_reverse <- function(symbol_distribution){
        F <- sum(symbol_distribution$count)
        max_bit_lenght <- floor(log2(F - 1)) + 1
        
        input_string <- rANS_sequence_string(symbol_distribution)
        output_string <- rep(NA, F)
        
        for(i in 0:(F-1)){
                bit_vector <- as.integer(intToBits(i)[1:max_bit_lenght])
                reversed_val <- sum(bit_vector * 2^c((max_bit_lenght - 1):0))
                output_string[reversed_val + 1] <- input_string[i + 1]
        }
        
        return(output_string)
}

# URL: http://cbloomrants.blogspot.com/2014/02/02-06-14-understanding-ans-8.html
# One way to solve this problem is to start assigning slots, and when you see that one is full you just look in the neighbor slot, etc.
tANS_coding_table_cbloom_greedy_search <- function(symbol_distribution){
        sort_count <- sum(symbol_distribution$count)
        
        output_string <- rep(NA, sort_count)
        
        for(idx in 1:nrow(symbol_distribution)){
                s <- symbol_distribution$symbol[idx]
                count <- symbol_distribution$count[idx]
                step <- (sort_count + (count/2) ) / count;
                first <- step/2;
                for(c in 1:count){
                        # Quick-fix: first slot was out of bounds
                        slot <- (first + step * c) %% sort_count;
                        # end quick-fix
                        
                        # find an empty slot :
                        while(TRUE) {
                                if (is.na(output_string[slot + 1]) == TRUE) {
                                        output_string[slot + 1] <- s
                                        break;
                                }
                                slot = (slot + 1) %% sort_count;
                        }
                }
        }
        
        return(output_string)
}