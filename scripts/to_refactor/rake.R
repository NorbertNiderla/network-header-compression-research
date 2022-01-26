rake_int_encode <- function(stream, t){
        if(!is.bit(stream)){
                stop("rake_encode: Input data is not bit vector.")
        } else if(length(stream)<16){
                stop("rake_ecode: Input vector is not long enough.")
        }
        
        l <- length(stream)
        pos <- 1
        out <- c()
        x <- 0
        while(pos <= l){
                if(stream[pos]==FALSE){
                        x <- x + 1
                        pos <- pos + 1
                }else{ #stream[pos]==1
                       out <- c(out, x)
                        x <- 0
                        pos <- pos + 1
                }
                
                if(x == t){
                        out <- c(out, x)
                        x <- 0
                }
        }
        
        return(out)
}