source("./scripts/spec.R")
source("./scripts/results.R")

calc_energy_ratio <- function(time, bits_in, bits_out){
      spec <- get_slwstk6220a_spec()
      r <- (time*spec$p_cpu + bits_out/spec$bitrate * spec$p_tx)/(bits_in*spec$p_tx)*spec$bitrate
      return(r)
}

get_energy_ratio <- function(){
      data <- get_all_results()
      data$size_in <- data$size_in * 8
      data$time <- data$time / 1000000
      
      data["energy_ratio"] <- calc_energy_ratio(data$time, data$size_in, data$size_out_bits)
      
      return(data)
}
