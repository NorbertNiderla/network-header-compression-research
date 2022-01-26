#sum of "count" should be power of two
tANS_generate_normal_symbol_distribution<-function(max_symbol, min_symbol, L, mu=0,sigma, num_of_sections=20){
  if(sigma==0){sigma=0.001} #when sigma=0, program is bugging
  x = min_symbol:max_symbol
  x_s = seq(min_symbol-0.5,max_symbol+0.5,1/num_of_sections)
  
  pdf = dnorm(x_s,mu,sigma)
  pdf = pdf*((L-length(x))/sum(pdf))+1/num_of_sections
  
  count = vector("integer",length(x))
  count_s = vector("numeric",length(x))
  
  for(i in 1:length(x)){count_s[i] = sum(pdf[((i-1)*num_of_sections+1):(i*num_of_sections)])}
  count = round(count_s)
  
  missing_count = L - sum(count)
  symbol_distribution <- data.frame("symbol" = x, "count"=count, "error"=count_s-count)
  
  while(missing_count!=0){
    if(missing_count>0){
      symbol_distribution<-symbol_distribution[order(symbol_distribution$error, decreasing = "TRUE"),]
      symbol_distribution$count[1] = symbol_distribution$count[1] + 1
      symbol_distribution$error[1] = symbol_distribution$error[1] - 1 
      missing_count = missing_count - 1
    }else if(missing_count<0){
      symbol_distribution<-symbol_distribution[order(symbol_distribution$error),]
      i = 1
      while(1){
        if(symbol_distribution$count[i]>1){
          symbol_distribution$count[i] = symbol_distribution$count[i] - 1
          symbol_distribution$error[i] = symbol_distribution$error[i] + 1
          missing_count = missing_count + 1
          break
        } else {i = i+1}
      }
    }
  }
  symbol_distribution<-symbol_distribution[-3]
  symbol_distribution<-symbol_distribution[order(symbol_distribution$symbol),]
  
  return(symbol_distribution)
}

tANS_generate_zero_based_symbol_distribution <- function(data, L){
    count = vector(mode = "numeric", length = 256) 
    t = table(data)
    dens = as.numeric(t[1])/as.numeric(sum(t))
    if(dens==1){
      count[1] = 10000
    }
    else if(dens==0){
      count[1] = 1;
    }
    else{
      count[1] = -dens*(250+5*3)/(dens-1);
    }
    
    coeff <- sum(count)/L
    count <- count/coeff
    count[count<1] <- 1
    m_b  = L-sum(count)
    
    if(m_b>0){
      idx = 0
      while(m_b!=0){
        count[idx] <- count[idx] + 1  
        idx < idx + 1
        m_b <- m_b - 1
        if(idx==256) idx=0
      }
    }
    else if(m_b<0){
      lin_flag <- TRUE;
      for(l in 2:256){
        if(count[l]!=1){
          lin_flag = FALSE
        }
      }
      if(lin_flag == TRUE){
        count[1] = count[1]+m_b
      }
      else{
        idx = 255
        while(m_b!=0){
          if(count[idx]>1){
            count[idx] <- count[idx] - 1
            m_b <- m_b + 1
          }
          idx <- idx - 1
          if(idx==0) idx=256;
        }
      }
    }
    
    return(data.frame("symbol"=0:255, "count"=count))
}

tANS_generate_bit_based_distribution <- function(data, L){
  data = as.numeric(data)
  occ = as.data.frame(table(data))
  zero_count = round(occ$Freq[1]/sum(occ$Freq)*L)
  one_count = L - zero_count
  return(data.frame("symbol" = 0:1, "count"=c(zero_count, one_count)))
}

# Plot for visual checking - can be moved to another file, if needed.
library(dplyr)
library(ggplot2)
# Example of running it: 
# test <- tANS_generate_normal_symbol_distribution(100, 512, 0, 10)
# plot_normal_and_symbol_distribution(test, 0, 10, scale_max = TRUE)
plot_normal_and_symbol_distribution <- function(symbol_distribution, mu = 0, sigma, scale_max = TRUE){
  xmin = min(symbol_distribution$symbol)
  xmax = max(symbol_distribution$symbol)
  dnorm_x_range <- seq(xmin, xmax, 0.01)
  df_norm <- data.frame(
    x = dnorm_x_range,
    y = dnorm(dnorm_x_range, mu, sigma),
    series = "dnorm"
  )
  
  if(scale_max == TRUE){
    max_count_sd <- max(symbol_distribution$count)
    max_dnorm <- max(df_norm$y)
    
    df_norm$y <- df_norm$y * max_count_sd / max_dnorm
    df_norm$series <- "dnorm (scaled)"
  }
  
  df_sd <- symbol_distribution %>% select(x = symbol, y = count) %>% mutate(series = "NN")
  df <- rbind(df_norm, df_sd)
  g <- ggplot(data = df) + geom_line(aes(x = x, y = y, colour = series))
  return(g)
}