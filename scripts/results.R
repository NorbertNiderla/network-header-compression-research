get_coap_adaptive_results <- function(){
      return(read.csv("./source/coap_adaptive.csv"))
}

get_coap_static_results <- function(){
      return(read.csv("./source/coap_static.csv"))
}

get_ipv6_adaptive_results <- function(){
      return(read.csv("./source/ipv6_adaptive.csv"))
}

get_ipv6_static_results <- function(){
      return(read.csv("./source/ipv6_static.csv"))
}

get_all_results <- function(){
      data_coap_adaptive <- get_coap_adaptive_results()
      data_coap_adaptive["protocol"] = "CoAP"
      data_coap_adaptive["type"] = "Adaptacyjny"
      
      data_coap_static <- get_coap_static_results()
      data_coap_static["protocol"] = "CoAP"
      data_coap_static["type"] = "Statyczny"
      
      data_ipv6_adaptive <- get_ipv6_adaptive_results()
      data_ipv6_adaptive["protocol"] = "IPv6"
      data_ipv6_adaptive["type"] = "Adaptacyjny"
      
      data_ipv6_static <- get_ipv6_static_results()
      data_ipv6_static["protocol"] = "IPv6"
      data_ipv6_static["type"] = "Statyczny"
      
      data <- rbind(data_coap_adaptive,
                    data_coap_static,
                    data_ipv6_adaptive,
                    data_ipv6_static)
      
      data$compression_ratio = 1 - data$size_out_bits / (data$size_in * 8)
      
      return(data)
}