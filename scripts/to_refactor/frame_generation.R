library(bit)

#ACK_part - part of output messages being ACKs to other node
#NON_part - part of CON, and NON messages being NON confirmable
#GET_part - part of output messages that are GET type
#POST_part - part of POST and PUT messages being POST
#payload_part - part of all frames having payload

#generate_coap_header(ACK_part = 0.2, NON_part = 0.2, TKL = 1, GET_part = 0, POST_part = 0.9, payload_part = 0.8)

#TODO: payload part should be calculated upon previous parameters

generate_coap_header_example <- function(){
   frame <- generate_coap_header(ACK_part = 0.2, NON_part = 0.2, TKL = 1, GET_part = 0, POST_part = 0.9, payload_part = 0.8, last_message_id = 0)
   return(frame)
}

generate_coap_header_example_lmid <- function(last_message_id){
   frame <- generate_coap_header(ACK_part = 0.2, NON_part = 0.2, TKL = 1, GET_part = 0, POST_part = 0.9, payload_part = 0.8, last_message_id = last_message_id)
   return(frame)
}

generate_coap_header <- function(ACK_part,NON_part,TKL, GET_part, POST_part, payload_part, last_message_id = 0){
      #ver 2b
      ver <- as.bit(c(0,1))
      ver_rep <- 1
      
      #T 2b
      if(runif(1, 0.0, 1.0)<ACK_part){
            T <- as.bit(c(1,0))
            T_rep <- "ACK"
      }else{
            if(runif(1, 0.0, 1.0)<NON_part){
                  T <- as.bit(c(0,1))
                  T_rep <- "NON"
            }else{
                  T <- as.bit(c(0,0))
                  T_rep <- "CON"
            }
      }
      
      #TKL 4b
      if(TKL>8){
            TKL <- 8
      }
      TKL_rep <- TKL
      TKL <- as.bit(tail(rev(as.integer(intToBits(TKL))),4))
      
      #Code class
      if(T_rep == "CON" | T_rep == "NON"){
            code_class <- as.bit(c(0,0,0))
            code_class_rep <- 0
      }else if(T_rep == "ACK"){
            code_class_rep <- sample(c(2,4,5),1)
            code_class <- as.bit(tail(rev(as.integer(intToBits(code_class_rep))),3))
      }
      
      #Code detail
      if(runif(1, 0.0, 1.0)<GET_part){
            code_detail <- as.bit(c(0,0,0,0,1))
            code_detail_rep <- 1
      }else{
            if(runif(1,0.0,1.0)<POST_part){
                  code_detail <- as.bit(c(0,0,0,1,0))
                  code_detail_rep <- 2
            }else{
                  code_detail <- as.bit(c(0,0,0,1,1))
                  code_detail_rep <- 3
            }
      }
      
      #message id
      if(last_message_id == 0){
         message_id_rep <- sample(0:(2^16-1),1)
      } else {
         message_id_rep <- last_message_id + 1
      }
      message_id <- as.bit(tail(rev(as.integer(intToBits(message_id_rep))),16))
      
      #token
      token = as.bit(as.logical(rawToBits(openssl::rand_bytes(TKL_rep))))
      
      #payload_indicator
      if(runif(1, 0.0, 1.0)<payload_part){
            payload_indicator <- as.bit(c(1,1,1,1,1,1,1,1))
            payload_indicator_rep <- 255
      } else {
            payload_indicator <- bit(0)
            payload_indicator_rep <- c()
      }
      
      frame <- c(ver, T, TKL, code_class, code_detail, message_id, token, payload_indicator)
      return(frame)
}