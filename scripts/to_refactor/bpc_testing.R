# setwd("c:/Users/norbe/Dysk Google/Studia/Praca In¿ynierska/code/")

source("./bpc.R")
source("./own_bit_library.R")
source("./frame_generation.R")


all_data = c()
all_encoded_stream = c()
all_data_decoded = c()

for(x in 1:1000){
      bit_width = 8
      size = 8
      
      data = sample(0:2^bit_width-1, size, replace = TRUE)
      all_data = c(all_data, data)
      
      encoded_stream = bpc_encode(data, debug = 0)
      all_encoded_stream = c(all_encoded_stream, encoded_stream)
      
      data_decoded = bpc_decode(encoded_stream, debug = 0)
      all_data_decoded = c(all_data_decoded, data_decoded)
}

if(sum(all_data-all_data_decoded) == 0){
      print("TEST PASSED")
}else{
      print("TEST NOT PASSED")
}

print(entropy::entropy(all_data))
print(entropy::entropy(all_data_decoded))


res = 0
q = c(1, 1)
for(x in 1:69095){
   if(is.na(match(stream[x], q))){
      res = res + 6
      q[2] = q[1]
      q[1] = stream[x]
   } else {
      res = res + 3
   }
}
