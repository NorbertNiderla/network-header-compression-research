source("./frame_generation.R")
source("./bpc.R")

all_stream_encoded = c()
all_stream_to_encode = c()

for(k in 1:1000){
   n_frames = 8
   frame_size = 48
   stream_to_encode = c()
   for(x in 1:n_frames){
         frame = generate_coap_header_example()
         
         if(length(frame)!=frame_size){
               frame = c(frame, bit(frame_size-length(frame)))
         }
         
         frame_int = bit_vector_to_int(frame,8)
         
         stream_to_encode[seq(from = x, by = frame_size/length(frame_int), to = frame_size)] = frame_int
   }
   
   stream_encoded = c()
   for(x in seq(from = 1,by = 8,to = 48)){
         stream_encoded = c(stream_encoded, bpc_encode(stream_to_encode[x:(x+7)],8,debug=FALSE))
   }
   
   all_stream_to_encode = c(all_stream_to_encode, stream_to_encode)
   all_stream_encoded = c(all_stream_encoded, stream_encoded)
   
   stream_decoded = c()
   for(x in seq(from = 1,by = 10,to = 60)){
         stream_decoded = c(stream_decoded, bpc_decode(stream_encoded[x:(x+9)], 8, 8))
   }
}
print(entropy::entropy(hist(all_stream_to_encode, breaks = (-0.5:255.5))$counts))
print(entropy::entropy(hist(all_stream_encoded, breaks = (-0.5:255.5))$counts))