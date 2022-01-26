source("tANS/tANS_coding_table_yann_precise.R")
source("tANS/tANS_coding_table.R")
source("frame_generation.R")
source("tANS/tANS.R")
source("tANS/tANS_generate_symbol_distribution.R")

frame = generate_coap_header_example()

L = 64
coding_table <- tANS_coding_table_from_string(tANS_coding_table_yann_precise(tANS_generate_bit_based_distribution(frame, L)))
#coding_table <- tANS_coding_table_generate_basic(tANS_generate_bit_based_distribution(frame, L))

tANS_coder_state <- list(state = L, L = L, b = 2, output_bits = bit(0))


tANS_coder_state <- tANS_coding(tANS_coder_state, coding_table, as.numeric(frame))

      