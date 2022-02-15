source("./scripts/probability_model.R")
source("./scripts/entropy.R")
source("./scripts/data.R")

coap_data <- get_header("./source/ring_coap_raw_data.txt")
mqtt_data <- get_header("./source/mqtt_headers.txt")
mqtt_data <- mqtt_data[lengths(mqtt_data) < 300]

results <- data.frame("protocol"= c("coap", "mqtt"),
                      "positional_1-bit"=c(get_total_entropy(get_position_probability_model(coap_data, 1)),
                                           get_total_entropy(get_position_probability_model(mqtt_data, 1))),
                      "positional_2-bit"=c(get_total_entropy(get_position_probability_model(coap_data, 2)),
                                           get_total_entropy(get_position_probability_model(mqtt_data, 2))),
                      "positional_4-bit"=c(get_total_entropy(get_position_probability_model(coap_data, 4)),
                                           get_total_entropy(get_position_probability_model(mqtt_data, 4))),
                      "positional_8-bit"=c(get_total_entropy(get_position_probability_model(coap_data, 8)),
                                           get_total_entropy(get_position_probability_model(mqtt_data, 8))),
                      "markov_1" = c(get_markov_chain_entropy(first_order_markov(coap_data)),
                                     get_markov_chain_entropy(first_order_markov(mqtt_data))),
                      "markov_2" = c(get_markov_chain_entropy(second_order_markov(coap_data)),
                                     get_markov_chain_entropy(second_order_markov(mqtt_data))),
                      "markov_3" = c(get_markov_chain_entropy(third_order_markov(coap_data)),
                                     get_markov_chain_entropy(third_order_markov(mqtt_data))),
                      "markov_4" = c(get_markov_chain_entropy(fourth_order_markov(coap_data)),
                                     get_markov_chain_entropy(fourth_order_markov(mqtt_data))),
                      "markov_5" = c( get_markov_chain_entropy(fifth_order_markov(coap_data)),
                                      get_markov_chain_entropy(fifth_order_markov(mqtt_data))),
                      "standard_1-bit"=c(get_standard_model_entropy(standard_model(coap_data, 1)),
                                         get_standard_model_entropy(standard_model(mqtt_data, 1))),
                      "standard_2-bit"=c(get_standard_model_entropy(standard_model(coap_data, 2)),
                                         get_standard_model_entropy(standard_model(mqtt_data, 2))),
                      "standard_4-bit"=c(get_standard_model_entropy(standard_model(coap_data, 4)),
                                         get_standard_model_entropy(standard_model(mqtt_data, 4))),
                      "standard_8-bit"=c(get_standard_model_entropy(standard_model(coap_data, 8)),
                                         get_standard_model_entropy(standard_model(mqtt_data, 8))))