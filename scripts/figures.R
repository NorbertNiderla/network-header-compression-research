source("./scripts/results.R")
source("./scripts/energy.R")
library(ggplot2)
library(Cairo)
library(tidyr)
library(hexbin)

AXIS_X_TEXT_SIZE <- 12
AXIS_Y_TEXT_SIZE <- 12
TITLE_TEXT_SIZE  <- 16
LEGEND_TEXT_SIZE <- 11

figure_algorithm_test_results <- function(data, save = FALSE){
   p <- ggplot(data = data, aes(x = mean, y = 1 - ratio)) +
      geom_hex(bins = 20, show.legend = FALSE) +
      scale_fill_continuous(type = "viridis") +
      scale_y_continuous(labels = scales::percent, limits=c(0,1)) + 
      ggtitle("Testowa kompresja danych generowanych") +
      labs(y = "Stopień kompresji [%]", x = "Średnie prawdopodobieństwo wystąpienia \"0\"") +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE))
      
      if(save){
         ggsave(filename = "./output/figures/test.pdf",
                plot=p,
                device = grDevices::cairo_pdf,
                width = 7.5,
                height = 5)
      }
      
   return(p)
}

figure_energy_ratio <- function(save = FALSE){
      data <- get_energy_ratio()
      p <- ggplot(data=data,aes(x = energy_ratio, y=..count../sum(..count..)*4))+
         geom_density(fill="#69b3a2", alpha=1) +
         facet_grid(type~protocol) +
         scale_x_continuous(limits = c(0,1.2)) +
         scale_y_continuous(labels=scales::percent, limits = c(0,0.1)) +
         ggtitle("Porównanie zużycia energii") +
         labs(x = "Stosunek zużytej energii", y = "Histogram") +
         theme_bw() +
         theme(title = element_text(size = TITLE_TEXT_SIZE),
               axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
               axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
               legend.text = element_text(size = LEGEND_TEXT_SIZE))
      
      if(save){
         ggsave(filename = "./output/figures/energy_ratio.pdf",
                plot=p,
                device = grDevices::cairo_pdf,
                width = 7.5,
                height = 7.5)
      }
      return(p)
}

figure_coap_adaptive_compression <- function(save = FALSE){
      data <- get_coap_adaptive_results()
      data$compression_ratio <-  1 - data$size_out_bits / (data$size_in * 8)
      data$changed_frame <- data$changed_bits / (data$size_in * 8)
      pl <- ggplot(data = data, aes(x = changed_frame,
                                    y = compression_ratio,
                                    group = changed_frame)) +
            scale_y_reverse(labels = scales::percent, limits = c(1,0)) +
            scale_x_continuous(labels = scales::percent, limits = c(0,0.4)) +
            geom_boxplot(alpha=1, show.legend = FALSE) +
            ggtitle("Kompresja nagłówka CoAP algorytmem adaptacyjnym") +
            labs(x = "Zmiana zawartości ramki", y = "Stopień kompresji [%]") +
         theme_bw() +
         theme(title = element_text(size = TITLE_TEXT_SIZE),
               axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
               axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
               legend.text = element_text(size = LEGEND_TEXT_SIZE))
      
      if(save){
            ggsave(filename = "./output/figures/coap_adaptive_compression.pdf",
                   plot=pl,
                   device = grDevices::cairo_pdf,
                   width = 7.5,
                   height = 5)
      }
      return(pl)
}

figure_ipv6_adaptive_compression <- function(save = FALSE){
   data <- get_ipv6_adaptive_results()
   
   data$compression_ratio <-  1 - data$size_out_bits / (data$size_in * 8)
   data$changed_frame <- data$changed_bits / (data$size_in * 8)
   pl <- ggplot(data = data, aes(x = changed_frame,
                                 y = compression_ratio,
                                 group = changed_frame)) +
      scale_y_reverse(labels = scales::percent, limits = c(1,0)) +
      scale_x_continuous(labels = scales::percent, limits = c(0,0.1)) +
      geom_boxplot(alpha=1, show.legend = FALSE) +
      ggtitle("Kompresja nagłówka IPv6 algorytmem adaptacyjnym") +
      labs(x = "Zmiana zawartości ramki", y = "Stopień kompresji [%]") +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE))
   
   if(save){
      ggsave(filename = "./output/figures/ipv6_adaptive_compression.pdf",
             plot=pl,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
   }
   return(pl)
}

figure_coap_static_compression <- function(save = FALSE){
   data <- get_coap_static_results()
   data$compression_ratio <-  1 - data$size_out_bits / (data$size_in * 8)
   data$changed_frame <- data$changed_bits / (data$size_in * 8)
   pl <- ggplot(data = data, aes(x = changed_frame,
                                 y = compression_ratio,
                                 group = changed_frame)) +
      scale_y_reverse(labels = scales::percent, limits = c(1,0)) +
      scale_x_continuous(labels = scales::percent, limits = c(0,0.4)) +
      geom_boxplot(alpha=1, show.legend = FALSE) +
      ggtitle("Kompresja nagłówka CoAP algorytmem statycznym") +
      labs(x = "Zmiana zawartości ramki", y = "Stopień kompresji [%]") +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE))
   
   if(save){
      ggsave(filename = "./output/figures/coap_static_compression.pdf",
             plot=pl,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
   }
   return(pl)
}

figure_ipv6_static_compression <- function(save = FALSE){
   data <- get_ipv6_static_results()
   data$compression_ratio <-  1 - data$size_out_bits / (data$size_in * 8)
   data$changed_frame <- data$changed_bits / (data$size_in * 8)
   pl <- ggplot(data = data, aes(x = changed_frame,
                                 y = compression_ratio,
                                 group = changed_frame)) +
      scale_y_reverse(labels = scales::percent, limits = c(1,0)) +
      scale_x_continuous(labels = scales::percent, limits = c(0,0.1)) +
      geom_boxplot(alpha=1, show.legend = FALSE) +
      ggtitle("Kompresja nagłówka IPv6 algorytmem statycznym") +
      labs(x = "Zmiana zawartości ramki", y = "Stopień kompresji [%]") +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE))
   
   if(save){
      ggsave(filename = "./output/figures/ipv6_static_compression.pdf",
             plot=pl,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
   }
   return(pl)
}

figure_coap_time <- function(save = FALSE){
   data <- get_coap_adaptive_results()
   data$compression_ratio <- 1 - data$size_out_bits / (data$size_in * 8)
   data$type <- rep("Adaptacyjny", nrow(data))
   
   data_s <- get_coap_static_results()
   data_s$compression_ratio <- 1 - data_s$size_out_bits / (data_s$size_in * 8)
   data_s$type <- rep("Statyczny", nrow(data_s))
   
   data <- rbind(data, data_s)
   data$time <- data$time / data$size_in
   
   pl <- ggplot(data = data, aes(x = compression_ratio,
                                 y = time,
                                 color = type)) +
      #scale_y_reverse(labels = scales::percent, limits = c(1,-1)) +
      scale_x_reverse(labels = scales::percent, limits = c(1,0)) +
      scale_y_continuous(limits = c(0,45)) +
      geom_point(size=2, show.legend = FALSE) +
      ggtitle("Czasy kompresji CoAP w przeliczeniu na 1B nagłówka") +
      labs(x = "Stopień kompresji [%]", y = "Czas przetwarzania [us]") +
      scale_color_discrete(name = "Typ algorytmu") +
      geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE))
   
   if(save){
      ggsave(filename = "./output/figures/coap_time.pdf",
             plot=pl,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
   }
   return(pl)
}

figure_ipv6_time <- function(save = FALSE){
   data <- get_ipv6_adaptive_results()
   data$compression_ratio <- 1 - data$size_out_bits / (data$size_in * 8)
   data$type <- rep("Adaptacyjny", nrow(data))
   
   data_s <- get_ipv6_static_results()
   data_s$compression_ratio <- 1 - data_s$size_out_bits / (data_s$size_in * 8)
   data_s$type <- rep("Statyczny", nrow(data_s))
   
   data <- rbind(data, data_s)
   data$time <- data$time / data$size_in
   
   pl <- ggplot(data = data, aes(x = compression_ratio,
                                 y = time,
                                 color = type)) +
      #scale_y_reverse(labels = scales::percent, limits = c(1,-1)) +
      scale_x_reverse(labels = scales::percent, limits = c(1,0)) +
      scale_y_continuous(limits = c(0,30)) +
      geom_point(size=2, show.legend = FALSE) +
      ggtitle("Czasy kompresji IPv6 w przeliczeniu na 1B nagłówka") +
      labs(x = "Stopień kompresji [%]", y = "Czas przetwarzania [us]") +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE)) +
      scale_color_discrete(name = "Typ algorytmu") +
      geom_smooth(method = "lm", formula = y ~ poly(x,2)) 
   
   if(save){
      ggsave(filename = "./output/figures/ipv6_time.pdf",
             plot=pl,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
   }
   return(pl)
}

figure_time <- function(save = FALSE){
   data <- get_all_results()
   data$time <- data$time / data$size_in
   pl <- ggplot(data = data, aes(x = compression_ratio,
                                 y = time,
                                 color = protocol,
                                 shape = type)) +
      #scale_y_reverse(labels = scales::percent, limits = c(1,-1)) +
      scale_x_reverse(labels = scales::percent, limits = c(1,0)) +
      scale_y_continuous(limits = c(0,45)) +
      geom_point(size=3, show.legend = TRUE) +
      ggtitle("Czasy kompresji w przeliczeniu na 1B nagłówka") +
      labs(x = "Stopień kompresji [%]", y = "Czas przetwarzania [us]") +
      scale_color_discrete(name = "Protokół") +
      scale_shape_discrete(name = "Typ algorytmu") +
      geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
      theme_bw() +
      theme(title = element_text(size = TITLE_TEXT_SIZE),
            axis.text.x = element_text(size = AXIS_X_TEXT_SIZE),
            axis.text.y = element_text(size = AXIS_Y_TEXT_SIZE),
            legend.text = element_text(size = LEGEND_TEXT_SIZE))
   
   if(save){
      ggsave(filename = "./output/figures/time.pdf",
             plot=pl,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
   }
   return(pl)
}

print_all_figures <- function(){
   figure_coap_adaptive_compression(save = TRUE)
   figure_coap_static_compression(save = TRUE)
   figure_ipv6_adaptive_compression(save = TRUE)
   figure_ipv6_static_compression(save = TRUE)
   figure_time(save = TRUE)
   figure_energy_ratio(save = TRUE)
}
