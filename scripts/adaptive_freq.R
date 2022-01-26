diff_when_update_0 <- function(f, N, total){
        return((-f+total)/(N+1))
}

diff_when_update_1 <- function(f, N){
        return((-f)/(N+1))
}


freq <- 1:63
count <- 4
t <- 64

diff <-c()
diff_1 <- c()

for(fr in freq){
        diff <- c(diff, diff_when_update_0(fr, count, t))
        diff_1 <- c(diff_1, diff_when_update_1(fr, count))
}

library(reshape2)
library(ggplot2)
data <- data.frame("freq" = freq, "diff_0" = diff, "diff_1" = diff_1)
data <- melt(data, id="freq")

p <- ggplot(data = data, aes(x=freq, y=value, color=variable)) +
      geom_line(size=2)+
      theme_bw() +
      ggtitle("Zmiana współczynnika częstotliwości (f_max = 64)") +
      labs(x = "Aktualna częstotliwość symbolu 0", y = "Zmiana") +
      theme(title = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)) +
      scale_color_discrete(name = "Symbol", labels=c("0","1")) 
if(TRUE){
      ggsave(filename = "./output/figures/adaptive_frequency_diff.pdf",
             plot=p,
             device = grDevices::cairo_pdf,
             width = 7.5,
             height = 5)
}
      
print(p)
