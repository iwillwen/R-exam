# A5
library(readxl)
library(ggplot2)

values <- read_excel("D:\A5.xlsx",   col_names = FALSE)$X0
df <- data.frame(value = values, window = character(2), stringsAsFactors=FALSE)
window <- (max(values) - min(values)) / 5
windows <- c("A", "B", "C", "D", "E")

for (i in 1:5) {
  min.value <- min(values) + (i - 1) * window
  max.value <- min(values) + i * window
  window.values <- values[values >= min.value & values <= max.value]
  
  for (j in 1:length(window.values)) {
    df[df$value == window.values[j], ]$window <- windows[i]
  }
}

p1 <- ggplot(df, aes(x=value, y= ..density..)) +
  geom_histogram(aes(colour = window, fill = window), alpha = 0.4) +
  geom_density(adjust = 1/5) +
  geom_rug(aes(value, NULL, colour = window), sides="b") +
  ggtitle("Histogram")
p2 <- ggplot(df, aes(x = window, y = value, group = window, colour = window)) +
  geom_boxplot() +
  ggtitle("Boxplot")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p2, cols = 2)