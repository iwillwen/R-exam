# A9

library(ggplot2)

# X~N(100, 25)
# miu=100
# sigma=sqrt(25)=5

n <- 50

randoms <- rnorm(n) * 5 + 100
t <- rank(randoms) / n
q <- qnorm(t)
print(summary(randoms))

n.max <- max(randoms)
n.min <- min(randoms)
window <- (n.max - n.min) / (n / 2)

p1 <- ggplot(mapping = aes(randoms, ..density.., colour = randoms)) +
  xlim(n.min, n.max) +
  geom_histogram() +
  ggtitle("Histogram")

p2 <- ggplot(mapping = aes(randoms, q, colour = randoms)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm") +
  ggtitle("Q-Q Plot")

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