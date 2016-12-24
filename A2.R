# A2
library(ggplot2)

x <- 1:10
y <- sample(1:100, 10)
df <- data.frame(x, y)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth() +
  geom_histogram(binwidth = 1, stat = "identity")