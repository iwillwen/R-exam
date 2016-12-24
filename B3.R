# B3

library(pracma)

x <- seq(-pi, pi, length.out = 100)
df <- data.frame(x)

for (i in 1:7) {
  p <- taylor(sin, 0, i)
  y <- polyval(p, x)
  df[paste(i)] <- y
}

ggplot(df, aes(x)) +
  geom_line(aes(y = df$`1`, colour = "1")) +
  geom_line(aes(y = df$`2`, colour = "2")) +
  geom_line(aes(y = df$`3`, colour = "3")) +
  geom_line(aes(y = df$`4`, colour = "4")) +
  geom_line(aes(y = df$`5`, colour = "5")) +
  geom_line(aes(y = df$`6`, colour = "6")) +
  geom_line(aes(y = df$`7`, colour = "7")) +
  ylab("y") +
  ggtitle("sin Taylor Approximation")
