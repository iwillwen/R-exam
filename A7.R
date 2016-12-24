# A7

data <- read.csv("D:\A7.csv", header = TRUE)
A.x <- data$x
A.y <- data$y[!is.na(data$y)]

ave.x <- mean(A.x)
ave.y <- mean(A.y)

m.x <- length(A.x)
m.y <- length(A.y)
n <- m.x + m.y
f.x <- m.x - 1
f.y <- m.y - 1
f.e <- n - 2

s.x.2 <- sd(A.x)^2
s.y.2 <- sd(A.y)^2

MS.e <- (s.x.2*m.x + s.y.2*m.y) / f.e
GMS.e <- (s.x.2^m.x * s.y.2^m.y)^(1/f.e)

C <- 1 + (1/f.x + 1/f.y - 2/f.e) / 3
B <- (f.e / C) * (log(MS.e / GMS.e))
X.2.0.95.1 <- 3.8415

if (!B > X.2.0.95.1) {
  print("X and Y are close.")
} else {
  print("X and Y are obviously different.")
}

print(paste("Average of X is", ave.x))
print(paste("Average of Y is", ave.y))

better <- "X"
if (ave.y > ave.x) better <- 'Y'

print(paste("So", better, "is better."))