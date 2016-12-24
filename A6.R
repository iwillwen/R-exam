# A6
data <- read.csv("D:\A6.csv", header= TRUE)
p.0.995.12 <- 3.0545
p.0.995.6 <- 3.7074

print(paste("P =", p.0.99.14))

## Method 1
print("# Method 1")

n <- length(data$A) + length(data$B)

ave.x <- mean(data$A)
ave.y <- mean(data$B)

s.2.x <- sum(data$A^2)
s.2.y <- sum(data$B^2)
s.w <- (s.2.x + s.2.y) / 2

t.1 <- (ave.x - ave.y) / (s.w / sqrt(n / 2))
print(paste("  t =", t.1))

if (abs(t.1) < p.0.995.12) {
  print("  Result: Accept")
} else {
  print("  Result: Reject")
}

## Method 2
print("# Method 2")

n <- 7

diffs <- data$A - data$B
ave.d <- mean(diffs)
s.d <- sqrt(sum((diffs - ave.d)^2)/(n - 1))
t.2 <- ave.d / (s.d / sqrt(n))
print(paste("  t =", t.2))

if (abs(t.2) < p.0.995.6) {
  print("  Result: Accept")
} else {
  print("  Result: Reject")
}