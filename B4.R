# B4

W <- function(data, theta.0) {
  n <- length(data)
  ave.data <- mean(data)
  X.2 <- 2 * n * ave.data / theta.0
  
  c(n, X.2)
}

W.1 <- function(data, theta.0, a) {
  w <- W(data, theta.0)
  n <- w[1]
  X.2 <- w[2]
  
  q <- qchisq(1 - a, 2 * n)
  
  p <- pchisq(X.2, 2 * n)
  
  result <- c()
  result$X.2 <- X.2
  result$accept <- X.2 < q
  result$p.value <- p
  result
}

W.2 <- function(data, theta.0, a) {
  w <- W(data, theta.0)
  n <- w[1]
  X.2 <- w[2]
  
  q <- qchisq(a, 2 * n)
  
  p <- 1 - pchisq(X.2, 2 * n)

  result <- c()
  result$X.2 <- X.2
  result$accept <- X.2 > q
  result$p.value <- p
  result
}

W.3 <- function(data, theta.0, a) {
  w <- W(data, theta.0)
  n <- w[1]
  X.2 <- w[2]
  
  q.1 <- qchisq(a / 2, 2 * n)
  q.2 <- qchisq(1 - a / 2, 2 * n)
  
  p <- 2 * min(c(pchisq(X.2, 2 * n), 1 - pchisq(X.2, 2 * n)))

  result <- c()
  result$X.2 <- X.2
  result$accept <- X.2 > q.1  || X.2 < q.2
  result$p.value <- p
  result
}

# 3.7.1
print("3.7.1")
data.3.7.1 <- c(395,4094,119,11572,6133)
p.value <- W.2(data.3.7.1, 6000, 0.05)$p.value
print(paste("  P-value =", p.value))

# 3.7.2
print("3.7.2")
data.3.7.2 <- c(2686,2001,2082,792,1600,4105,1416,2089)

# W1
w <- W.1(data.3.7.2, 1200, 0.05)
X.2 <- w$X.2
p.value <- w$p.value
print(paste("  W1 Accept:", w$accept))
print(paste("  W1 X^2 =", X.2))
print(paste("  W1 P-value =", p.value))

# W2
w <- W.2(data.3.7.2, 1200, 0.05)
X.2 <- w$X.2
p.value <- w$p.value
print(paste("  W2 Accept:", w$accept))
print(paste("  W2 X^2 =", X.2))
print(paste("  W2 P-value =", p.value))

# W3
w <- W.3(data.3.7.2, 1200, 0.05)
X.2 <- w$X.2
p.value <- w$p.value
print(paste("  W3 Accept:", w$accept))
print(paste("  W3 X^2 =", X.2))
print(paste("  W3 P-value =", p.value))