# A4

# ln(1+x) = x - x^2/2 + ... + (-1)^(n - 1) * (x^n) / n

F <- function(x, n) {
  sum <- 0
  for (i in 1:n) {
    sum <- sum + (-1)^(i - 1) * (x^i) / i
  }
  sum
}

print(F(-0.5, 100)) # -0.6931472