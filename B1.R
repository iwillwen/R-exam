# B1

EP <- function(data) {
  n <- length(data)
  ave.data <- mean(data)
  sd.star.2 <- sd(data)^2 * (n - 1) / n
  
  T.EP <- 1 + n / sqrt(3)
  
  tmp <- 0
  
  for (i in 2:n) {
    for (j in 1:(i - 1)) {
      tmp <- tmp + exp((-(data[j] - data[i])^2) / (2 * sd.star.2))
    }
  }
  
  T.EP <- T.EP + (2 / n) * tmp
  T.EP <- T.EP - sqrt(2) * sum(sapply(data, function(x) exp((-(x - ave.data)^2) / (4 * sd.star.2))))
  
  T.EP
}

# 7.5.4
data.7.5.4 <- c(147,186,141,183,190,123,155,164,183,150,134,170,
                144,99,156,176,160,174,153,162,167,179,78,173,168)
print(paste("7.5.4, T.EP =", EP(data.7.5.4))) # 0.6115393

print("-------")

# Practice 7.5.4
print("Practice 7.5.4")

data.p.7.5.4 <- c(0.32,0.25,0.29,0.25,0.28,0.30,0.23,0.23,0.40,0.32,0.35,0.19,0.34,
                  0.33,0.33,0.28,0.28,0.22,0.30,0.24,0.35,0.24,0.30,0.23,0.22)
T.EP <- EP(data.p.7.5.4)
print(paste("  T.EP =", T.EP))

T.0.95.EP.20 <- 0.366
T.0.95.EP.30 <- 0.371
T.0.95.EP.25 <- T.0.95.EP.20 + (T.0.95.EP.30 - T.0.95.EP.20) / (30 - 20) * (25 - 20)

if (T.EP > T.0.95.EP.25) {
  print("  The examples are not obey the normal distribution.")
} else {
  print("  The examples are obey the normal distribution.")
}