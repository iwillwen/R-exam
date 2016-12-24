# B5

Bartlett.p <- function(data) {
  tmp <- as.numeric(as.matrix(data))
  tmp <- tmp[!is.na(tmp)]
  
  r <- 4
  n <- length(tmp)
  f.e <- n - r
  
  MS.e <- sum(sapply(data, function(A) {
    d <- A[!is.na(A)]
    m <- length(d)
    sd.2 <- (m - 1) * sd(d)^2
    
    sd.2
  }))  / f.e
  
  GMS.e <- prod(sapply(data, function(A) {
    d <- A[!is.na(A)]
    m <- length(d)
    sd.2.m <- (sd(d)^2)^(m - 1)
    sd.2.m
  })) ^ (1 / f.e)
  
  C <- 1 + (sum(sapply(data, function(A) {
    d <- A[!is.na(A)]
    m <- length(d)
    
    (1 / (m -1))
  })) - (1 / f.e))/ (3 * (r - 1))
  
  B <- (f.e / C) * (log(MS.e / GMS.e))

  f.1 <- r - 1
  f.2 <- (r + 1) / ((C - 1)^2)
  A <- f.2 / (2 - C + (2 / f.2))
  
  B.p <- (f.2 * B * C) / (f.1 * (A - B * C))
  
  r <- c()
  
  r$p.value <- B.p
  r$f.1 <- f.1
  r$f.2 <- f.2
  
  r
}

# 8.3.2
print("8.3.2")

data.8.3.2 <- data.frame(
  A1=c(7.9,6.2,6.6,8.6,8.9,10.1,9.6),
  A2=c(5.7,7.5,9.8,6.1,8.4,NA,NA),
  A3=c(6.4,7.1,7.9,4.5,5,4,NA),
  A4=c(6.8,7.5,5,5.3,6.1,7.4,NA)
)
p.8.3.3 <- Bartlett.p(data.8.3.2)$p.value
print(paste("  P-value =", p.8.3.3))

print("----")

# 8.3.6
print("8.3.6")

data.8.3.6 <- data.frame(
  A1=c(12,18,NA),
  A2=c(14,12,13),
  A3=c(19,17,21),
  A4=c(24,30,NA)
)
r <- Bartlett.p(data.8.3.6)
p.8.3.6 <- r$p.value
print(paste("  P-value =", p.8.3.6))
print(paste("  f.1 =", r$f.1))
print(paste("  f.2 =", r$f.2))
F.0.95.3.30 <- 2.92
F.0.95.3.60 <- 2.76
F.0.95.3.50 <- F.0.95.3.30 + (F.0.95.3.60 - F.0.95.3.30) / (60 - 30) * (50 - 30)

if (p.8.3.6 < F.0.95.3.50) {
  print(paste(" ", p.8.3.6, "<", F.0.95.3.50, ", Accept."))
} else {
  print(paste(" ", p.8.3.6, ">=", F.0.95.3.50, ", Reject."))
}