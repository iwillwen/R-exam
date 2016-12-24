# A8

obj <- c(2,3,1)
mat <- matrix(c(1,1,1,4,1,7), nrow = 2)
dir <- c("<=", "<=")
rhs <- c(3, 9)

# Package lpSolve
print("Package lpSolve")
library(lpSolve)

r1 <- lp("max", obj, mat, dir, rhs)
print(paste("  Solution:", paste(r1$solution, collapse = " ")))
print(paste("  Max Z =", r1$objval))

print("----")

# Package Rglpk
print("Package Rglpk")
library(Rglpk)

r2 <- Rglpk_solve_LP(obj, mat, dir, rhs, max = TRUE)
print(paste("  Solution:", paste(r2$solution, collapse = " ")))
print(paste("  Max Z =", r2$optimum))

# Compare
print("----")

if (paste(r1$solution, collapse = " ") == paste(r2$solution, collapse = " ") && r1$objval == r2$optimum) {
  print("lpSolve and Rglpk return the same solution.")
} else {
  print("lpSolve and Rglpk return different solutions.")
}