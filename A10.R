# A10

readinteger <- function() { 
  n <- readline(prompt = "Enter an integer: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}

n <- readinteger()

if (n < 0) {
  print("Please input a positive integer.")
} else {
  if (n %% 2 == 0) {
    print(paste("2n =", 2 * n))
  } else {
    print(paste("3n-1 =", 3 * n - 1))
  }
}