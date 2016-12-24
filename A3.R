# A3

# (1)
A <- as.matrix(read.table("D:\A3.txt", header = TRUE, colClasses = c("NULL", rep("integer", 7))))

# (2)
print(det(A))     # -0.0001465853

# (3)
print(qr(A)$rank) # 6