# B2

# Load and format the data
library(readxl)
data <- read_excel("D:\B2.xlsx")
col_names <- c("ID", "Name", "Gender", "MathScore", "EnglishScore", "ScienceScore")
colnames(data) <- c(col_names, "NA")
data <- data[, col_names]

# Caculate the final scores
data$FinalScore <- data$MathScore + data$EnglishScore + data$ScienceScore

# Sort the data by the final scores
data <- data[order(-data$FinalScore), ]

# Ranking
n <- length(data$ID)

A.count <- floor(n * 0.2)
B.count <- floor(n * 0.35)
C.count <- floor(n * 0.3)
D.count <- n - (A.count + B.count + C.count)

ranks <- c(rep("A", A.count), rep("B", B.count), rep("C", C.count), rep("D", D.count))
data$Rank <- ranks

print(data)

# Print the 3D Pie Chart
labels <- c(
  paste("A (", A.count, ")", sep = ""),
  paste("B (", B.count, ")", sep = ""),
  paste("C (", C.count, ")", sep = ""),
  paste("D (", D.count, ")", sep = "")
)

pie3D(c(A.count, B.count, C.count, D.count), labels = labels, explode = 0.1, main = "Pie Chart of the Grades")