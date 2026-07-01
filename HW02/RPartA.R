# Read data
data <- read.table("roe01.txt", header = TRUE)

# Column means
cat("--- Column Means ---\n")
print(colMeans(data))

# Correlation matrix
cat("\n--- Correlation Matrix ---\n")
print(cor(data))

# Linear regression: ROE ~ all other predictors
cat("\n--- Linear Model: ROE ~ . ---\n")
model <- lm(ROE ~ ., data = data)
print(summary(model))
