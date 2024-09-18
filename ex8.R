library(pracma)

# Sample data and set parameters
set.seed(1592)
data <- c(31.8, 31.7, 35.2, 37.1, 31.7, 36.1, 36.3, 33.2, 34.3, 37.5, 30.4, 34.6, 32.4, 31.7, 30.2, 34.3, 35.6, 34.9, 38.9)
sample_data <- sample(data, 12)
n <- length(sample_data)
gamma <- 0.96
alpha <- (1 - gamma) / 2
a <- qchisq(alpha, df = n-1)
b <- qchisq(1 - alpha, df = n-1)
s2 <- var(sample_data)

# Define the objective function for optim
objective_function_optim <- function(x) {
  eq1 <- pchisq(x[2], df = n-1) - pchisq(x[1], df = n-1) - gamma
  eq2 <- dchisq(x[2], df = n+3) - dchisq(x[1], df = n+3)
  return(c(eq1, eq2))  # Minimize sum of absolute differences
}

result_optim <- fsolve(objective_function_optim, c(a, b))

c <- result_optim$x[1]
d <- result_optim$x[2]
conf_int_optimized <- (n - 1) * s2 / c(d, c)
width_optimized <- abs(conf_int_optimized[2] - conf_int_optimized[1])
initial_interval_width <- abs((n - 1) * s2 / b - (n - 1) * s2 / a)
difference_in_width <- round(initial_interval_width - width_optimized, 3)

cat("Optimized Confidence Interval:", conf_int_optimized, "\n")
cat("Difference in Interval Widths:", difference_in_width, "\n")
