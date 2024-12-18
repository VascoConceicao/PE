library(stats4)

# Data and parameters
data <- c(4.37,4.3,5.15,5.11,5.15,4.66,6.15,5.72,5.87,5.64,4.05)
a <- 4
p <- 0.75
th <- 3.6 # theta

# Define the log-likelihood function for Pareto distribution
log_likelihood <- function(theta) {
  if (theta <= 0) return(Inf)  # To ensure theta is positive
  sum_log_likelihood <- sum(log(theta) + theta * log(a) - (theta + 1) * log(data))
  return(-sum_log_likelihood)
}

# Estimate the parameter theta using the mle function
mle_result <- mle(log_likelihood, start = list(theta = th))

# Show the maximum likelihood estimate of theta
theta_estimate <- coef(mle_result)
cat("Estimated Theta:", theta_estimate, "\n")

# Calculate the probability quantile p = p using the estimated theta
quantile_estimate <- a * (1 - p)^(-1/theta_estimate)
cat("Estimated Quantile for p = p:", quantile_estimate, "\n")

# True quantile when theta = p
true_quantile <- a * (1 - p)^(-1/th)
cat("True Quantile for theta=th:", true_quantile, "\n")

# Calculate the absolute deviation between the estimate and the true value of the quantile
absolute_deviation <- abs(quantile_estimate - true_quantile)
cat("Absolute Deviation:", absolute_deviation, "\n")

