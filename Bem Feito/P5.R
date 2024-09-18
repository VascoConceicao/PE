n <- 4
seed <- 2126
r <- 150
m <- 130
limite <- -0.9 # p = P(T <= limite)

set.seed(seed)

ratios <- c()
for (i in 1:r) {
  count <- 0
  for (j in 1:m) {
    t <- sqrt(n) * rnorm(1) / sqrt(sum(rnorm(n)^2))
    if (t <= limite) {
      count <- count + 1
    }
  }
  ratio <- count/m
  ratios <- c(ratios, ratio)
}

p1 <- mean(ratios)

p2 <- pt(limite, n)

print(abs(p1 - p2) * 100)
