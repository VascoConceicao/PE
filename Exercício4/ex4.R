set.seed(2336)

probabilidades <- (1:9) / 45
realizacoes <- replicate(650, sample(1:9, 7, replace = TRUE, prob = probabilidades))
