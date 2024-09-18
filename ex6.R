# Definir parâmetros
n <- 30  # número de componentes eletrônicas
a <- 4   # valor esperado da distribuição exponencial
limite <- 90  # limite para a duração total

# Abordagem 1: Valor simulado
set.seed(1948)  # fixar a semente
simulacoes <- 1000  # número de simulações
amostras <- matrix(rexp(n * simulacoes, rate = 1/a), ncol = simulacoes)  # gerar amostras
valores_simulados <- rowSums(amostras)  # calcular a duração total para cada amostra
prop_simulados_maiores_que_limite <- mean(valores_simulados > limite)  # proporção de valores simulados maiores que o limite

# Abordagem 2: Valor exato usando a distribuição gama
prob_exata <- pgamma(limite, shape = n, rate = 1/a, lower.tail = FALSE)

# Diferença entre as abordagens
diferenca <- abs(prop_simulados_maiores_que_limite - prob_exata) * 100

# Resultados
cat("Abordagem 1 (Valor simulado):", prop_simulados_maiores_que_limite, "\n")
cat("Abordagem 2 (Valor exato):", prob_exata, "\n")
cat("Diferença absoluta em porcentagem:", round(diferenca, 4), "%")

