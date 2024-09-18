# Fixar a semente para reprodutibilidade
set.seed(2126)

# Parâmetros da simulação
num_observacoes <- 4  # Número de observações (n)
num_repeticoes <- 150  # Número de repetições da simulação (r)
num_valores_T_por_simulacao <- 130  # Número de valores T gerados por repetição (m)
valor_limite <- -0.9  # Limite para calcular a proporção

# Função para gerar valores de T
gerar_valores_T <- function(num_observacoes, num_valores_T_por_simulacao) {
  valores_T <- numeric(num_valores_T_por_simulacao)
  for (i in 1:num_valores_T_por_simulacao) {
    Z <- rnorm(num_observacoes + 1)  # Gerar uma sequência de números aleatórios normais
    valores_T[i] <- sqrt(num_observacoes) * Z[1] / sqrt(sum(Z[2:(num_observacoes + 1)]^2))
  }
  return(valores_T)
}

# Gerar num_repeticoes amostras de num_valores_T_por_simulacao valores de T cada
proporcoes <- numeric(num_repeticoes)
for (i in 1:num_repeticoes) {
  valores_T <- gerar_valores_T(num_observacoes, num_valores_T_por_simulacao)
  proporcoes[i] <- mean(valores_T <= valor_limite)
}

# Calcular a média das proporções
proporcao_media_simulada <- mean(proporcoes)
print(proporcao_media_simulada)

# Calcular a probabilidade exata usando a distribuição t de Student
probabilidade_exata <- pt(valor_limite, df = num_observacoes)
print(probabilidade_exata)

# Calcular a diferença absoluta e multiplicar por 100
diferenca_absoluta <- abs(proporcao_media_simulada - probabilidade_exata) * 100
diferenca_arredondada <- round(diferenca_absoluta, 5)

# Imprimir o resultado final
print(diferenca_arredondada)