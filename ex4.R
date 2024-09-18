set.seed(2336)

# Número de realizações
num_realizacoes <- 650

# Inicializar contadores
avisos_sonoros <- 0
sistemas_desligados <- 0

# Realizar as simulações
for (i in 1:num_realizacoes) {
  # Simular os sinais emitidos por cada circuito
  sinais <- sample(1:9, size = 7, replace = TRUE, prob = (1:9)/45)
  
  # Verificar se é produzido um aviso sonoro
  if (2 %in% sinais) {
    avisos_sonoros <- avisos_sonoros + 1
    # Verificar se o sistema é desligado
    if (1 %in% sinais) {
      sistemas_desligados <- sistemas_desligados + 1
    }
  }
}

# Calcular a proporção de vezes em que é produzido um aviso sonoro num sistema que não é desligado
proporcao <- avisos_sonoros / (num_realizacoes - sistemas_desligados)

# Arredondar a proporção a 2 casas decimais
proporcao_arredondada <- round(proporcao, 2)

print(paste("Proporção arredondada:", proporcao_arredondada))

