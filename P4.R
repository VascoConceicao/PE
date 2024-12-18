# Número de circuitos elétricos
num_circuitos <- 7

# Sinais codificados no conjunto {1, 2, ..., max_sinal}
max_sinal <- 9

# Fixar a semente
set.seed(2336)

# Número de realizações
num_realizacoes <- 650

# Inicializar contadores
avisos_sonoros_sistemas_nao_desligados <- 0
sistemas_nao_desligados <- 0

# Realizar as simulações
for (i in 1:num_realizacoes) {
  # Simular os sinais emitidos por cada circuito
  sinais <- sample(1:max_sinal, size = num_circuitos, replace = TRUE, prob = (1:max_sinal)/sum(1:max_sinal))
  
  # Verificar se o sistema não é desligado
  if (!(1 %in% sinais)) {
    sistemas_nao_desligados <- sistemas_nao_desligados + 1
    # Verificar se é produzido um aviso sonoro num sistema não é desligado
    if (2 %in% sinais) {
      avisos_sonoros_sistemas_nao_desligados <- avisos_sonoros_sistemas_nao_desligados + 1
    }
  }
}

# Calcular a proporção de vezes em que é produzido um aviso sonoro num sistema que não é desligado
proporcao <- avisos_sonoros_sistemas_nao_desligados / sistemas_nao_desligados

print(paste("Proporção:", proporcao))

