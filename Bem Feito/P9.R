# Fixar a semente para reprodutibilidade
set.seed(4588)

# Parâmetros
lambda0 <- 2.40  # Parâmetro sob H0
lambda1 <- 2.65  # Parâmetro sob H1
k <- 2.623       # Limiar de decisão
m <- 5000        # Número de simulações
n <- 100         # Tamanho de cada amostra

# Função para simular as amostras e calcular erros
simulate_and_calculate_errors <- function(m, n, lambda0, lambda1, k) {
  type1_errors <- 0  # Erro de tipo I (rejeitar H0 quando H0 é verdadeiro)
  type2_errors <- 0  # Erro de tipo II (não rejeitar H0 quando H1 é verdadeiro)
  
  for (i in 1:m) {
    sample0 <- rpois(n, lambda0)
    sample1 <- rpois(n, lambda1)
    
    # Calculando médias amostrais
    mean0 <- mean(sample0)
    mean1 <- mean(sample1)
    
    # Verificando se rejeitamos H0
    if (mean0 > k) {
      type1_errors <- type1_errors + 1
    }
    
    # Verificando se falhamos em rejeitar H0 sob H1
    if (mean1 <= k) {
      type2_errors <- type2_errors + 1
    }
  }
  
  # Calculando frequências relativas dos erros
  freq_type1_error <- type1_errors / m
  freq_type2_error <- type2_errors / m
  
  # Calculando o quociente pedido
  quotient <- freq_type2_error / freq_type1_error
  
  return(list(freq_type1_error = freq_type1_error, 
              freq_type2_error = freq_type2_error, 
              quotient = quotient))
}

# Executar a função de simulação
results <- simulate_and_calculate_errors(m, n, lambda0, lambda1, k)

# Imprimir os resultados
print(results)

