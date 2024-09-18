library(ggplot2)

dados <- read.csv('C:\\Users\\vasco\\OneDrive - Universidade de Lisboa\\Universidade\\2ºAno\\2ºSemestre\\ProbabilidadesEstatíostica\\Projeto\\Exercício2\\master.csv')
dados <- subset(dados, year == 1986 & age == "25-34 years")

ggplot(dados, aes(x = sex, y = suicides.100k.pop)) +
  geom_boxplot() +
  labs(title = "Number of Suicides per 100000 Inhabitants (1986, 25-34 years)",
       x = "Sex",
       y = "Suicides per 100000 Inhabitants")