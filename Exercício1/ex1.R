library(ggplot2)

dados <- read.csv('C:\\Users\\vasco\\OneDrive - Universidade de Lisboa\\Universidade\\2ºAno\\2ºSemestre\\ProbabilidadesEstatíostica\\Projeto\\Exercício1\\Paises_PIB_ICH.csv')
dados <- subset(dados, Continent %in% c("Asia", "Africa"))
paises <- c("United Arab Emirates", "Nepal", "Comoros", "Namibia")

ggplot(dados, aes(x = log(GDP), y = HCI)) +
  geom_point(aes(shape = Continent, color = Continent), size = 3) +
  geom_point(data = subset(dados, dados$Country %in% paises), shape = 1, size = 6, color = "black") +
  geom_text(data = subset(dados, dados$Country %in% paises), aes(label = Country), size = 3, nudge_y = 0.04) +
  scale_shape_manual(values = c("Asia" = 16, "Africa" = 17)) +
  labs(x = "GDP per capita", y = "Human Capital Index", title = "Human Capital Index vs GDP per capita by Continent")