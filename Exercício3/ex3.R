library(readxl)
library(ggplot2)

dados <- read_excel('C:\\Users\\vasco\\OneDrive - Universidade de Lisboa\\Universidade\\2ºAno\\2ºSemestre\\ProbabilidadesEstatíostica\\Projeto\\Exercício3\\electricity.xlsx')
paises <- c("IEA Total", "Hungary", "Iceland")
dados <- subset(dados, YEAR >= 2015 & PRODUCT == "Renewables" & COUNTRY %in% paises)
dados$share <- as.numeric(dados$share) * 100
dados$CODE_TIME <- factor(dados$CODE_TIME, levels = unique(dados$CODE_TIME))

ggplot(dados, aes(x = CODE_TIME, y = share, group = COUNTRY, color = COUNTRY)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = dados$CODE_TIME[seq(1, nrow(dados), by = 12 * length(paises))]) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Month", y = "Renewables (%)", title = "Monthly Proportion of Electricity Produced from Renewables (2015 - 2022)")