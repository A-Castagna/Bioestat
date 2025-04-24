# Definir o caminho do arquivo 
caminho_arquivo <- "C:/Users/borbo/OneDrive/Área de Trabalho/dengue-dataset.csv"

# Ler o arquivo CSV
dados <- read.csv(caminho_arquivo, sep = ',', dec = '.', header = TRUE,
                  fileEncoding = "latin1")


names(dados)

# Função Cálculo de Moda 
moda_multiplas <- function(v) {
  v <- na.omit(v)  # remove NAs
  tab <- table(v)  # conta frequência
  modas <- names(tab[tab == max(tab)])
  return(modas)
}

# Descrição Chuva
media_chuva <- mean(dados$chuva, na.rm = TRUE)
cat("A média de chuva por mês é ", media_chuva, "milímetros")
mediana_chuva <- median(dados$chuva, na.rm = TRUE)
cat("A mediana de chuva por mês é ", mediana_chuva, "milímetros")
moda_chuva <- moda_multiplas(dados$chuva)
cat("A moda de chuva é ", moda_chuva, "milímetros")
var_chuva <- var(dados$chuva, na.rm = TRUE)
cat("A variância da chuva por mês é", var_chuva, "milímetros")
sd_chuva <- sd(dados$chuva, na.rm = TRUE)
cat("O desvio padrão é", sd_chuva)

# Descrição Casos Confirmados

media_casos <- mean(dados$casos.confirmados, na.rm = TRUE)
cat("A média de casos confirmados por mês é ", media_casos)
mediana_casos <- median(dados$casos.confirmados, na.rm = TRUE)
cat("A mediana de casos confirmados por mês é ", mediana_casos)
moda_casos <- moda_multiplas(dados$casos.confirmados)
cat("A moda de casos confirmados é", moda_casos)
var_casos <- var(dados$casos.confirmados, na.rm = TRUE)
cat("A variância de casos por mês é", var_casos)
sd_casos <- sd(dados$casos.confirmados, na.rm = TRUE)
cat("O desvio padrão é", sd_casos)

# Descrição Média de Temperatura
media_temperatura <- mean(dados$temperatura.media, na.rm = TRUE)
cat("A média da média de temperatura por mês é ", media_temperatura, "ºC")
mediana_temperatura <- median(dados$temperatura.media, na.rm = TRUE)
cat("A mediana de temperatura média por mês é ", mediana_temperatura, "ºC")
moda_temperatura <- moda_multiplas(dados$temperatura.media)
cat("A moda de temperatura média é", moda_temperatura, "ºC")
var_temperatura <- var(dados$temperatura.media, na.rm = TRUE)
cat("A variância da temperatura por mês é", var_temperatura, "milímetros")
sd_temperatura <- sd(dados$temperatura.media, na.rm = TRUE)
cat("O desvio padrão é", sd_temperatura)

summary(dados)

#--------------------------#
# TABELAS E GRÁFICOS

library(dplyr)
library(lubridate)

# Criar a coluna de mês
dados$mes <- month(dados$data, label = TRUE, abbr = TRUE)

# Criar tabela de frequência entre mês e casos confirmados
tabela_freq_mensal <- dados %>%
  group_by(mes) %>%
  summarise(total_casos = sum(casos.confirmados, na.rm = TRUE)) %>%
  arrange(mes) %>%
  mutate(porcentagem = (total_casos / sum(total_casos)) * 100)

# Visualizar a tabela
print(tabela_freq_mensal)
View(tabela_freq_mensal)


library(ggplot2)

# Gráfico de barras
ggplot(tabela_freq_mensal, aes(x = mes, y = total_casos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total de Casos Confirmados por Mês",
    x = "Mês",
    y = "Casos Confirmados"
  ) +
  theme_minimal()

# Verificar o valor mínimo e máximo da temperatura média
min(dados$temperatura.media, na.rm = TRUE)
max(dados$temperatura.media, na.rm = TRUE)

# Ajustando os intervalos com base no valor mínimo e máximo da temperatura média
temp_min <- min(dados$temperatura.media, na.rm = TRUE)
temp_max <- max(dados$temperatura.media, na.rm = TRUE)

# Definir o número de intervalos que você deseja
num_intervals <- 10  # Exemplo: 10 intervalos

# Criar intervalos com base no mínimo e máximo das temperaturas
intervalos <- seq(temp_min, temp_max, length.out = num_intervals + 1)

# Criar a coluna de faixas de temperatura
dados$faixa_temperatura <- cut(dados$temperatura.media, 
                               breaks = intervalos, 
                               include.lowest = TRUE, 
                               labels = paste(head(intervalos, -1), tail(intervalos, -1), sep = "-"))

# Tabela de frequência por faixa de temperatura
tabela_freq_faixa_temperatura <- dados %>%
  group_by(faixa_temperatura) %>%
  summarise(total_casos = sum(casos.confirmados, na.rm = TRUE)) %>%
  arrange(faixa_temperatura)

# Exibindo a tabela
print(tabela_freq_faixa_temperatura)
View(tabela_freq_faixa_temperatura)

library(ggplot2)

# Gráfico de barras com a tabela de frequência por faixa de temperatura
ggplot(tabela_freq_faixa_temperatura, aes(x = faixa_temperatura, y = total_casos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total de Casos Confirmados por Faixa de Temperatura",
    x = "Faixa de Temperatura (°C)",
    y = "Total de Casos Confirmados"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Verificar os valores mínimo e máximo de chuva
chuva_min <- min(dados$chuva, na.rm = TRUE)
chuva_max <- max(dados$chuva, na.rm = TRUE)

# Definir intervalos (ex: 20)
num_intervals_chuva <- 20

# Criar intervalos e faixas
intervalos_chuva <- seq(chuva_min, chuva_max, length.out = num_intervals_chuva + 1)

dados$faixa_chuva <- cut(dados$chuva,
                         breaks = intervalos_chuva,
                         include.lowest = TRUE,
                         labels = paste0(round(head(intervalos_chuva, -1), 1), 
                                         "-", 
                                         round(tail(intervalos_chuva, -1), 1)))

# Calcular a tabela de frequência
tabela_freq_faixa_chuva <- dados %>%
  group_by(faixa_chuva) %>%
  summarise(total_casos = sum(casos.confirmados, na.rm = TRUE)) %>%
  arrange(faixa_chuva)

# Visualizar
View(tabela_freq_faixa_chuva)


# Gráfico de barras da frequência de casos por faixa de chuva
ggplot(tabela_freq_faixa_chuva, aes(x = faixa_chuva, y = total_casos)) +
  geom_bar(stat = "identity", fill = "darkseagreen4") +
  labs(
    title = "Total de Casos Confirmados por Faixa de Chuva",
    x = "Faixa de Chuva (mm)",
    y = "Total de Casos Confirmados"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------#
# ANÁLISE DOS DIFERENTES GRUPOS

# Correlação entre Temperatura média e Casos de Dengue
cor.test(dados$temperatura.media, dados$casos.confirmados, method = "pearson")
# Correlação de Pearson entre temperatura média e casos de dengue mostrou que não há uma correlação linear entre essas duas variáveis
# p-valor = 0.5588, corr = 0.041

#------------#
# Correlação entre Quantidade de Chuva e Casos de Dengue
cor.test(dados$chuva, dados$casos.confirmados, method = "pearson")
#Correlação de Pearson entre quantidade de chuva e casos de dengue mostrou que não há uma correlação linear entre essas duas variáveis
# p-valor = 0.2435, corr = - 0.083

#------------#
# Correlação de Spearman entre Temperatura Média e Casos de Dengue
cor.test(dados$temperatura.media, dados$casos.confirmados, method = "spearman", exact = FALSE)
# Há uma correlação positiva fraca (rho = 0.197) entre Temp Média e Casos de Dengue, 
# p-valor = 0.0045, essa correlação é significativamente estatística

#------------#
# Correlação de Spearman entre Quantidade de Chuva e Casos de Dengue
cor.test(dados$chuva, dados$casos.confirmados, method = "spearman", exact = FALSE)
# Não há correlação entre essas duas variáveis (rho = 0.042), e p-valor = 0.552, corroborando a hipótese nula


#------------#
# Regressão Binomial Negativa
library(MASS)
modelo_nb <- glm.nb(casos.confirmados ~ temperatura.media + chuva, data = dados)
summary(modelo_nb)
# Mantendo a chuva constante, um aumento de 1 grau de temperatura está associado com um 
# aumento de 31% nos casos de dengue, p-valor = 0.000067 

#------------#
# Comparando as temperaturas médias (alta, média e baixa) e os casos de dengue

library(FSA)
# Dividindo a temperatura em 3 faixas
dados$temp_cat <- cut(dados$temperatura.media, breaks = 3, labels = c("baixa", "média", "alta"))

# Para dados que não são distribuídos normalmente
kruskal.test(casos.confirmados ~ temp_cat, data = dados)

# Teste de Dunn com correção de Bonferroni
dunnTest(casos.confirmados ~ temp_cat, data = dados, method = "bonferroni")
# Diferença significativa entre temperatura média e temperatura alta (p-valor = 0.0228)
# Tendência de diferença entre temperatura baixa e alta (p-valor = 0.0558)
# Nenhuma diferença entre temperatura média e baixa

# Dividindo a chuva em 3 faixas
dados$chuva_cat <- cut(dados$chuva, breaks = 3, labels = c("baixa", "média", "alta"))
kruskal.test(casos.confirmados ~ chuva_cat, data = dados)
# Teste de Dunn com correção de Bonferroni
dunnTest(casos.confirmados ~ chuva_cat, data = dados, method = "bonferroni")
# Não há diferença significativa entre os grupos de chuva 









