# Trabalho Final de Bioestatística


# Instalando Pacotes

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}

using("dplyr","lubridate","ggplot2","MASS","FSA","tidyr","scales","car","RVAideMemoire","ggpubr","ggeffects","psych","gt")

# Adicionando banco de dados 
# Define o caminho do arquivo e adiciona o banco de dados
# Enrico 
# 07/04
caminho_arquivo <- "C:/Users/borbo/OneDrive/Área de Trabalho/dengue-dataset.csv"
dados <- read.csv(caminho_arquivo, sep = ',', dec = '.', header = TRUE,
                  fileEncoding = "latin1")

# Estatística Descritiva
# Descreve os valores das variáveis do banco de dados
# Enrico
# 07/04
summary(dados)

#--------------------------#
# TABELAS E GRÁFICOS


# Código para gráfico de Casos Confirmados e Temperatura
# Cria um gráfico com os casos confirmados de dengue em função da temperatura média
# Enrico
# 07/04
library(dplyr)
library(ggplot2)

dados <- dados %>%
  filter(!is.na(temperatura.media))
largura_faixa <- 1.0
temp_min <- floor(min(dados$temperatura.media))
temp_max <- ceiling(max(dados$temperatura.media))
intervalos_temp <- seq(temp_min, temp_max, by = largura_faixa)
dados$faixa_temperatura <- cut(
  dados$temperatura.media,
  breaks = intervalos_temp,
  include.lowest = TRUE,
  labels = paste0(
    head(intervalos_temp, -1), " - ",
    tail(intervalos_temp, -1)
  )
)
tabela_freq_faixa_temperatura <- dados %>%
  group_by(faixa_temperatura) %>%
  summarise(total_casos = sum(casos.confirmados, na.rm = TRUE)) %>%
  arrange(faixa_temperatura)
ggplot(tabela_freq_faixa_temperatura, aes(x = faixa_temperatura, y = total_casos)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Casos Confirmados por Faixa de Temperatura",
    x = "Faixa de Temperatura (°C)",
    y = "Casos Confirmados"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Código para gráfico de Casos Confirmados e Quantidade de chuva
# Cria um gráfico com os casos confirmados de dengue em função da quantidade de chuva em milímetros
# Enrico
# 07/04 
dados <- dados %>%
  filter(!is.na(chuva))
largura_faixa <- 50
chuva_min <- floor(min(dados$chuva))
chuva_max <- ceiling(max(dados$chuva))
intervalos_chuva <- seq(chuva_min, chuva_max, by = largura_faixa)
dados$faixa_chuva <- cut(
  dados$chuva,
  breaks = intervalos_chuva,
  include.lowest = TRUE,
  labels = paste0(
    head(intervalos_chuva, -1), " - ",
    tail(intervalos_chuva, -1)
  )
)
tabela_freq_faixa_chuva <- dados %>%
  group_by(faixa_chuva) %>%
  summarise(total_casos = sum(casos.confirmados, na.rm = TRUE)) %>%
  arrange(faixa_chuva)
ggplot(tabela_freq_faixa_chuva, aes(x = faixa_chuva, y = total_casos)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(
    title = "Total de Casos Confirmados por Quantidade de Chuva",
    x = "Quantidade de Chuva (mm)",
    y = "Total de Casos Confirmados"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Código para gerar um gráfico de média de casos por mês
# Cria um gráfico com os casos confirmados de dengue em função dos meses do ano
# Mari e Isabela
# 11/04
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
dados <- dados %>%
  mutate(
    date  = ymd(data), 
    mes = month(date, label = TRUE, abbr = TRUE)  # Meses como fator abreviado
  )
dados_agrupados <- dados %>%
  group_by(mes) %>%
  summarise(media_casos = mean(casos.confirmados, na.rm = TRUE))
ggplot(dados_agrupados, aes(x = mes, y = media_casos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Média de Casos Confirmados por Mês",
    x = "Mês",
    y = "Média de Casos Confirmados"
  ) +
  theme_minimal()

# Código para gerar um gráfico com as variáveis casos confirmados, temp. média e quant. chuva
# Cria um gráfico mostrando a relação das três variáveis de interesse
# Mari
# 11/04

dados_medias <- dados %>%
  group_by(mes) %>%
  summarise(
    Casos = mean(casos.confirmados, na.rm = TRUE),
    Temperatura = mean(temperatura.media, na.rm = TRUE),
    Chuva = mean(chuva, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -mes,
    names_to = "Variavel",
    values_to = "Valor"
  )
ggplot(dados_medias, aes(x = mes, y = Valor, color = Variavel, group = Variavel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    name = "Casos Confirmados e Temperatura Média",
    sec.axis = sec_axis(~ . * 1, name = "Chuva Média (mm)") 
  ) +
  scale_color_manual(
    values = c("Casos" = "#e63946", "Temperatura" = "#1d3557", "Chuva" = "#457b9d")
  ) +
  labs(
    title = "Relação Mensal entre Casos Confirmados, Temperatura e Chuva",
    subtitle = "Médias mensais comparadas",
    x = "Mês",
    color = "Variável"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Código para gerar três gráficos separados
# Cria três gráficos mostrando a média de cada variável ao longo dos meses
# Mari
# 11/04
ggplot(dados_medias, aes(x = mes, y = Valor, color = Variavel)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~Variavel, scales = "free_y", ncol = 1) +  
  theme_minimal()


# Código para gerar uma tabela descritiva
# Cria uma tabela contendo todas os valores de interesse (média, dp, mediana etc)
# Isabela
# 12/04
library(psych)
library(gt)
library(dplyr)
vars <- c("chuva", "temperatura.media", "temperatura.mininima", "temperatura.maxima", "casos.confirmados")
estat <- describe(dados[, vars])
variancias <- sapply(dados[, vars], var, na.rm = TRUE) %>% round(2)
estat_df <- estat %>%
  select(n, mean, sd, min, median, max) %>%
  round(2) %>%
  mutate(
    Variância = variancias,
    Variável = rownames(.)
  ) %>%
  select(Variável, n, mean, Variância, sd, min, median, max) %>%
  rename(
    Amostras = n,
    Média = mean,
    Desvio_Padrao = sd,
    Mínimo = min,
    Mediana = median,
    Máximo = max
  ) %>%
  mutate(
    Variável = recode(Variável,
                      "chuva" = "Chuva (mm)",
                      "temperatura.media" = "Temperatura Média (°C)",
                      "temperatura.mininima" = "Temperatura Mínima (°C)",
                      "temperatura.maxima" = "Temperatura Máxima (°C)",
                      "casos.confirmados" = "Casos Confirmados"
    )
  )
gt(estat_df) %>%
  tab_header(
    title = "Estatística Descritiva das Variáveis"
  )

#--------------------------#
# ANÁLISES ESTATÍSTICAS

# Código para verificar normalidade das variáveis
# Utiliza o teste Shapiro-Wilk para verificar se as variáveis se distribuem de maneira normal
# Enrico e André
# 17/04
# Normalidade da chuva:
shapiro.test(dados$chuva)
ggplot(dados, aes(sample = chuva)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot da Chuva", x = "Quantis Teóricos", y = "Quantis Amostrais")
ggplot(dados, aes(x = chuva)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(dados$chuva), sd = sd(dados$chuva)), color = "red") +
  labs(title = "Histograma da Chuva com Curva Normal", x = "Precipitação (mm)", y = "Densidade")
# Chuva não varia normalmente p<<0.05

#Normalidade da Temperatura Média
shapiro.test(dados$temperatura.media)
ggplot(dados, aes(x = temperatura.media)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(dados$temperatura.media), sd = sd(dados$temperatura.media)), color = "red") +
  labs(title = "Histograma da Temperatura Média", x = "Temperatura", y = "Densidade")
ggplot(dados, aes(sample = temperatura.media)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot da Temp Média", x = "Quantis Teóricos", y = "Quantis Amostrais")
# Temperatura média não varia normalmente p<<0.05

# Normalidade do número de casos
shapiro.test(dados$casos.confirmados)
ggplot(dados, aes(x = casos.confirmados)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(dados$casos.confirmados), sd = sd(dados$casos.confirmados)), color = "red") +
  labs(title = "Histograma dos Casos Confirmados com Curva Normal", x = "Num. de Casos", y = "Densidade")
ggplot(dados, aes(sample = casos.confirmados)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot do Num. Casos", x = "Quantis Teóricos", y = "Quantis Amostrais")
# Número de casos não varia normalmente p<<0.05

library(car)
# Verificação normalidade variancias, usando center = median para compensar por outliers
leveneTest(casos.confirmados ~ mes, data = dados, center=median)
# Variâncias são homogêneas

leveneTest(chuva ~ mes, data = dados, center=median)
# Variâncias não são homogêneas

leveneTest(temperatura.media ~ mes, data = dados, center=median)
# Variâncias são homogêneas

# Usando Teste de Kruskal, pois distribuição não é normal
kruskal.test(casos.confirmados ~ mes, data = dados)

library(FSA)
# Gráfico de casos confirmados por mês
boxplot(casos.confirmados ~ mes, data = dados, col = "lightblue", main = "Casos confirmados por mês")

kruskal.test(casos.confirmados ~ chuva, data = dados)
kruskal.test(casos.confirmados ~ temperatura.media, data = dados)

# Teste de Correlação de Spearman Para Chuva e Casos Confirmados
cor.test(dados$chuva, dados$casos.confirmados, method = "spearman")
ggplot(dados, aes(x = chuva, y = casos.confirmados)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Relação entre Chuva e Casos de Dengue")
# Não há correlação entre a quantidade de casos confirmados e a precipitação

# Teste de Correlação de Spearman Para Temperatura Média e Casos Confirmados
cor.test(dados$temperatura.media, dados$casos.confirmados, method = "spearman")
ggplot(dados, aes(x = temperatura.media, y = casos.confirmados)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Relação entre Temperatura Média e Casos de Dengue",
       x = "Temperatura Média (°C)", y = "Casos Confirmados")
# Há uma relação entre a temperatura média e os casos de dengue, mas ela é fraca:
# a temperatura explica apenas 3.9% da variância dos casos (0.19^2)

# Teste de Correlação de Spearman com atraso de 1 mês
dados$temp_lag <- lag(dados$temperatura.media, 1) 
cor.test(dados$temp_lag, dados$casos.confirmados, method = "spearman")
library(ggplot2)
ggplot(dados, aes(x = temp_lag, y = casos.confirmados)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Relação entre Temperatura Defasada e Casos de Dengue",
       x = "Temperatura Média (Lag)", y = "Casos Confirmados")
# Quando adicionamos um atraso de 1 mês, observamos uma correlação mais forte,
# com a temperatura explicando 21% da variação dos casos de dengue
# e se adicionarmos um atraso de 2 meses, essa correlação é ainda maior.
dados$temp_lag <- lag(dados$temperatura.media, 2) 
cor.test(dados$temp_lag, dados$casos.confirmados, method = "spearman")
# rho = 0.59, temperatura explica 35% da variação dos casos de dengue


# Usando Regressão Binomial Negativa, pois variância >> média

# Temperatura direta
library(MASS)
modelo_nb <- glm.nb(casos.confirmados ~ temperatura.media, data = dados)
summary(modelo_nb)
# Para cada aumento de 1ºC na temperatura, há um aumento no número
# de casos em 12%, todavia, p-valor = 0.0596, podemos afirmar apenas uma tendência

# Temperatura defasada em 1 mês
dados$temp_lag <- lag(dados$temperatura.media, 1)
modelo_nb <- glm.nb(casos.confirmados ~ temp_lag, data = dados)
summary(modelo_nb)
# Para cada aumento de 1ºC na temperatura defasada por 1 mês, há um aumento 
#no número de casos em 72%, p-valor < 0.001
library(ggeffects)
predicoes <- ggpredict(modelo_nb, terms = "temp_lag [all]")
plot(predicoes) + 
  labs(title = "Efeito da Temperatura Defasada nos Casos de Dengue",
       x = "Temperatura Defasada (°C)", y = "Casos Previstos")


# Teste de Kruskal-Wallis
# Faz o teste de Kruskal-Wallis, comparando casos confirmados e mês,
# usamos esse, pois as suposições da ANOVA não foram cumpridas. Também cria uma tabela com 
# as comparações que apresentam diferença significativa. 
# Enrico
# 01/05
library(FSA)
library(dplyr)
library(gt)
kruskal.test(casos.confirmados ~ mes, data = dados)
dunn_result <- dunnTest(casos.confirmados ~ mes, data = dados, method = "bonferroni")
pares_significativos <- dunn_result$res %>%
  filter(P.adj < 0.05) %>%
  mutate(
    Z = round(Z, 2),
    P.adj = signif(P.adj, 3)
  ) %>%
  select(Comparison, Z, P.adj)  
gt(pares_significativos) %>%
  tab_header(
    title = "Comparações Significativas (Teste de Dunn)",
    subtitle = "Ajuste de Bonferroni, p < 0,05"
  ) %>%
  cols_label(
    Comparison = "Comparação",
    Z = "Estatística Z",
    P.adj = "p ajustado"
  ) %>%
  fmt_number(columns = Z, decimals = 2) %>%
  fmt_number(columns = P.adj, decimals = 3) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = P.adj < 0.05)
  
