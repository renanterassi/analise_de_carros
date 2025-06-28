setwd("C:/Users/ricar/OneDrive/Desktop/UTFPR/PROBABILIDADE E ESTATISTICA/trabalho")


# ====================================
# ANÁLISE ESTATÍSTICA DO PREÇO DE CARROS
# ====================================

# Carregando bibliotecas
library(tidyverse)

# Importando os dados
dados <- read.csv("dados_corrigidos_numericos.csv")

# ===============================
# 1. Comportamento da variável Preço
# ===============================

# Estatísticas descritivas
summary(dados$Price)

# Histograma da variável preço
ggplot(dados, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribuição dos Preços dos Carros", x = "Preço", y = "Frequência") +
  theme_minimal()

# Boxplot do preço
ggplot(dados, aes(y = Price)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot do Preço dos Carros", y = "Preço") +
  theme_minimal()

# ===============================
# 2. Diferença nos preços médios entre tipos de carroceria
# ===============================

# Boxplot por tipo de carroceria
ggplot(dados, aes(x = Body.Type, y = Price)) +
  geom_boxplot(fill = "darkcyan") +
  labs(title = "Preço por Tipo de Carroceria", x = "Tipo de Carroceria", y = "Preço") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANOVA entre tipos de carroceria
anova_model <- aov(Price ~ Body.Type, data = dados)
summary(anova_model)

# ===============================
# 3. Influência da potência e quilometragem no preço
# ===============================

# Correlações
cor(dados$Power, dados$Price, use = "complete.obs")
cor(dados$Kilometers, dados$Price, use = "complete.obs")

# Gráfico de dispersão: Potência vs Preço
ggplot(dados, aes(x = Power, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Preço vs Potência", x = "Potência do Motor", y = "Preço") +
  theme_minimal()

# Gráfico de dispersão: Km vs Preço
ggplot(dados, aes(x = Kilometers, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Preço vs Quilometragem", x = "Km Rodados", y = "Preço") +
  theme_minimal()

# Regressão linear múltipla
modelo <- lm(Price ~ Power + Kilometers, data = dados)
summary(modelo)

# ===============================
# 4. Padrões de preço por faixa de quilometragem
# ===============================

# Criar faixas de quilometragem
dados <- dados %>%
  mutate(km_faixa = case_when(
    Kilometers < 50000 ~ "Baixa",
    Kilometers >= 50000 & Kilometers < 150000 ~ "Média",
    Kilometers >= 150000 ~ "Alta"
  ))

# Boxplot por faixa de quilometragem
ggplot(dados, aes(x = km_faixa, y = Price)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Preço por Faixa de Quilometragem", x = "Faixa de Km", y = "Preço") +
  theme_minimal()

# ANOVA entre faixas de quilometragem
anova_km <- aov(Price ~ km_faixa, data = dados)
summary(anova_km)


# Gráfico com zoom no eixo X (formato de sino mais claro)
ggplot(dados_limpos, aes(x = z_price)) +
  geom_histogram(aes(y = ..density..), bins = 60, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "darkred", size = 1.3) +
  coord_cartesian(xlim = c(-4, 4)) +  # Limita visualização ao intervalo onde o sino aparece
  labs(title = "Distribuição Normal Padronizada (Formato de Sino)",
       x = "Preço Padronizado (Z-score)",
       y = "Densidade") +
  theme_minimal()


# Calcular a taxa lambda da exponencial
lambda <- 1 / mean(dados_limpos$Price)

# Histograma com curva exponencial ajustada e menos zoom
ggplot(dados_limpos, aes(x = Price)) +
  geom_histogram(aes(y = ..density..), bins = 60, fill = "lightgreen", color = "black") +
  stat_function(fun = dexp, args = list(rate = lambda), color = "darkgreen", size = 1.2) +
  coord_cartesian(xlim = c(0, quantile(dados_limpos$Price, 0.98))) +  # Percentil 98 em vez de 95
  labs(title = "Distribuição Exponencial Ajustada ao Preço dos Carros",
       x = "Preço",
       y = "Densidade") +
  theme_minimal()

# Carregar bibliotecas
library(tidyverse)

# Remover valores ausentes da variável de interesse
dados_limpos <- dados[!is.na(dados$Price), ]

# -------------------------------
# Medidas de Tendência Central
# -------------------------------

# Média
media <- mean(dados_limpos$Price)

# Mediana
mediana <- median(dados_limpos$Price)

# Moda (função customizada)
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
moda_preco <- moda(dados_limpos$Price)

# -------------------------------
# Medidas de Dispersão
# -------------------------------

# Amplitude
amplitude <- diff(range(dados_limpos$Price))

# Desvio padrão
desvio_padrao <- sd(dados_limpos$Price)

# Variância
variancia <- var(dados_limpos$Price)

# Intervalo interquartil
iqr <- IQR(dados_limpos$Price)

# -------------------------------
# Medidas de Posição
# -------------------------------

quartis <- quantile(dados_limpos$Price, probs = c(0.25, 0.5, 0.75))
percentis <- quantile(dados_limpos$Price, probs = c(0.10, 0.90))

# -------------------------------
# Resumo organizado
# -------------------------------

medidas <- data.frame(
  Medida = c("Média", "Mediana", "Moda", "Amplitude", "Desvio Padrão", "Variância", "IQR", 
             "1º Quartil (Q1)", "2º Quartil (Q2)", "3º Quartil (Q3)", 
             "10º Percentil", "90º Percentil"),
  Valor = c(media, mediana, moda_preco, amplitude, desvio_padrao, variancia, iqr,
            quartis[1], quartis[2], quartis[3], percentis[1], percentis[2])
)

# Exibir a tabela (no R Markdown ou relatório)
print(medidas)

# Se quiser em tabela mais bonita no relatório:
# install.packages("knitr") # se ainda não tiver
library(knitr)
kable(medidas, caption = "Medidas Estatísticas do Preço dos Carros")

# Carregar o conjunto de dados
dados <- read.csv("dados_corrigidos_numericos.csv")

# Carregar pacotes
library(dplyr)

# Criar as faixas de quilometragem
dados <- dados %>%
  mutate(km_faixa = case_when(
    Kilometers < 50000 ~ "Baixa",
    Kilometers >= 50000 & Kilometers < 150000 ~ "Média",
    Kilometers >= 150000 ~ "Alta",
    TRUE ~ NA_character_
  ))

# Remover NAs
dados_anova <- na.omit(dados)

# ANOVA
anova_model <- aov(Price ~ km_faixa, data = dados_anova)
summary(anova_model)


# Carregar os dados
dados <- read.csv("dados_corrigidos_numericos.csv", stringsAsFactors = FALSE)

# Remover valores ausentes da variável de interesse
dados <- dados %>% filter(!is.na(Price), Brand %in% c("Fiat", "Chevrolet"))

# Separar os grupos
fiat <- dados %>% filter(Brand == "Fiat") %>% pull(Price)
chevrolet <- dados %>% filter(Brand == "Chevrolet") %>% pull(Price)

# Medianas para referência
cat("Mediana Fiat:", median(fiat), "\n")
cat("Mediana Chevrolet:", median(chevrolet), "\n")

# Teste de Wilcoxon (não paramétrico)
teste <- wilcox.test(fiat, chevrolet, alternative = "two.sided")

# Resultado
print(teste)

# Interpretação simples
if (teste$p.value < 0.05) {
  cat("Rejeitamos H0: Há diferença significativa entre os preços.\n")
} else {
  cat("Não rejeitamos H0: Não há diferença significativa.\n")
}


