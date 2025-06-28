# Caminho para o diretório
setwd("C://Users//user//Desktop//TRABALHO PBESTATICTICA")

# Carregar bibliotecas
library(dplyr)
library(ggplot2)
library(ggpubr)

# Carregar os dados
dados <- read.csv("dados_corrigidos_numericos.csv", sep=",", dec=".", stringsAsFactors = FALSE)

# Preencher NAs numéricos com média
numericas <- c("Price", "Kilometers", "Power", "Seats", "Doors", "Cylinders", "Year")
for (col in numericas) {
  if (col %in% colnames(dados)) {
    dados[[col]][is.na(dados[[col]])] <- mean(dados[[col]], na.rm = TRUE)
  }
}

# Filtrar Fiat e outra marca aleatória
fiat <- dados %>% filter(Brand == "Fiat")
marca_alvo <- dados %>% filter(Brand != "Fiat") %>% pull(Brand) %>% unique() %>% .[1]
outra <- dados %>% filter(Brand == marca_alvo)

# Estatísticas descritivas
media_fiat_preco <- mean(fiat$Price)
media_fiat_km <- mean(fiat$Kilometers)
media_outro_preco <- mean(outra$Price)
media_outro_km <- mean(outra$Kilometers)

mediana_fiat_preco <- median(fiat$Price)
mediana_fiat_km <- median(fiat$Kilometers)

sd_fiat_preco <- sd(fiat$Price)
sd_fiat_km <- sd(fiat$Kilometers)

# Amplitude
amplitude_fiat_km <- range(fiat$Kilometers)
amplitude_fiat_preco <- range(fiat$Price)

# IIQ
iiq_km <- IQR(fiat$Kilometers)
iiQ_preco <- IQR(fiat$Price)

# Histogramas com curvas teóricas
x1 <- seq(min(fiat$Kilometers), max(fiat$Kilometers), by = 1)
ftexp <- dexp(x1, rate = 1 / mean(fiat$Kilometers))

hist(fiat$Kilometers, xlab = "Kilometragem de carros Fiat", ylab = "Frequência", main = "", freq = FALSE)
lines(x1, ftexp, col = "red")

x2 <- seq(min(fiat$Price), max(fiat$Price), by = 1)
ftnorm <- dnorm(x2, mean = mean(fiat$Price), sd = sd(fiat$Price))

hist(fiat$Price, xlab = "Preço de carros Fiat", ylab = "Frequência", main = "", freq = FALSE)
lines(x2, ftnorm, col = "red")

# Correlação Preço x Potência
plot(fiat$Power, fiat$Price)
cor(fiat$Power, fiat$Price, method = "pearson")

# Correlação Preço x Ano
plot(fiat$Year, fiat$Price)
cor(fiat$Year, fiat$Price, method = "pearson")

# ANOVA para Preço x Gearbox
anova_resultado <- aov(Price ~ Gearbox, data = fiat)
summary(anova_resultado)

# Testes de Hipótese
cat("\n--- Teste de Hipótese: Preço ---\n")
print(t.test(fiat$Price, outra$Price))

cat("\n--- Teste de Hipótese: Quilometragem ---\n")
print(t.test(fiat$Kilometers, outra$Kilometers))

# Histogramas com curvas teóricas para outra marca
df_chi <- round(mean(outra$Price)^2 / var(outra$Price))

hist_norm <- ggplot(outra, aes(x = Price)) +
  geom_histogram(aes(y = after_stat(density)), fill = "skyblue", bins = 30, alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(outra$Price), sd = sd(outra$Price)),
                color = "darkblue", size = 1.2) +
  ggtitle(paste("Histograma com curva Normal -", marca_alvo)) +
  theme_minimal()

hist_chi <- ggplot(outra, aes(x = Price)) +
  geom_histogram(aes(y = after_stat(density)), fill = "orange", bins = 30, alpha = 0.7) +
  stat_function(fun = dchisq, args = list(df = df_chi), color = "red", size = 1.2) +
  ggtitle(paste("Histograma com curva Qui-Quadrado (df =", df_chi, ") -", marca_alvo)) +
  theme_minimal()

ggarrange(hist_norm, hist_chi, ncol = 2, nrow = 1)