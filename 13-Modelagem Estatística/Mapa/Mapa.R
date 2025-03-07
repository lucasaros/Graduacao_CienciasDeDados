library(datasets)
library(ggplot2)

# Carregar conjunto de dados
data("infert")

# Visualizar primeiras linhas do conjunto de dados
head(infert)

# Medidas descritivas para age e parity
summary(infert$age)
summary(infert$parity)

# Gráfico de dispersão entre age e parity
ggplot(infert, aes(x = age, y = parity)) +
  geom_point() +
  labs(x = "Idade", y = "Paridade",
       title = "Gráfico de Dispersão entre Idade e Paridade")


# Gráfico de barras para distribuição de education
ggplot(infert, aes(x = education, fill = education)) +
  geom_bar() +
  labs(x = "Educação", y = "Contagem",
       title = "Distribuição de Mulheres por Nível de Educação") +
  scale_fill_discrete(name = "Educação")


# Gráfico de barras empilhadas para distribuição de induced por education
ggplot(infert, aes(x = education, fill = factor(induced))) +
  geom_bar(position = "stack") +
  labs(x = "Educação", y = "Contagem",
       title = "Distribuição de Induções por Nível de Educação") +
  scale_fill_discrete(name = "Induzido", labels = c("Não Induzido", "Induzido"))






# Construção do modelo de regressão linear simples
modelo <- lm(parity ~ age, data = infert)

# informações de convergência do modelo
summary(modelo)


# Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo)



# Gráfico de dispersão dos resíduos em relação às previsões
plot(modelo, which = 1)


# Instalar e carregar o pacote lmtest
install.packages("lmtest")
library(lmtest)

# Teste de Breusch-Pagan para homogeneidade dos resíduos
bptest(modelo)
