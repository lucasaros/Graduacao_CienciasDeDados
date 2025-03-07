#######################################################################
########## UNIDADE 2 ##################################################
#######################################################################


#----------------------------------------------
# Instalando e carregando pacotes necessários
install.packages("datarium")
install.packages("tidyverse")
install.packages("broom")
install.packages("dplyr")
library('dplyr')
library("datarium")
library("tidyverse")
library("broom")
library("ggplot2")

#----------------------------------------------


#----------------------------------------------
# Carregando o banco de dados
data("marketing", package = "datarium")
# Visualizando as primeiras 25 linhas do banco de dados.
marketing[1:25,]
#----------------------------------------------


#----------------------------------------------
#criação de informações de diagnóstico do modelo (chamado de model)
model <- lm(sales ~ youtube, data = marketing)
attach(marketing)

model.diag.metrics <- augment(model)
head(model.diag.metrics)
#----------------------------------------------

#----------------------------------------------
#criação do gráfico de dispersão com a inserção da reta que 
#representa o modelo e os resíduos marcados em vermelho
x11()
ggplot(model.diag.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 1) +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20)) +
  labs(title = 'Gráfico de Dispersão', x = "Youtube", y = "Sales")


#----------------------------------------------
# Criação de Histograma dos Resíduos
x11()
ggplot(data = marketing, aes(x = model$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Histograma dos Resíduos', x = 'Resíduos', y = 'Frequência')
#----------------------------------------------


#----------------------------------------------
# Gráfico qq-PLOT
install.packages("nima")
library("nima")

x11()
qq_plot(model$residuals)+ theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), 
        text = element_text(size = 20)) +
  labs(title = 'QQ-Plot - Resíduos', 
       x = 'Quantis Teóricos', y = 'Quantis dos Resíduos do Modelo')
#----------------------------------------------


#----------------------------------------------
# Teste de Shapiro-Wilk

# Se p-valor < 0.05, rejetamos a normalidade
shapiro.test(model$residuals)
#----------------------------------------------


#----------------------------------------------
#Gráficos direto do Modelo nos dá informações sobre os resíduos
x11()
# comando para organizar os 4 gráficos em painéis.
par(mfrow=c(2,2))
plot(model)


# O teste de Breush Pagan 
# Se p-valor <0.05, rejeitamos a hipótese de HETEROCEDASTICIDADE
install.packages("lmtest")
library("lmtest")
bptest(model)
#----------------------------------------------


#----------------------------------------------
# EXEMPLO

data(mtcars)

modelo1 = lm(mpg~wt, data=mtcars)
summary(modelo1)

# Criação de QQPlot
x11()
qq_plot(modelo1$residuals)+ theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), 
        text = element_text(size = 20)) +
  labs(title = 'QQ-Plot - Resíduos', 
       x = 'Quantis Teóricos', y = 'Quantis dos Resíduos do Modelo')


# Criação de Histograma dos Resíduos
x11()
ggplot(data = mtcars, aes(x = modelo1$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Histograma dos Resíduos', x = 'Resíduos', y = 'Frequência')

# Gráficos de Resíduos do Modelo
x11()
par(mfrow=c(2,1))
plot(modelo1, c(1,3))

# O teste de Breush Pagan 
# Se p-valor <0.05, rejeitamos a hipótese de HETEROCEDASTICIDADE
install.packages("lmtest")
library("lmtest")
bptest(modelo1)
#----------------------------------------------



