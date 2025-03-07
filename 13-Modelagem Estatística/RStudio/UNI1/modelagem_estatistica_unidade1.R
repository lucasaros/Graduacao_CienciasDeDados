#############################################################################
########### UNIDADE 1 #######################################################
#############################################################################

#---------------------------------------
# Carregar o conjunto de dados
data(mtcars)

# Visualizar o conjunto de dados
mtcars

# Tipo do conjunto de dados
class(mtcars)

# Dimensão do conjunto de dados
dim(mtcars)

# Listar todas as variáveis
names(mtcars)

# Seleciona todas as linhas, e colunas de 1 a 6 
mtcars[1:6]

# seleciona todas as colunas, e linhas de 1 a 6
mtcars[1:6,]

# Selecionando todas as linhas, e as colunas que serão utilizadas
carros = mtcars[,c(1,2,4,6,9,10)]
carros

# Estrutura do banco de dados
str(carros)

# Transformar variável em FATOR(categórico)
carros$am = as.factor(carros$am)
carros$cyl = as.factor(carros$cyl)
carros$gear = as.factor(carros$gear)

carros$mpg
# ou
attach(carros)
mpg

summary(mpg)

# Função criada
descritivas = function(x)list(
  Média=mean(x),
  Mediana=median(x),
  Max.Min=range(x),
  Amplitude=max(mpg) - min(mpg),
  Variancia=var(x),
  DesvioPadrão=sd(x),
  CoeficienteVariação=sd(x)/mean(x)*100,
  Quantis=quantile(x)
)
descritivas(mpg)

# Gráficos
plot(mpg)

#Gerar o gráfico em uma tela separada no RStudio
x11()
#Gerar a base do gráfico
plot(mpg, type="p", main = "Gráfico de Dispersão de Milhas por Galão 
por Modelo", 
     xlab="", ylab="Milhas por Galão", pch=25, bg=4, col="red", bty="l",
     las=1, cex=1.5, xlim=c(0,32), ylim=c(10, 35), xaxt="n")
#adicionando o texto de valor acima do ponto
text(mpg, labels=mpg, pos=3, col=1, cex=1.2)
#adicionando as informações do eixo x
axis(1, at=1:32, labels=FALSE)
text(seq(1,32, by=1),
     #mover os nomes para baixo
     par("usr")[3] - 0.2, 
     #identificar o texto do eixo x 
     labels = row.names(carros), 
     #rotacionar o texto em 45 graus
     srt = 45, 
     #ajustar o quão próximo do eixo está o texto
     adj=1.1, 
     #Ajustar a região de texto
     xpd = NA, 
     #Ajustar o tamanho do texto
     cex=0.8)
#adicionar a grade ao gráfico
abline(h=10:40, v=1:32, col="gray", lty=3)


#Histograma
x11() 
hist(mpg)

x11() 
hist(mpg, 
     #selecionando intervalos de 2 e 2 unidades
     breaks=seq(10,34,2), 
     #alterando escala do eixo x para os valores entre 10 e 34
     xlim=c(10, 34), 
     #nome para o título, eixo y e eixo x
     main="Histograma de Milhas por Galão",
     ylab="Frequência", xlab="Milhas por Galão", 
     #inserindo cor
     col="lightblue",
     #alterando o tamanho do texto
     xaxt="n", cex.lab=1.5, cex.axis=1.5, 
     #alterando a orientação do eixo y
     las=2)
#Definindo o eixo x
axis(1, at=seq(10, 34, by=2), cex.axis=1.5)



#Variabilidade
x11()
boxplot(mpg)

x11()
par(mfrow = c(1,3))
boxplot(mpg, xlab="Milhas por Galão")
boxplot(hp, xlab="Potência em Cavalos")
boxplot(wt, xlab="Peso (em 1000 Libras)")


#Instalando pacotes
install.packages("ggplot2")
library("ggplot2")

# Relacionando variáveis "hp" com "mpg"
x11()
qplot(hp, mpg, data=carros, geom="point")

x11()
qplot(hp, mpg, data = carros, geom = c("point", "smooth"))

# Mesmo gráfico, agora separando pela variável "cyl"
x11()
qplot(hp, mpg, data = carros, colour = cyl, shape = cyl)


#Instalando o pacote "ggpubr" para unir os gráficos
install.packages("ggpubr")

# a instalação é necessária somente a primeira vez, depois, basta carregar utilizando o comando library()
library("ggpubr")
x11()
# Vamos criar uma nova variável, chamada modelo, que irá conter os nomes dos carros analisados.
carros$modelo <- rownames(carros)
# Criando o gráfico de barras (que na verdade será de colunas). Vamos chama-lo de Barras.
Barras <- ggbarplot(carros, x = "modelo", y = "mpg",
                    fill = "cyl", # Vamos mudar o preenchimento das barras com as cores dependendo da variável cyl (cilindros)
                    color = "white", # Ajustar as bordas das barras para branco
                    palette = "jco", # Alterar paleta de cores, utilizando a “jco”
                    sort.val = "asc", # Colocar os valores em ordem crescente
                    sort.by.groups = TRUE, # Selecionar as informações por grupo
                    x.text.angle = 90 # Rotacionar o texto do eixo x.
)
# Para visualizarmos o gráfico, basta utilizar como comando, seu nome. 
#Aumentar (ou diminuir) o texto.
Barras + font("x.text", size = 8)
# Criando o Gráfico de Dispersão. Vamos chama-lo de Dispersao.
Dispersao <- ggscatter(carros, x = "wt", y = "mpg",
                       add = "reg.line", # Adicionar a linha de regressão (tendência)
                       conf.int = TRUE, # Adicionar intervalo de confiança 
                       color = "cyl", palette = "jco", # Colori de acordo com a variável cyl 
                       (cilindros)
                       shape = "cyl" # Alterar a forma do ponto de acordo 
                       com a variável cyl
)+
  stat_cor(aes(color = cyl), label.x = 3) # Acrescentar texto com o Coeficiente de 
Correlação
#Para visualizar o gráfico
Dispersao
# Agora, vamos gerar os gráficos juntos
# comando para visualizar o gráfico em uma janela externa
x11()
ggarrange(Barras, Dispersao, 
          labels = c("Gráfico de Colunas", "Gráfico de Dispersão"),
          ncol = 2, nrow = 1)




# instalando o pacote "lattice"
install.packages("lattice")
# a instalação é necessária somente a primeira vez, depois, basta carregar 
utilizando o comando library()
library("lattice")
# - Gráfico de Densidade da variável Milhas por Galão. Daremos o nome de densidade1
x11()
densidade1=densityplot(~mpg,
                       main="Gráfico de Densidade",
                       xlab="Milhas por Galão",
                       ylab= "Densidade")


cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4 Cilindros","6 Cilindros","8 Cilindros"))
# Gráfico de Densidade da variável Milhas por Galão, agora separadas pela variável Quantidade de Cilindros
densidade = densityplot(~mpg|cyl.f,
                        main="Gráfico de Densidade por Quantidade de Cilindros",
                        xlab="Milhas por Galão",
                        ylab = "Densidade",
                        layout =(c(1,3))) #o comando layout é para indicar que são 3 linhas e 1 coluna 
# Faremos a mesma alteração para a variável “gear”, que representa a quantidade de marchas de cada modelo.
gear.f<-factor(gear,levels=c(3,4,5),
               labels=c("3 Marchas","4 Marchas","5 Marchas"))
# Gráfico de Dispersão da variável Milhas por Galão e Peso, separadas por Cilindros e quantidade de Marchas.
dispersao = xyplot(mpg~wt|cyl.f*gear.f,
                   main="Gráfico de Dispersão por Cilindros e Marchas",
                   ylab="Milhas por Galão", xlab="Peso do Carro (1000 Libras)")
# Gráfico de Dispersão em 3D, agrupando as variáveis Milhas por Galão, Peso, Potência, separadas por quantidade de cilindros.
dispersao3d=cloud(mpg~wt*hp|cyl.f,
                  main="Gráfico de Dispersão por Cilindros 3D",
                  xlab= "Peso", ylab="Potência", 
                  zlab="Mpg",
                  layout=(c(3,1)))
#Agora, vamos construir os gráficos em uma mesma figura. Aqui, como estamos utilizando o pacote lattice, não podemos utilizar o comando par()
x11()
print(densidade, split = c(1, 1, 2, 2), more = TRUE)

densidade1


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





