#######################################################################
########## UNIDADE 1 ##################################################
#######################################################################


#--------------------------------------------------
# Carregar o conjunto de dados
data(mtcars)

# Visualisar o conjunto de dados
mtcars

# Verificar dimensão do conjunto de dados
dim(mtcars)

# Listar todas as variáveis
names(mtcars)

# Mais informações sobre o conjunto de dados
help(mtcars)

# Visualizar somente as primeiras 6 linhas do conjunto de dados
head(mtcars)

mtcars[1:6,]
#--------------------------------------------------


#--------------------------------------------------
# Criando Vetores
c(1,3,4,6,7,8,6,4,2,2,2,4,4)

c(1:5, 7:9)

c("Janeiro", "Fevereiro", "Março", "Abril")
#--------------------------------------------------



#--------------------------------------------------
# Criando um subconjunto de dados com : "mpg", "cyl", 
# "hp", "wt", "am", "gear". 
names(mtcars)

carros = mtcars[ ,c(1,2,4,6,9,10)]

carros
#--------------------------------------------------


#--------------------------------------------------
# attach
mpg

carros$mpg
# ou
attach(carros)
mpg
#--------------------------------------------------


#--------------------------------------------------
# Estrutura do banco de dados
str(carros)

# Transformar variável em FATOR (Categórico)
carros$am = as.factor(carros$am)
carros$cyl = as.factor(carros$cyl)
carros$gear = as.factor(carros$gear)

str(carros)
#--------------------------------------------------


#--------------------------------------------------
summary(mpg)



descritivas = function(x)list(
  Média=mean(x),
  Mediana=median(x),
  Max.Min=range(x),
  Amplitude=max(mpg) - min(mpg),
  Variância=var(x),
  DesvioPadrão=sd(x),
  CoeficienteVariação=sd(x)/mean(x)*100,
  Quantis=quantile(x)
)

descritivas(mpg)
#--------------------------------------------------


#--------------------------------------------------
# PLOT

x11()
plot(mpg)



x11()
plot(mpg, type="p", 
     main = "Gráfico de Dispersão de Milhas por Galão por Modelo", 
     xlab="", ylab="Milhas por Galão",pch=25, bg=4, col="red", bty="l",
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
abline(h=10:40, v=1:32, col="gray", lty=3)

#--------------------------------------------------


#--------------------------------------------------
# Histograma

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
#---------------------------------------------------

#---------------------------------------------------
x11()
boxplot(mpg)


x11()
par(mfrow = c(1,3))
boxplot(mpg, xlab="Milhas por Galão", xaxt="n", cex.lab=2.5, cex.axis=1.5)
boxplot(hp, xlab="Potência em Cavalos",xaxt="n", cex.lab=2.5, cex.axis=1.5)
boxplot(wt, xlab="Peso (em 1000 Libras)",xaxt="n", cex.lab=2.5, cex.axis=1.5)
#---------------------------------------------------



#---------------------------------------------------
install.packages("ggplot2")
library("ggplot2")

# Relacionamento das variáveis "hp" com "mpg"
x11()
qplot(hp, mpg, data=carros, geom="point")

x11()
qplot(hp, mpg, data = carros, 
      geom = c("point", "smooth"))


# Mesmo gráfico, agora separando pela variável "cyl"
x11()
qplot(hp, mpg, data = carros, colour = cyl, shape = cyl)
#---------------------------------------------------



#---------------------------------------------------
# elevando o nível:


# é necessário instalar o pacote ggpubr para unir os gráficos.
install.packages("ggpubr")
library("ggpubr")

# Vamos criar uma nova variável, chamada modelo, 
# que irá conter os nomes dos carros analisados.
carros$modelo <- rownames(carros)

# Criando o gráfico de Colunas
Colunas <- ggbarplot(carros, x = "modelo", y = "mpg",
                    fill = "cyl",         # Vamos mudar o preenchimento das barras 
                                          #com as cores dependendo da variável cyl (cilindros)
                    color = "white",      # Ajustar as bordas das barras para branco
                    palette = "jco",      # Alterar paleta de cores, utilizando a "jco"
                    sort.val = "asc",     # Colocar os valores em ordem crescente
                    sort.by.groups = TRUE,# Selecionar as informações por grupo
                    x.text.angle = 90     # Rotacionar o texto do eixo x.
)
# Para visualizarmos o gráfico, basta utilizar como comando, seu nome. Aumentar (ou diminuir) o texto.
Colunas + font("x.text", size = 8)

# Criando o Gráfico de Dispersão. Vamos chama-lo de Dispersao.
Dispersao <- ggscatter(carros, x = "wt", y = "mpg",
                       add = "reg.line",               # Adicionar a linha de regressão (tendência)
                       conf.int = TRUE,                # Adicionar intervalo de confiança 
                       color = "cyl", palette = "jco", # Colori de acordo com a variável cyl (cilindros)
                       shape = "cyl"                   # Alterar a forma do ponto de acordo com a variável cyl
)+
  stat_cor(aes(color = cyl), label.x = 3)       # Acrescentar texto com o Coeficiente de Correlação

#Para visualizar o gráfico
Dispersao

# Agora, vamos gerar os gráficos junstos
# comando para visualizar o gráfico em uma janela externa
x11()
ggarrange(Colunas, Dispersao,  
          labels = c("Gráfico de Colunas", "Gráfico de Dispersão"),
          ncol = 2, nrow = 1)

#---------------------------------------------------




#-------------- Gráficos utilizando o pacote lattice ---------------------

install.packages("lattice")
library("lattice")

# - Gráfico de Densidade da variável Milhas por Galão. 
densidade1 = densityplot(~mpg, data=carros,
                       main="Gráfico de Densidade",
                       xlab="Milhas por Galão",
                       ylab= "Densidade")

# Para visualizar o gráfico
densidade1

# Embora nossa variável "cyl" (quantidade de cilindros) 
# já esteja como categórica, vamos alterar sua notação para indicar o texto, 
# em português, para auxiliar na geração do gráfico
cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4 Cilindros","6 Cilindros","8 Cilindros"))

# Gráfico de Densidade da variável Milhas por Galão, 
# agora separadas pela variável Quantidade de Cilindros
densidade = densityplot(~mpg|cyl.f, data=carros,
                        main="Gráfico de Densidade por Quantidade de Cilindros",
                        xlab="Milhas por Galão",
                        ylab = "Densidade",
                        layout =(c(1,3))) #o comando layout é para indicar que são 3 linhas e 1 coluna 

# Para visualizar o gráfico
densidade

# Faremos a mesma alteração para a variável "gear", 
# que representa a quantidade de marchas de cada modelo.
gear.f<-factor(gear,levels=c(3,4,5), 
               labels=c("3 Marchas","4 Marchas","5 Marchas"))

# Gráfico de Dispersão da variável Milhas por Galão e Peso, separadas por 
# Cilindros e quantidade de Marchas.
dispersao = xyplot(mpg~wt|cyl.f*gear.f, data=carros,
                   main="Gráfico de Dispersão por Cilindros e Marchas",
                   ylab="Milhas por Galão", xlab="Peso do Carro (1000 Libras)")

# Gráfico de Dispersão em 3D, agrupando as variáveis Milhas por Galão, 
# Peso, Potência, separadas por quantidade de cilindros.
dispersao3d = cloud(mpg~wt*hp|cyl.f, data=carros,
                  main="Gráfico de Dispersão por Cilindros 3D",
                  xlab= "Peso", ylab="Potência", 
                  zlab="Mpg",
                  layout=(c(3,1)))

#Agora, vamos construir os gráficos em uma mesma figura. 
# Aqui, como estamos utilizando o pacote lattice, não podemos utilizar o comando par()
x11()
print(densidade, split = c(1, 1, 2, 2), more = TRUE)
print(densidade1, split = c(2, 1, 2, 2), more = TRUE)
print(dispersao, split = c(1, 2, 2, 2), more = TRUE)
print(dispersao3d, split = c(2, 2, 2, 2), more = FALSE)