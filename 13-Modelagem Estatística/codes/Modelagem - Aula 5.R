############################################################
########### UNIDADE 5 #####################################
##############################################################

#----------------------------------------------
# Carregando o pacote do Conjunto de Dados
install.packages("datasets")
library("datasets")

data = warpbreaks


#----------------------------------------------
# Conferindo as primeiras linhas e a estrutura dos dados
head(data)

ls.str(data)
#----------------------------------------------



#----------------------------------------------
# Histograma

x11() 
hist(data$breaks, 
     #alterando escala do eixo x para os valores entre 10 e 70
     xlim=c(10,70), 
     #nome para o título, eixo y e eixo x
     main="Histograma da quantidade de quebras",
     ylab="Frequência", xlab="Quantidade de Quebras", 
     #inserindo cor
     col="lightblue",
     #alterando o tamanho do texto
     xaxt="n", cex.lab=1.5, cex.axis=1.5, 
     #alterando a orientação do eixo y
     las=2)
#Definindo o eixo x
axis(1, at=seq(10,70, by=10), cex.axis=1.5)

#----------------------------------------------


#----------------------------------------------

# Cálculo da Média
mean(data$breaks) 
# Cálculo da Variância
var(data$breaks)


# ATENÇÃO PARA O TAMANHO DA VARIÂNCIA - super-dispersão 
#----------------------------------------------



#----------------------------------------------
# Modelo Poisson para contagem

ModeloPoisson = glm(breaks ~ wool + tension, 
                    data, 
                    family = poisson(link = "log"))
summary(ModeloPoisson)

str(data)
#----------------------------------------------


#----------------------------------------------
# Modelo Poisson para contagem - quasipoisson

ModeloPoisson2 = glm(breaks ~ wool + tension, 
                     data, 
                     family = quasipoisson(link = "log"))
summary(ModeloPoisson2)


#----------------------------------------------
# Listar os coeficientes do modelo2 (EXP)

coef = cbind(coef(ModeloPoisson2),
             exponent = exp(coef(ModeloPoisson2))
)
coef
#----------------------------------------------



#----------------------------------------------
# Criando o data frame com as informações.

data2 = data.frame(wool = "B", tension = "M")
data2
#----------------------------------------------
# Utilizar o comando predict para aplicar o modelo nesse novo dataframe

predict(ModeloPoisson2, newdata = data2, type = "response")
#----------------------------------------------


#----------------------------------------------
# QQPLOT

library("car")
x11()
qqPlot(residuals(ModeloPoisson2),lwd=2, envelope=.95, pch=20, cex=1, 
       xlab="Quantis Teóricos", ylab="Quantis dos Resíduos do Modelo", 
       main="QQ-Plot Modelo de Poisson")
#----------------------------------------------
#----------------------------------------------


#----------------------------------------------
#----------------------------------------------
# Conjunto de dados BINOMIAL



#----------------------------------------------
# ler o conjunto de dados (direto da internet)

data = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")


head(data)


#----------------------------------------------

str(data)
data$rank = factor(data$rank)
data$admit = factor(data$admit)
data$gre = as.numeric(data$gre)
data$gpa = as.numeric(data$gpa)
str(data)
#----------------------------------------------



#----------------------------------------------
# Mondelo BINOMIAL

ModeloBinomial = glm(admit ~ gre + gpa + rank, 
                     data = data, family = binomial(link="logit"))

summary(ModeloBinomial)
#----------------------------------------------



#----------------------------------------------
# Coeficientes do modelo binomial

exp(cbind(OR = coef(ModeloBinomial), confint(ModeloBinomial)))
#----------------------------------------------


#----------------------------------------------
# Se utilizar a média de gre e gpa e considerar somente a diferença do RANK
data2 = with(data, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
data2$rankProb <- predict(ModeloBinomial, newdata = data2, type = "response")

data2
#----------------------------------------------


#----------------------------------------------
# Gerando valores aleatórios para o GRE (entre 200 e 800)
data2 <- with(data, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                         4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
data3 <- cbind(data2, predict(ModeloBinomial, newdata = data2, type = "link",
                              se = TRUE))
data3 <- within(data3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})


head(data3)
tail(data3)

#----------------------------------------------
# Probabilidade de admissão: menores gre x maiores gre
data3[c(1, 101, 201, 301),]
data3[c(100,200,300,400),]
#----------------------------------------------


#----------------------------------------------
library("ggplot2")

x11()
ggplot(data3, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + 
  geom_line(aes(colour = rank), size = 1)+
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20)) +
  labs(title = '', x = "gre", y = "Probabilidades Previstas")



