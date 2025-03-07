#######################################################################
########## UNIDADE 3 ##################################################
#######################################################################

#-----------------------------------------
data(iris)

head(iris)
str(iris)
#-----------------------------------------


#-----------------------------------------
# Gr�fico de Dispers�o entre as vari�veis Comprimento e Largura das P�talas
library("ggplot2")

x11()
scatter <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species), size=5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Largura x Comprimento de P�talas', 
       x = 'Comprimento das P�talas', y = 'Largura das P�talas')
#-----------------------------------------



#-----------------------------------------
# Modelo Linear Simples
Modelo = lm(Petal.Width ~ Petal.Length, data=iris)
Modelo

#-----------------------------------------

# Para conferir a utiliza��o dos par�metros
summary(Modelo)
#-----------------------------------------



#-----------------------------------------
# O gr�fico de Dispers�o com o modelo ajustado
x11()
scatter <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species), size=3) + 
  geom_smooth(method="lm", se=FALSE, colour="red")+
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Largura x Comprimento de P�talas', 
       x = 'Comprimento das P�talas', y = 'Largura das P�talas')
#-----------------------------------------



#-----------------------------------------
# SHINY!
install.packages("pairsD3")
library("pairsD3")

shinypairs(marketing)
#-----------------------------------------


#-----------------------------------------
# Modelo Linear M�ltiplo
Modelo2 = lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(Modelo2)

#-----------------------------------------
# Modelo sem Newspaper
Modelo2b = lm(sales ~ youtube + facebook, data = marketing)
summary(Modelo2b)
#-----------------------------------------


#-----------------------------------------
# Compara��o de Modelos ANOVA
Modelo.Multiplo = lm(sales ~ youtube + facebook , data = marketing)
Modelo.Simples = lm(sales ~ youtube , data = marketing)
anova(Modelo.Simples, Modelo.Multiplo)



# Modelo com Newspaper x sem Newspaper
Modelo.Multiplo = lm(sales ~ youtube + facebook , data = marketing)
Modelo2 = lm(sales ~ youtube + facebook + newspaper, data = marketing)
anova( Modelo.Multiplo, Modelo2)
#-----------------------------------------



#-----------------------------------------
# EXEMPLO Modelo N�O LINEAR
install.packages("AER")
library(AER) 

data(CASchools)
head(CASchools)
tail(CASchools)

# Criando a vari�vel Pontua��o
CASchools$pontuacao <- (CASchools$read + CASchools$math) / 2 

# Gr�fico de Dispers�o entre a renda e a pontua��o
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de D�lares)", 
     ylab = "Pontua��o",
     cex.lab = 1.5,cex.main=2,
     main = "Pontua��o X Renda do Distrito")


# Modelo Quadr�tico

Modelo.Quad = lm(pontuacao ~ income + I(income^2), data = CASchools)
summary(Modelo.Quad)


# Gr�fico de Dispers�o com o Modelo Quadr�tico
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de D�lares)", 
     ylab = "Pontua��o",
     cex.lab = 1.5,cex.main=2,
     main = "Pontua��o X Renda do Distrito")
order_id <- order(CASchools$income)

lines(x = CASchools$income[order_id], 
      y = fitted(Modelo.Quad)[order_id],
      col = "red", 
      lwd = 2) 


# Modelo Linear-Log
Modelo.Log = lm(pontuacao ~ log(income), data = CASchools)
summary(Modelo.Log)


# Gr�fico de Dispers�o com o Modelo Quadr�tico x Modelo Linear-Log
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de D�lares)", 
     ylab = "Pontua��o",
     cex.lab = 1.5,cex.main=2,
     main = "Pontua��o X Renda do Distrito")
order_id <- order(CASchools$income)

lines(x = CASchools$income[order_id], 
      y = fitted(Modelo.Quad)[order_id],
      col = "red", lty=2,
      lwd = 4) 
lines(CASchools$income[order_id], 
      fitted(Modelo.Log)[order_id], 
      col = "green", 
      lwd = 4)
legend("bottomright",
       legend = c("Modelo Quadr�tico", "Modelo Linear-Log"),
       lwd = 2, cex=1.5,
       col = c("red", "green"), lty=c(2,1))
