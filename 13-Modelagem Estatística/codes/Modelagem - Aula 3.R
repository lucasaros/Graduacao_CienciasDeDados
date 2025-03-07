#######################################################################
########## UNIDADE 3 ##################################################
#######################################################################

#-----------------------------------------
data(iris)

head(iris)
str(iris)
#-----------------------------------------


#-----------------------------------------
# Gráfico de Dispersão entre as variáveis Comprimento e Largura das Pétalas
library("ggplot2")

x11()
scatter <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species), size=5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Largura x Comprimento de Pétalas', 
       x = 'Comprimento das Pétalas', y = 'Largura das Pétalas')
#-----------------------------------------



#-----------------------------------------
# Modelo Linear Simples
Modelo = lm(Petal.Width ~ Petal.Length, data=iris)
Modelo

#-----------------------------------------

# Para conferir a utilização dos parâmetros
summary(Modelo)
#-----------------------------------------



#-----------------------------------------
# O gráfico de Dispersão com o modelo ajustado
x11()
scatter <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species), size=3) + 
  geom_smooth(method="lm", se=FALSE, colour="red")+
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Largura x Comprimento de Pétalas', 
       x = 'Comprimento das Pétalas', y = 'Largura das Pétalas')
#-----------------------------------------



#-----------------------------------------
# SHINY!
install.packages("pairsD3")
library("pairsD3")

shinypairs(marketing)
#-----------------------------------------


#-----------------------------------------
# Modelo Linear Múltiplo
Modelo2 = lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(Modelo2)

#-----------------------------------------
# Modelo sem Newspaper
Modelo2b = lm(sales ~ youtube + facebook, data = marketing)
summary(Modelo2b)
#-----------------------------------------


#-----------------------------------------
# Comparação de Modelos ANOVA
Modelo.Multiplo = lm(sales ~ youtube + facebook , data = marketing)
Modelo.Simples = lm(sales ~ youtube , data = marketing)
anova(Modelo.Simples, Modelo.Multiplo)



# Modelo com Newspaper x sem Newspaper
Modelo.Multiplo = lm(sales ~ youtube + facebook , data = marketing)
Modelo2 = lm(sales ~ youtube + facebook + newspaper, data = marketing)
anova( Modelo.Multiplo, Modelo2)
#-----------------------------------------



#-----------------------------------------
# EXEMPLO Modelo NÃO LINEAR
install.packages("AER")
library(AER) 

data(CASchools)
head(CASchools)
tail(CASchools)

# Criando a variável Pontuação
CASchools$pontuacao <- (CASchools$read + CASchools$math) / 2 

# Gráfico de Dispersão entre a renda e a pontuação
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de Dólares)", 
     ylab = "Pontuação",
     cex.lab = 1.5,cex.main=2,
     main = "Pontuação X Renda do Distrito")


# Modelo Quadrático

Modelo.Quad = lm(pontuacao ~ income + I(income^2), data = CASchools)
summary(Modelo.Quad)


# Gráfico de Dispersão com o Modelo Quadrático
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de Dólares)", 
     ylab = "Pontuação",
     cex.lab = 1.5,cex.main=2,
     main = "Pontuação X Renda do Distrito")
order_id <- order(CASchools$income)

lines(x = CASchools$income[order_id], 
      y = fitted(Modelo.Quad)[order_id],
      col = "red", 
      lwd = 2) 


# Modelo Linear-Log
Modelo.Log = lm(pontuacao ~ log(income), data = CASchools)
summary(Modelo.Log)


# Gráfico de Dispersão com o Modelo Quadrático x Modelo Linear-Log
x11()
plot(CASchools$income, CASchools$pontuacao,
     col = "steelblue",
     pch = 20, lwd=5, 
     xlab = "Renda do Distrito (Milhares de Dólares)", 
     ylab = "Pontuação",
     cex.lab = 1.5,cex.main=2,
     main = "Pontuação X Renda do Distrito")
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
       legend = c("Modelo Quadrático", "Modelo Linear-Log"),
       lwd = 2, cex=1.5,
       col = c("red", "green"), lty=c(2,1))
