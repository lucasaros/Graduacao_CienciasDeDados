#######################################################################
########## UNIDADE 4 ##################################################
#######################################################################

library("lattice")
library("ggplot2")
library("car")

setwd("C:\\Users\\tiago\\Desktop\\LIVRO - MODELAGEM ESTATISTICA\\AULAS CONCEITUAIS")

dente = read.csv(file = "Dentes.csv", header = T, sep = ";", 
                 na.strings = "NA", dec = ",")
head(dente)


#==============================================================================
# Wide para long
#------------------------------------------------------------------------------

dentelong   =   reshape(dente, 
                        varying    = names(dente)[3:5], 
                        v.names    = "Medidas",
                        timevar    = "Afastamento", 
                        times      = 1:3,
                        direction  = "long",
                        idvar      ="Pacientes")

head(dentelong)
tail(dentelong)
#===============================================================================

#===============================================================================
# ordenar em rela��o ao Paciente e ao Dente
#-------------------------------------------------------------------------------

data = dentelong[order(dentelong$Dente),]

#-------------------------------------------------------------------------------
# Incluindo a vari�vel trat = tratamento no conjunto de dados



trat2 =  c(rep(c("H","C","P"),3), "H","P", rep(c("H","C","P"),4),"H",
           rep(c("C","P","H"),3), "C","H", rep(c("C","P","H"),4), "C",
           rep(c("P","H","C"),3), "P","C", rep(c("P","H","C"),4), "P")

dentelong$trat = trat2
data = dentelong[order(dentelong$Paciente,dentelong$Dente),]
rownames(data) = 1:dim(data)[1]
head(data)



# Dados como fatores -----------------------------------------------------------
data$trat = as.factor(data$trat)
data$Paciente = as.factor(data$Paciente)
data$Dente = as.factor(data$Dente)
data$Afastamento = as.factor(data$Afastamento)
data$Pacientes = as.factor(data$Pacientes)

data$Medidas = as.numeric(data$Medidas)

str(data)

#===============================================================================
# Escolhendo qual vari�vel iremos utilizar como base de compara��o 
# (no caso, o placebo (Controle), trat=3)

data$trat <- relevel(data$trat, ref = "P")
#===============================================================================

summary(data)
data

#===============================================================================
# Gr�ficos

# Histograma--------------------------------------------------------------------

data

x11();
hist(data$Medidas,breaks=24, main="",
     xlab="Afastamento Gengival (em mil�metros)", ylab="Frequ�ncia", cex.lab=1.7, las=1)
#-------------------------------------------------------------------------------


# Boxplot ----------------------------------------------------------------------
x11();
boxplot(Medidas~trat,data=data,  
        xlab="" , cex.axis=1.5, las=1,
        ylab="Afastamento (mm)", main="", pch=16, col=c("red","blue","green"),
        names=c("Placebo", "Cloridrato de Nafazolina", "Hemostop"))

legend("topleft", c("Placebo", "Col�rio", "Hemostop"), 
       col = c("red","blue","green"), text.col = "red4", lty = c(1),lwd=c(3), 
       pch = c(NA),
       merge = TRUE, cex=1.4,
       bty="n")
#-------------------------------------------------------------------------------
#===============================================================================


x11();

histogram(~Medidas|trat, data=data, nint=20, main="",
          xlab=list(label="Afastamento Gengival Vertical por Tratamento em mil�metros.",cex=1.5), 
          ylab=list(label="Frequ�ncia", cex=1.5),
          strip=strip.custom(factor.levels=c("Placebo", "Col�rio", "Hemostop")),
          layout=c(3,1))




#===============================================================================

# Gr�fico de Perfil 

#install.packages("lattice")
library("lattice")


#------------------ 
# primeiramente, o gr�fico de todos os perfis.(Todas os tratamentos)

x11();
xyplot(Medidas~Afastamento, groups=Pacientes, data=data, type="b", 
       xlab="Afastamento: 1-Linha M�dia, 2-Lado Direito, 3-Lado Esquerdo",
       scales=list(tck=c(1,0), x=list(cex=1.2), y=list(cex=1.5)),
       strip=strip.custom( par.strip.text=list(cex=1.5)))
#----------------

#----------------
# Agora, os gr�ficos para cada tratamento. (a,c e p)
x11()
xyplot(Medidas~Afastamento|trat, groups=Pacientes, layout(1,3),data=data, 
       type="b", pch=16)
#----------------


dentelong$Afastamento  =    as.numeric(dentelong$Afastamento)

#----------------
# Agora, o gr�fico com o valor da vari�ncia e m�dio de cada droga.
dd=aggregate(data$Medidas, by=list(data$trat, data$Afastamento), FUN=mean)
ddf=data.frame(dd)

#Gr�fico de Perfil da Vari�ncia para cada Droga

x11()
xyplot(x~Group.2,groups=Group.1, data=ddf, type="b",pch=16,
       main="", 
       xlab="Afastamento", ylab="Medidas", 
       auto.key=list(space="top", columns=3, 
                     title="Tratamento", cex.title=1.5,
                     lines=TRUE, points=F),
       strip=strip.custom( par.strip.text=list(cex=2)))


names(data)


#==============================================================================
# Modelo Linear Misto ---------------------------------------------------------

install.packages("nlme")
library("nlme")

Modelo1 = lme(Medidas ~ trat,
              random = list(Paciente = ~ 1),     
              data=data, method = "REML")

summary(Modelo1)

x11(width=6, height=6)
plot( Modelo1, adj = -1, cex=1.2, pch=16,
      xlab=list("Valores Ajustado",cex=2, cex.axis=3),
      ylab=list("Res�duos Padronizados", cex=2, cex.axis=2))

library("nima")

x11()
qqPlot(residuals(Modelo1),lwd=2, envelope=.99, pch=20, cex=1, 
       xlab="Quantis Te�ricos", ylab="Quantis dos Res�duos do Modelo" )


# Modelo Linear MULTIN�VEL ----------------------------------------------------

Modelo.Mult = lme(Medidas ~ trat,
                  random = list(Paciente = ~ 1, Dente = ~ 1),  
                  data=data, method = "REML")

summary(Modelo.Mult)

# Compara��o entre modelos aninhados -----------

anova(Modelo1, Modelo.Mult)


# Gr�fico QQPlot para os dois modelos
x11()
par(mfrow = c(2,1))
qqPlot(residuals(Modelo1),lwd=2, envelope=.99, pch=20, cex=1, 
       xlab="Quantis Te�ricos", ylab="Quantis dos Res�duos do Modelo", main="QQ-Plot Modelo Misto" )
qqPlot(residuals(Modelo.Mult),lwd=2, envelope=.99, pch=20, cex=1, 
       xlab="Quantis Te�ricos", ylab="Quantis dos Res�duos do Modelo", main="QQ-Plot Modelo Multin�vel")




#----------------------------------------------
# EXEMPLO LONGITUDINAL


install.packages("lme4")
library("lme4")


sleepstudy[1:20,]
str(sleepstudy)


#----------------------------------------------
# Gr�fico de perfil geral "Lattice"
x11()
xyplot(Reaction~Days, groups=Subject, data=sleepstudy, type="b", 
       xlab="Dias", ylab="Tempo de Rea��o (ms)",
       scales=list(tck=c(2,0), x=list(cex=1.2), y=list(cex=1.5)),
       strip=strip.custom( par.strip.text=list(cex=2)))

#----------------------------------------------
# Gr�fico de perfil individual "ggplot2"
x11()
ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "black") + 
  theme_bw() + facet_wrap(~Subject) +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20))  +
  labs(title = 'Gr�fico de Perfil dos Indiv�duos', 
       x = 'Dias', y = 'Tempo de Rea��o')


#----------------------------------------------
# Modelo Longitudinal 1

Modelo.long = lme(Reaction ~ Days,
                  random = list(Subject = ~ 1),     
                  data=sleepstudy, method = "REML")

summary(Modelo.long)

x11()
qqPlot(residuals(Modelo.long),lwd=2, envelope=.99, pch=20, cex=1, 
       xlab="Quantis Te�ricos", ylab="Quantis dos Res�duos do Modelo" )

# Como verificar exatamente qual � o tempo de rea��o indicado pelo modelo?

coef(Modelo.long)


#----------------------------------------------
# Modelo Longitudinal 2
Modelo.long2 = lme(Reaction ~ Days,
                   random = ~ 1 + Days|Subject,     
                   data=sleepstudy, method = "REML")
summary(Modelo.long2)
coef(Modelo.long2)


anova(Modelo.long, Modelo.long2)

#----------------------------------------------
# QQPLOT de ambos os modelos
x11()
par(mfrow = c(2,1))
qqPlot(residuals(Modelo.long),lwd=2, envelope=.99, pch=20, cex=1, 
       xlab="Quantis Te�ricos", ylab="Quantis dos Res�duos do Modelo", 
       main="QQ-Plot Modelo Longitudinal (Intercepto Aleat�rio)" )
qqPlot(residuals(Modelo.long2),lwd=2, envelope=.99, pch=20, cex=1, 
       xlab="Quantis Te�ricos", ylab="Quantis dos Res�duos do Modelo", 
       main="QQ-Plot Modelo Longitudinal (Intercepto e Inclina��o Aleat�rios)")

