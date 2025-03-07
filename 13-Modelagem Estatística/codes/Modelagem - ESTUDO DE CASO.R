############################################################
########### ESTUDO DE CASO #####################################
##############################################################

#=========================================================================#
library('foreign') # Pacote necessário para ler os arquivos .dbf
library('plyr')    # Pacote necessário para agrupar os data frames
library('car')     # Pacote necessário para agrupar as regionais
#=========================================================================#

#=========================================================================#
# Para organizar o nome do arquivo .dbf quanto aos anos. 
# Todos os meses de 2008 a 2015 e até o mês 5 de 2016.
#-------------------------------------------------------------------------#

(anos=c(paste0(c(paste0(0,8:9), 10:15),
               rep(c(paste0(0,1:9),10:12),
                   each=12)),paste0(160,1:4)))                           
#-------------------------------------------------------------------------#

#=========================================================================#
# Transformar de .dbc para .dbf, e ler o .dbf
#-------------------------------------------------------------------------#

for(ano in anos) 
{
  #RD (reduzidos de AIH) 
  #PR Parana
  arq = paste("RDPR",ano,".dbc",sep="")
  system(paste('E:/MESTRADO/TabWIN/dbf2dbc.exe',arq))
  aux=read.dbf(gsub("dbc","dbf",arq))
  if(ano==anos) vars=colnames(aux)
  print(colnames(aux))
  print(dim(aux))
}


# Vetor do código para os tipos de câncer de mama
mama  =  c("C500", "C501", "C502","C503","C504","C505","C506","C508","C509")


# Cada item da lista representa um mês de informações.Totalizando 101 ítens.
# Para cada item, criamos um data frame com suas informações já separando os filtros
# que utilizaremos: Câncer de mama como Diagnóistico Principal e sexo somente MULHERES.

mes=NULL
for(i in 1:100)
{
  mes[[i]] = subset(dadosPR[[i]], DIAG_PRINC %in% mama & SEXO == '3' 
                    & MUNIC_RES %in% 410001:420000)
}


# ------------------------------------------------------------------------#
# Agrupar todos os subconjuntos já com o Filtro de CANCER de MAMA e SEXO FEMININO
# A library 'plyr' para agruparmos dataframes com quantidades diferentes de colunas

# 2008 - 86  colunas
# 2016 - 113 colunas

df.fill = lapply(ls(pattern = 'mes'), get)
cancer  =  do.call('rbind.fill', df.fill)

#-------------------------------------------------------------------------#

#=========================================================================#
# Para criar var Regional de Saude

municipiosPR=read.table("MunicipiosPR.csv",header=T,sep=";") 
head(municipiosPR)

codigos=list()

for(j in 1:22) { 
  codigos[[j]]=municipiosPR$CodigoMunicipioPR[municipiosPR$RegionalSaudePR==j]%/%10
}



#=========================================================================#
#=========================================================================#
#                                                                         #
#                              AGREGANDO DADOS                            #
#                                                                         #
#=========================================================================#
#=========================================================================#
# Diagnóstico - hospitalizacao Câncer de Mama
# Regional Saúde Paraná - 22 regionais
# Sexo Feminino
# ------------------------------------------------------------------------#




#=========================================================================#
# Acrescentando a variável das regionais de saúde no data frame

cancer$RSCod = recode(cancer$MUNIC_RES, 
                      paste("codigos[[",1:22,"]] ='",1:22,"'", 
                            collapse = ';'))
# ------------------------------------------------------------------------#

#=========================================================================#
# Para acrescentar uma Faixa Etária

faixa <- function(x){
  FaixaEtaria <- ifelse (x <15,   "I",
                         ifelse(x >=15 & x<=24, "II",
                                ifelse(x >=25 & x<=44, "III",
                                       ifelse(x >=45 & x<=64, "IV",
                                              "V" ))))
  return(FaixaEtaria)
}

cancer$FaixaEtaria = as.factor(faixa(cancer$IDADE))


# ------------------------------------------------------------------------#
# Renomeando as variáveis


names(cancer)[names(cancer)=="DIAG_PRINC"] = "DPCID"
names(cancer)[names(cancer)=="MORTE"]      = "Obito"
names(cancer)[names(cancer)=="RACA_COR"]   = "Raca"
names(cancer)[names(cancer)=="ANO_CMPT"]   = "Ano"
names(cancer)[names(cancer)=="US_TOT"]     = "Valor"


# ------------------------------------------------------------------------#
# Selecionando quais variáveis manter no data frame.
cancermama1 = cancer[,c('MUNIC_RES','Valor','DPCID','IDADE','Obito','Raca',
                        'RSCod','Ano','FaixaEtaria')]

#=========================================================================#

# MUNIC_RES continua com 2406 níveis, e o DPCID com 7693


cancermama = transform(cancermama1, DPCID = droplevels(DPCID), 
                        MUNIC_RES = droplevels((MUNIC_RES)))


write.csv2(cancermama, "Cancer_2016.csv", row.names=T)
#=========================================================================#
#=========================================================================#
#=========================================================================#


#=========================================================================#

setwd("C:\\Users\\tiago\\Dropbox\\Cancer2016") 

cancer2016 = read.csv(file = "Cancer_2016.csv", header = T, 
                      sep = ";", na.strings = "NA", dec = ",")


#=========================================================================#

names(cancer2016)

head(cancer2016)
str(cancer2016)

cancer2016$Ano         = as.factor(cancer2016$Ano)
cancer2016$Obito       = as.factor(cancer2016$Obito)
cancer2016$RSCod       = as.factor(cancer2016$RSCod)
cancer2016$Raca        = as.factor(cancer2016$Raca)
cancer2016$DPCID       = as.factor(cancer2016$DPCID)
cancer2016$Raca        = as.factor(cancer2016$Raca)

#=========================================================================#
# Quantidade total de internações por CID 
#=========================================================================#

z=plot(cancer2016$DPCID, yaxt="n", ylim=c(0,12000), cex.axis=1.2)
axis(2,at=seq(0,12000,1000),las=2, cex.axis=1.2)
#legend("topleft", c("TOTAL"), text.col=1, cex=2, bty="n")

text(z,table(cancer2016$DPCID),pos = 3,table(cancer2016$DPCID))


#=========================================================================#
# Quantidade de InternaÃ§Ãµes por CID separado por ano


x11()
par(mfrow=c(3,3))

dados2008 = subset(cancer2016,Ano==2008)
plot(dados2008$DPCID, col=1, yaxt="n", ylim=c(0,1000))
axis(2,at=seq(0,1000,200),las=2, cex.axis=1.5)
legend("topleft", c("2008"), text.col=1, cex=2, bty="n")

dados2009 = subset(cancer2016,Ano==2009)
plot(dados2009$DPCID, col=2, yaxt="n", ylim=c(0,1200))
axis(2,at=seq(0,1200,200),las=2, cex.axis=1.5)
legend("topleft", c("2009"), text.col=2, cex=2, bty="n")

dados2010 = subset(cancer2016,Ano==2010)
plot(dados2010$DPCID, col=3, yaxt="n", ylim=c(0,1200))
axis(2,at=seq(0,1200,200),las=2, cex.axis=1.5)
legend("topleft", c("2010"), text.col=3, cex=2, bty="n")

dados2011 = subset(cancer2016,Ano==2011)
plot(dados2011$DPCID, col=4, yaxt="n", ylim=c(0,1600))
axis(2,at=seq(0,1600,200),las=2, cex.axis=1.5)
legend("topleft", c("2011"), text.col=4, cex=2, bty="n")

dados2012 = subset(cancer2016,Ano==2012)
plot(dados2012$DPCID, col=5, yaxt="n", ylim=c(0,1800))
axis(2,at=seq(0,1800,200),las=2, cex.axis=1.5)
legend("topleft", c("2012"), text.col=5,  cex=2, bty="n")

dados2013 = subset(cancer2016,Ano==2013)
plot(dados2013$DPCID, col=6, yaxt="n", ylim=c(0,1600))
axis(2,at=seq(0,1600,200),las=2, cex.axis=1.5)
legend("topleft", c("2013"), text.col=6,  cex=2, bty="n")

dados2014 = subset(cancer2016,Ano==2014)
plot(dados2014$DPCID, col=7, yaxt="n", ylim=c(0,1600))
axis(2,at=seq(0,1600,200),las=2, cex.axis=1.5)
legend("topleft", c("2014"), text.col=7,  cex=2, bty="n")

dados2015 = subset(cancer2016,Ano==2015)
plot(dados2015$DPCID, col=8, yaxt="n", ylim=c(0,1600))
axis(2,at=seq(0,1600,200),las=2, cex.axis=1.5)
legend("topleft", c("2015"), text.col=8,  cex=2, bty="n")

dados2016 = subset(cancer2016,Ano==2016)
plot(dados2016$DPCID, col=9, yaxt="n", ylim=c(0,1200))
axis(2,at=seq(0,1200,200),las=2, cex.axis=1.5)
legend("topleft", c("2016"), text.col=9,  cex=2, bty="n")

#=========================================================================#

#=========================================================================#
# Quantidade de internaÃ§Ãµes por Regional separado por ano
#=========================================================================#
library(lattice)


cancer2016$rs=rep(1,length(cancer2016$Ano))

data1 <- aggregate(rs~Ano +RSCod,data = cancer2016,FUN = sum)

x11()
xyplot(rs~Ano, groups = RSCod , data1, type="b",ylim=c(0:1400),
       ylab="Internações", col=c(1:23),
       pch=16, lwd=2, cex=1, yaxt="n",
       auto.key=list(space="top", columns=5, 
                     title="Regionais de Saúde", cex=1, lines=F, points=F,
                     text=c("1 - Paranaguá", "2 - Metropolitana", "3 - Ponta Grossa",
                            "4 - Irati", "5 - Guarapuava", "6 - União da Vitória",
                            "7 - Pato Branco", "8 - Francisco Beltrãoo", "9 - Foz do Iguaçu", 
                            "10 - Cascavel", "11 - Campo Mourão", "12 - Umuarama", 
                            "13 - Cianorte", "14 - Paranavaí­", "15 - Maringá", 
                            "16 - Apucarana", "17 - Londrina", "18 - Cornélio Procópio", 
                            "19 - Jacarezinho", "20 - Toledo", "21 - Telêmaco Borba", 
                            "22 - Ivaiporã"),
                     col=c(1:22)))

#=========================================================================#
# Quantidade de internaÃ§Ãµes por faixa etária
#=========================================================================#
#=========================================================================#
# Valor total Gasto
#=========================================================================#
#=========================================================================#
# Quantidade de atendimentos por CID, de 2008 a 2016
#=========================================================================#
11()
par(mfrow=c(3,3))

c500 <- subset(cancer2016, DPCID == "C500" )
plot(c500$Ano, yaxt='n', ylim=c(0,500), col=2, cex.axis=2)
axis(2, seq(0,600,100), cex.axis=1.5, las=2)
legend("topright", c("C50.0"), text.col=2, cex=2, bty="n")

c501 <- subset(cancer2016, DPCID == "C501" )
plot(c501$Ano, yaxt='n', ylim=c(0,600), col=3)
axis(2, seq(0,600,100), cex.axis=1.5, las=2)
legend("top", c("C50.1"), text.col=3, cex=2, bty="n")

c502 <- subset(cancer2016, DPCID == "C502" )
plot(c502$Ano, yaxt='n', ylim=c(0,100), col=4)
axis(2, seq(0,100,20), cex.axis=1.5, las=2)
legend("topleft", c("C50.2"), text.col=4, cex=2, bty="n")

c503 <- subset(cancer2016, DPCID == "C503" )
plot(c503$Ano, yaxt='n', ylim=c(0,600), col=5)
axis(2, seq(0,600,100), cex.axis=1.5, las=2)
legend("topleft", c("C50.3"), text.col=5, cex=2, bty="n")

c504 <- subset(cancer2016, DPCID == "C504" )
plot(c504$Ano, yaxt='n', ylim=c(0,600), col="coral")
axis(2, seq(0,600,100), cex.axis=1.5, las=2)
legend("topleft", c("C50.4"), text.col="coral", cex=2, bty="n")

c505 <- subset(cancer2016, DPCID == "C505" )
plot(c505$Ano, yaxt='n', ylim=c(0,550), col=6)
axis(2, seq(0,500,100), cex.axis=1.5, las=2)
legend("topleft", c("C50.5"), text.col=6, cex=2, bty="n")

c506 <- subset(cancer2016, DPCID == "C506" )
plot(c506$Ano, yaxt='n', ylim=c(0,600), col=7)
axis(2, seq(0,600,100), cex.axis=1.5, las=2)
legend("top", c("C50.6"), text.col=7, cex=2, bty="n")

c508 <- subset(cancer2016, DPCID == "C508" )
plot(c508$Ano, yaxt='n', ylim=c(0,1500), col=8)
axis(2, seq(0,1500,300), cex.axis=1.5, las=2)
legend("topleft", c("C50.8"), text.col=8, cex=2, bty="n")

c509 <- subset(cancer2016, DPCID == "C509" )
plot(c509$Ano, yaxt='n', ylim=c(0,2000), col=9)
axis(2, seq(0,2000,250), cex.axis=1.5, las=2)
legend("topleft", c("C50.9"), text.col=9, cex=2, bty="n")






#=========================================================================#
resmisto  = read.csv(file = "resmisto2.csv", header = T, 
                     sep = ";", na.strings = "NA", dec = ",")

names(resmisto)

# Somente a categoria certa
res = subset(resmisto, DPCID == LEVEL)

x11()
plot(res$Mu, res$res2,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res$res2)) * c(-1,1),
     main="")
abline(h = c(-2,0,2), lty = c(3,2,3),col=c(2,2,2))


# qqplot 

x11()
qqplot(x=qlogis(ppoints(1000)), y=res$res, main="Q-Q Plot",
       xlab="Theoretical Quantiles", ylab= "Data Quantiles")
qqline(res$res, distribution=qlogis)
