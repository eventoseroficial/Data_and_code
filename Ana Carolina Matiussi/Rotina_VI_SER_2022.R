### Rotina para metodologia BM (block maxima) e para POT (peaks over threshold) ###

rm(list=ls(all=TRUE)) ### limpar memória ###

### Pacotes utilizados para análise dos valores extremos ###
library(evd) ### funções para as distribuições de valores extremos ###
library(extRemes) ### funções gerais para realizar análises de valores extremos ###
library(hydroGOF) ### funções de qualidade de ajuste ###

### Exemplo de pacotes para um uso específico ###
library(evir) ### uma alternativa -> funções para teoria de valores extremos ###
library(revdbayes) ### funções para a análise bayesiana de modelos de valores extremos ###
library(fExtremes) ### modelagem de eventos extremos em Finanças ###
library(SpatialExtremes) ### modelar extremos Espaciais ###
library(lmom) ### funções para o uso do método de momentos-L ###
### entre outros pacotes auxiliares ###

############################# metodologia BM (block maxima) ############################# 
### carregar dataset ### 
dados=read.csv(file.choose(), header=TRUE, sep=";", dec=",")
dados
head(dados)
tail(dados)
attach(dados)

dados2=aggregate(var~ano+mês, data=dados,max);dados2

Y1=dados2$var; Y1
length(Y1)
summary(Y1)
summary(dados2)

############################# Gráfico da série ############################# 
### transformando em serie temporal ###
dados3<- ts(Y1, start=1, end=15)
dados3
plot(Y1,type="l", ylab="Número de mortes", 
     xlab="Tempo", col="red", bty="l", 
     panel.first=grid())

### teste de Mann-Kendall ### 
require(trend)
mk.test(Y1) ### teste de tendência ###

### teste de Ljung-Box ###
Box.test(Y1, type = c("Ljung-Box")) ### teste de independência ###

############################# Distribuições #############################
### ajuste da distribuição Gumbel ###
fit1 <- fevd(var, dados2, type="Gumbel", period="month")
fit1 
loc1=fit1$results$par[1]
scale1=fit1$results$par[2]

### ajuste da distirbuição GEV ###
fit2 <- fevd(var, dados2, type ="GEV", period="month")
fit2
loc2=fit2$results$par[1]
scale2=fit2$results$par[2]
shape2=fit2$results$par[3]

### diagnostico geral ###
plot(fit1, main="") 
plot(fit2, main="") 
rl1=return.level(fit1,return.period=c(seq(2,10,1)),time.units="months",
                 period="months",do.ci=TRUE)
rl1

rl2=return.level(fit2,return.period=c(seq(2,10,1)),time.units="months",
                 period="months",do.ci=TRUE)
rl2

### teste da razão de verossimilhança ###
lr.test(fit1, fit2) ### analisar o p-valor para a escolha da distribuição (GVE ou Gumbel) ###
### para avaliar qual distribuição de valores extremos se ajustam melhor aos dados ###

### Teste Kolmogorov-Smirnov -> avaliar a qualidade do ajuste ###
ks.test(Y1,"pgumbel", loc1, scale1) ### Gumbel ###
ks.test(Y1,"pgev", loc2, scale2, shape2) ### GVE ###

### Gráfico de diagnostico ###
plot(fit1, "probprob", main="") ### Gráfico percentil-percentil (PP) ### 
plot(fit1, "qq2", xlab="Empirical Quantiles") ### gráfico quantil-quantil (QQ) ###
plot(fit1, "density", main="") ### densidade dos dados vs modelo ajustado ###

### diagnostico pelos gráficos de Nível de retorno ###
plot(fit1, "rl", main="Gumbel") ### Gumbel ###
plot(fit2, "rl", main="GVE") ### GVE ###

############################# Probabilidades #############################
nivel<-c(1000,2000,3000,4000,5000) ### niveis de interesse ###
round(pevd(nivel,loc1,scale1,type="Gumbel",lower.tail=FALSE)*100,2) ### Gumbel ###
round(pevd(nivel,loc2,scale2,shape2,type="GEV",lower.tail=FALSE)*100,2) ### GVE ###

############################# Tempo de retorno #############################

tr<-c(seq(2,10,1)) ### Tempos de retorno armazenados no vetor x ###
p<-1-(1/tr)  ### Probabilidades para cada um dos tempos de retorno do vetor x ###

ret1=qgumbel(p,loc1, scale1) ### niveis de retorno pela Gumbel ###
ret1
ret2=qgev(p,loc2, scale2, shape2) ### niveis de retorno pela GVE ###
ret2

############################# gráficos do nivel de retorno #############################
par(mfrow=c(1,1))
### Gumbel ###
plot(tr,ret1,type ='b', pch=19, col = "red", lwd=2, xaxt="n", main="Distribuição", 
     xlab='Time of return (month)', ylab='Return level', ylim=c(1000,5000), cex.lab=1.5, 
     cex.axis=1.2, cex.main=1.5,las=2, panel.first=grid(),bty="L")
lines(tr, rl1[,1], type ='b', lty=2, pch=19, col = 2, lwd=2)
lines(tr, rl1[,3], type ='b', lty=2, pch=19, col = 2, lwd=2)
axis(1, at=c(seq(2,10,1)),cex.axis=1.5)
### GVE ###
lines(tr, ret2, type ='b', lty=2, pch=19, col = 1, lwd=2)
lines(tr, rl2[,1], type ='b', lty=2, pch=19, col = 1, lwd=2)
lines(tr, rl2[,3], type ='b', lty=2, pch=19, col = 1, lwd=2)
### legenda ###
legend("topright", legend=c("Gumbel","GVE"), lty = c(1,1), lwd = c(2,2), 
       pch=c(16,16),col=c("red","black"), bty="n")

############################# metodologia POT (peaks over threshold) ############################# 
### carregar dataset ### 
dados=read.csv(file.choose(), header=TRUE, sep=";", dec=",")
dados
head(dados)
tail(dados)
attach(dados)

Y1=dados$var; Y1
length(Y1)
############################# Gráfico da série ############################# 
### transformando em serie temporal ###
dados2 <- ts(Y1)
dados2
plot(dados2,type="l", ylab="Número de mortes", 
     xlab="Tempo", col="red", bty="l", 
     panel.first=grid())

### gráfico da media dos excedentes ###
mrlplot(Y1, main="Mean Residual Life Plot", 
        col=c("blue", "black", "blue"))

### gráfico para escolha do threshold ###
par(mfrow=c(1,2))
tlim = c(0,3000)
tcplot(Y1,tlim,std.err= FALSE) ### threshold choice plot ###
ts<-2800 ### threshold selecionado ###

dadosmod<-Y1[Y1>ts] ### máximos acima de um limiar ###
length(dadosmod)

############################# Mesma estrutura da primeira metodologia  ############################# 
### teste de Ljung-Box 
### teste de Mann-Kendall 
### ajuste das distribuições
### teste da razão de verossimilhança 
### teste Kolmogorov-Smirnov
### probabilidades 
### tempo de retorno

#####################################################################################
###    COLES, Stuart. An Introduction to Statistical Modeling of Extreme Values.  ### 
###            Springer series in statistics, v. 1, no 1, p. 1-219, 2001.         ### 
###            Disponível em: https://doi.org/10.1007/978-1-4471-3675-0.          ### 
#####################################################################################

