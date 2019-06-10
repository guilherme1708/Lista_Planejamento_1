# Lista 4 - MAE0317

library(car)
library(MASS)

# Exercício 1

# item b

Pulse <- read.csv("Pulse.txt", header=T, sep= ";")
attach(Pulse)
n1 <- 35
n2 <- 57
Tratb <- factor(c(rep(1,n1),rep(2,n2)))
anovb <- data.frame(P2,Tratb)
boxplot(P2~Tratb,xlab="Correu")
title("Boxplot de P2 por Tratamento")

modb <- aov(P2~Tratb,data= anovb) 
anovab <- anova(modb)
knitr::kable(caption = "Tabela de ANOVA", anovab)

s2 <- anovab$"Mean Sq"[2]
par(mfrow=c(2,2))
res <- rstudent(modb) # extraindo resíduos
# Verificação de independência
plot(res, type="l") 
title("Gráfico 2:Resíduos vs índices")
abline(h=0, col="red")

plot(modb$fit, modb$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 3:Resíduos vs Preditos")

hist(res, main=NULL)
title("Gráfico 4:Histograma dos resíduos")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico 5: QQPlot Normal")

#Teste de normalidade de Shapiro-Wilk
shapiro.test(res)  

# Teste de homogeneidade de variâncias

leveneTest(modb)
bartlett.test(P2~Tratb) 

# item d

Dif <- P2 - P1

Tratd <- factor(c(rep(1,n1),rep(2,n2)))
anovd <- data.frame(Dif,Tratd)
boxplot(Dif~Tratd,xlab="Correu")
title("Boxplot de Dif=P2-P1 por Tratamento")

modd <- aov(Dif~Tratd,data= anovd) 
anovad <- anova(modd)
knitr::kable(caption = "Tabela de ANOVA", anovad)


s2 <- anovad$"Mean Sq"[2]

par(mfrow=c(2,2))
res <- rstudent(modd) # extraindo resíduos
# Verificação de independência
plot(res, type="l") # res x índice das observações
title("Gráfico 7: Resíduos vs índices")
abline(h=0, col="red")

plot(modd$fit, modd$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 8:Resíduos vs Preditos")

hist(res, main=NULL)
title("Gráfico 9:Histograma dos resíduos")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico 10: QQPlot Normal")

#Teste de normalidade de Shapiro-Wilk
shapiro.test(res)  

# Teste de homogeneidade de variâncias
leveneTest(modd)

# item e

Medidas <- c(P1[1:35],rep(0,92),P1[36:92],P2[1:35],rep(0,92),P2[36:92])
Bloco <- factor(c(1:92,1:92,1:92,1:92))
correu_ou_ncorreu <- factor(c(rep(1,92),rep(2,92),rep(1,92),rep(2,92)))
inicial_ou_final <- factor(c(rep(1,184),rep(2,184)))
dadose <- data.frame(Medidas,correu_ou_ncorreu,inicial_ou_final,Bloco)
boxplot(Medidas~correu_ou_ncorreu,xlab="Correu")
title("Boxplot das Medidas por Tratamento se Correu ou não")

boxplot(Medidas~inicial_ou_final,xlab="Tempo")
title("Boxplot das Medidas por Tratamento se é tempo inicial ou final")

mode <- aov(Medidas~correu_ou_ncorreu*inicial_ou_final+Bloco)
anovae <- anova(mode)
knitr::kable(caption = "Tabela de ANOVA", anovae)


s2 <- anovae$"Mean Sq"[5]

par(mfrow=c(2,2))
res <- rstudent(mode) # extraindo resíduos
# Verificação de independência
plot(res, type="l") # res x índice das observações
title("Gráfico 13: Resíduos vs índices")
abline(h=0, col="red")

plot(mode$fit, mode$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 14:Resíduos vs Preditos")

hist(res, main=NULL)
title("Gráfico 15: Histograma dos resíduos")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico 16: QQPlot Normal")

# item f

Medidas <- c(P1,P2)
correu_ou_ncorreu <- factor(c(rep(1,35),rep(2,57),rep(1,35),rep(2,57)))
inicial_ou_final <- factor(c(rep(1,92),rep(2,92)))
Bloco <- factor(c(1:92,1:92))
dadosf <- data.frame(Medidas,correu_ou_ncorreu,inicial_ou_final,Bloco)
boxplot(Medidas~correu_ou_ncorreu,xlab="Correu")
title("Boxplot das Medidas por Tratamento se Correu ou não")

boxplot(Medidas~inicial_ou_final,xlab="Tempo")
title("Boxplot das Medidas por Tratamento se é tempo inicial ou final")

modf <- aov(Medidas~correu_ou_ncorreu/inicial_ou_final+Bloco)
anovaf <- anova(modf)
knitr::kable(caption = "Tabela de ANOVA", anovaf)

s2 <- anovaf$"Mean Sq"[4]
par(mfrow=c(2,2))
res <- rstudent(modf) # extraindo resíduos 
# Verificação de independência
plot(res, type="l") # res x índice das observações
title("Gráfico 19: Resíduos vs índices")
abline(h=0, col="red")

plot(modf$fit, modf$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 20: Resíduos vs Preditos")

hist(res, main=NULL)
title("Gráfico 21: Histograma dos resíduos")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico 22: QQPlot Normal")

par(mfrow=c(1,2))
plot.default(inicial_ou_final,res)
title("Gráfico 23: Resíduos vs B(A)")
plot.default(correu_ou_ncorreu,res)
title("Gráfico 24: Resíduos vs A")

#Teste de normalidade de Shapiro-Wilk
shapiro.test(res)  

# Teste de homogeneidade de variâncias
leveneTest(Medidas~correu_ou_ncorreu/inicial_ou_final)

# item g


Medidas <- c(P1,P2)
correu_ou_ncorreu <- factor(c(rep(1,35),rep(2,57),rep(1,35),rep(2,57)))
inicial_ou_final <- factor(c(rep(1,92),rep(2,92)))
Bloco <- factor(c(1:92,1:92))
dadosg <- data.frame(Medidas,correu_ou_ncorreu,inicial_ou_final,Bloco)

boxcox(Medidas ~ correu_ou_ncorreu/inicial_ou_final+Bloco, data=dadosf, plotit=T)

Medidas <- Medidas^-1
dadosg <- data.frame(Medidas,correu_ou_ncorreu,inicial_ou_final,Bloco)
modg <- aov(Medidas~correu_ou_ncorreu/inicial_ou_final+Bloco)
anovag <- anova(modg)
knitr::kable(caption = "Tabela de ANOVA", anovag)

s2 <- anovag$"Mean Sq"[4]

par(mfrow=c(2,2))
res <- rstudent(modg) # extraindo resíduos 
# Verificação de independência
plot(res, type="l") # res x índice das observações
title("Gráfico 24: Resíduos vs índices")
abline(h=0, col="red")

plot(modg$fit, modg$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 25: Resíduos vs Preditos")

hist(res, main=NULL)
title("Gráfico 26: Histograma dos resíduos")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico 27: QQPlot Normal")

par(mfrow=c(1,2))
plot.default(inicial_ou_final,res)
title("Gráfico 28: Resíduos vs B(A)")
plot.default(correu_ou_ncorreu,res)
title("Gráfico 29: Resíduos vs A")

#Teste de normalidade de Shapiro-Wilk
shapiro.test(res)  

# Teste de homogeneidade de variâncias
leveneTest(Medidas~correu_ou_ncorreu/inicial_ou_final)

# Exercício 3

# item a

resp <- c(0.02,0.15,0.45,0.18,0.27,0.24,-0.01,0.58,0.11,0.35,0.14,-0.03,0.48,0.04,0.18,0.22)
Bloco <- factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)))
Var <- factor(c(1,2,3,4,2,3,1,4,3,4,2,1,4,1,3,2))
dat <- data.frame(resp,Var,Bloco)

#ANOVA do DABC
fit1 <- aov(resp ~ Var + Bloco)
as <- anova(fit1)
knitr::kable(caption = "Tabela de ANOVA", as)
res <- rstudent(fit1)

par(mfrow=c(2,2))
plot(fit1$fitted.values,res)
title("Gráfico 30: Resíduos vs índices")
plot.default(dat$Bloco,fit1$residuals)
title("Gráfico 31: Resíduos vs Blocos")
plot.default(dat$Var,fit1$residuals)
title("Gráfico 32: Resíduos vs Tratamentos")
qqnorm(fit1$residuals,ylab="Residuos", main=NULL)
qqline(fit1$residuals)
title("Gráfico 33: QQPlot Normal")

# item b

interaction.plot(Bloco, Var, resp, main="Gráfico 34: Efeito de Interação dos Fatores Var e Bloco")

# item e

tr <- c(0, fit1$coeff[2:4])
tr <- rep(tr, 4)
bl <- c(0, fit1$coeff[5:7])
bl <- rep(bl, each=4)
trbl <- tr * bl #novo fator que mede a interação
datt <- cbind(dat,tr,bl, trbl)

fitna <- aov(resp ~ Var + Bloco + trbl)
d <- anova(fitna) #Interprete. O que está sendo testado?
knitr::kable(caption = "Tabela de ANOVA", d)

#Estudo do efeito de Tratamento
fit1.tk <- TukeyHSD(fit1, "Var")
fit1.tk

plot(fit1.tk)

# Exercício 4

# Exp1
Resp1 <- matrix(c(2 , 3 , 4 ,  6, 
                  6 , 8 , 8 , 12,   
                  13, 10, 15, 20),12,1) 

Bloco <- factor(c(rep(1:4,each=3)))
Trat <- factor(c(rep(1:3,4)))
tab1 <- cbind(Bloco,Trat,Resp1)
tab1 <- data.frame(tab1)

#ANOVA do DABC
fit0 <- aov(Resp1 ~ Trat + Bloco)
an <- anova(fit0)
knitr::kable(caption = "Tabela de ANOVA", an)

interaction.plot(Bloco, Trat, Resp1, main="Efeito de Interação dos Fatores Tratamento e Bloco")

par(mfrow=c(2,2))
plot(fit0$fitted.values,rstudent(fit0), ylab="Resíduos studentizados", xlab="Valores Preditos")
title("Gráfico 37: Resíduos vs Preditos")
plot.default(tab1$Bloco,rstudent(fit0), ylab="Resíduos studentizados", xlab="Blocos")
title("Gráfico 38: Resíduos vs Blocos")
plot.default(tab1$Trat,rstudent(fit0), ylab="Resíduos studentizados", xlab="Tratamento")
title("Gráfico 39: Resíduos vs Tratamento")
plot(rstudent(fit0), type="l", ylab="Resíduos studentizados", xlab="Índices") 
title("Gráfico 40:Resíduos vs indíce")
abline(h=0, col="red")
par(mfrow=c(1,1))
qqnorm(rstudent(fit0),ylab="Resíduos studentizados", main=NULL)
qqline(rstudent(fit0))
title("Gráfico 41: QQPlot Normal")

tr <- c(0, fit0$coeff[2:3])
tr <- rep(tr, 4)
bl <- c(0, fit0$coeff[4:6])
bl <- rep(bl, each=3)
trbl <- tr * bl # Novo fator que mede a interação
datt <-cbind(tab1,tr,bl, trbl)

fitna <- aov(Resp1 ~ Trat + Bloco + trbl)
anv <- anova(fitna)
knitr::kable(caption = "Tabela de ANOVA", anv)

fit0.tka <- TukeyHSD(fit0, "Trat")
fit0.tka
plot(fit0.tka)

# Exp2
Resp2 <- matrix(c(2 , 15, 16, 6, 
                  12, 13, 8 , 6,   
                  7 , 10, 3 , 4),12,1) 

tab2 <- cbind(Bloco,Trat,Resp2)
tab2 <- data.frame(tab2)

#ANOVA do DABC
fit1 <- aov(Resp2 ~ Trat + Bloco)
an <- anova(fit1)
knitr::kable(caption = "Tabela de ANOVA", an)

interaction.plot(Bloco, Trat, Resp2, main="Efeito de Interação dos Fatores Tratamento e Bloco")

par(mfrow=c(2,2))
plot(fit1$fitted.values,rstudent(fit1), ylab="Resíduos studentizados", xlab="Valores Preditos")
title("Gráfico 44: Resíduos vs Preditos")
plot.default(tab2$Bloco,rstudent(fit1), ylab="Resíduos studentizados", xlab="Blocos")
title("Gráfico 45: Resíduos vs Blocos")
plot.default(tab2$Trat,rstudent(fit1), ylab="Resíduos studentizados", xlab="Tratamento")
title("Gráfico 46: Resíduos vs Tratamento")
plot(rstudent(fit1), type="l", ylab="Resíduos studentizados", xlab="Índices") 
title("Gráfico 47:Resíduos vs indíce")
abline(h=0, col="red")
par(mfrow=c(1,1))
qqnorm(rstudent(fit1),ylab="Resíduos studentizados", main=NULL)
qqline(rstudent(fit1))
title("Gráfico 48: QQPlot Normal")

tr <- c(0, fit1$coeff[2:3])
tr <- rep(tr, 4)
bl <- c(0, fit1$coeff[4:6])
bl <- rep(bl, each=3)
trbl2 <- tr * bl #novo fator que mede a interação
datt <-cbind(tab2,tr,bl, trbl2)

fitna <- aov(Resp2 ~ Trat + Bloco + trbl2)
anvo <- anova(fitna)
knitr::kable(caption = "Tabela de ANOVA", anvo)

# Exercício 5

# item a

Resp <- c(65,58,63,57,66,68,62,75,64,70,56,
          65,58,70,64,45,56,54,48,60,74,81,
          76,80,68,69,76,80,78,73,52,56,62,
          58,51,73,78,83,75,76,69,83,74,78,
          80,63,70,72,68,75,81,72,73,76,70,
          67,79,73,77,71)
dados <- data.frame(fa=factor(rep(1:3,each=20)), fb=factor(rep(1:12,each=5)), resp=Resp)
attach(dados)
fit1 <- aov(Resp ~ fa/fb, data=dados)
a <- anova(fit1)
knitr::kable(caption = "Tabela de ANOVA", a)

par(mfrow=c(2,2))
plot(fit1$fitted.values,rstudent(fit1), ylab="Resíduos studentizados", xlab="Valores Preditos")
title("Gráfico 49: Resíduos vs Preditos")
plot.default(dados$fb,rstudent(fit1))
title("Gráfico 50: Resíduos vs Operadores(Máquinas)")
plot.default(dados$fa,rstudent(fit1))
title("Gráfico 51: Resíduos vs Máquinas")
plot(rstudent(fit1), type="l", ylab="Resíduos studentizados", xlab="Índices") 
title("Gráfico 52:Resíduos vs índices")
abline(h=0, col="red")

qqnorm(rstudent(fit1),ylab="Resíduos studentizados", main=NULL)
qqline(rstudent(fit1))
title("Gráfico 53: QQPlot Normal")

# item c

s2 <- sum(resid(fit1)^2)/fit1$df.res
dt <- qtukey(0.95, 2, 48) * sqrt(s2/5)
fit1.m <- tapply(Resp, list(fa,fb), mean)
# Para maquina 1
md1 <- fit1.m[1,1]-fit1.m[1,2]
data.frame("1-2",dif=md1, li=md1-dt, ls=md1+dt, sig=ifelse(abs(md1)>dt,"*","ns"))
md2 <- fit1.m[1,1]-fit1.m[1,3]
data.frame("1-3",dif=md2, li=md2-dt, ls=md2+dt, sig=ifelse(abs(md2)>dt,"*","ns"))
md3 <- fit1.m[1,1]-fit1.m[1,4]
data.frame("1-3",dif=md3, li=md3-dt, ls=md3+dt, sig=ifelse(abs(md3)>dt,"*","ns"))
md4 <- fit1.m[1,2]-fit1.m[1,3]
data.frame("2-3",dif=md4, li=md4-dt, ls=md4+dt, sig=ifelse(abs(md4)>dt,"*","ns"))
md5 <- fit1.m[1,2]-fit1.m[1,4]
data.frame("2-4",dif=md5, li=md5-dt, ls=md5+dt, sig=ifelse(abs(md5)>dt,"*","ns"))
md6 <- fit1.m[1,3]-fit1.m[1,4]
data.frame("3-4",dif=md6, li=md6-dt, ls=md6+dt, sig=ifelse(abs(md6)>dt,"*","ns"))

# Para maquina 2
md7 <- fit1.m[2,5]-fit1.m[2,6]
data.frame("5-6",dif=md7, li=md7-dt, ls=md7+dt, sig=ifelse(abs(md7)>dt,"*","ns"))
md8 <- fit1.m[2,5]-fit1.m[2,7]
data.frame("5-7",dif=md8, li=md8-dt, ls=md8+dt, sig=ifelse(abs(md8)>dt,"*","ns"))
md9 <- fit1.m[2,5]-fit1.m[2,8]
data.frame("5-8",dif=md9, li=md9-dt, ls=md9+dt, sig=ifelse(abs(md9)>dt,"*","ns"))
md10 <- fit1.m[2,6]-fit1.m[2,7]
data.frame("6-7",dif=md10, li=md10-dt, ls=md10+dt, sig=ifelse(abs(md10)>dt,"*","ns"))
md11 <- fit1.m[2,6]-fit1.m[2,8]
data.frame("6-8",dif=md11, li=md11-dt, ls=md11+dt, sig=ifelse(abs(md11)>dt,"*","ns"))
md12 <- fit1.m[2,7]-fit1.m[2,8]
data.frame("7-8",dif=md12, li=md12-dt, ls=md12+dt, sig=ifelse(abs(md12)>dt,"*","ns"))

# Para maquina 3

md13 <- fit1.m[3,9]-fit1.m[3,10]
data.frame("9-10",dif=md13, li=md13-dt, ls=md13+dt, sig=ifelse(abs(md13)>dt,"*","ns"))
md14 <- fit1.m[3,9]-fit1.m[3,11]
data.frame("9-11",dif=md14, li=md14-dt, ls=md14+dt, sig=ifelse(abs(md14)>dt,"*","ns"))
md15 <- fit1.m[3,9]-fit1.m[3,12]
data.frame("9-12",dif=md15, li=md15-dt, ls=md15+dt, sig=ifelse(abs(md15)>dt,"*","ns"))
md16 <- fit1.m[3,10]-fit1.m[3,11]
data.frame("10-11",dif=md16, li=md16-dt, ls=md16+dt, sig=ifelse(abs(md16)>dt,"*","ns"))
md17 <- fit1.m[3,10]-fit1.m[3,12]
data.frame("10-12",dif=md17, li=md17-dt, ls=md17+dt, sig=ifelse(abs(md17)>dt,"*","ns"))
md18 <- fit1.m[3,11]-fit1.m[3,12]
data.frame("11-12",dif=md18, li=md18-dt, ls=md18+dt, sig=ifelse(abs(md18)>dt,"*","ns"))

# item d

interaction.plot(fb, fa, Resp, main="Perfis médios de acordo com Máquina e Operador", xlab="Operadores")

# Exercício 6

# item a

Cor <- factor(c(rep(1,5),rep(2,5),rep(3,5),rep(1,5),rep(2,5),rep(3,5)))
Estacionamento <- factor(c(seq(1:15),seq(1:15)))
Semanas <- factor(c(rep(1,15),rep(2,15)))
Resp <- c(28, 26, 31, 27, 35, 34, 29, 25, 31, 29, 31, 25, 27, 29, 28, 32, 23, 29, 24, 37, 33, 27, 22, 34, 25, 35, 28, 25, 25, 31)

mercado <- data.frame(Cor,Estacionamento,Semanas,Resp)

mcor <- tapply(Resp,Cor,mean)
plot.default(Cor, Resp,main="Gráfico 55: Taxa de retorno de acordo com a Cor")
points(mcor, pch="x", col=2, cex=1.5)

mestacionamento <- tapply(Resp,Estacionamento,mean)
plot.default(Estacionamento, Resp,main="Gráfico 56: Taxa de retorno de acordo com a Cor")
points(mestacionamento, pch="x", col=2, cex=1.5)

msemana <- tapply(Resp,Semanas,mean)
plot.default(Semanas, Resp,main="Gráfico 57: Taxa de retorno de acordo com a Cor")
points(msemana, pch="x", col=2, cex=1.5)

mod6 <- aov(Resp ~ Cor/Estacionamento+Semanas)
anova6 <- anova(mod6)
knitr::kable(caption = "Tabela de ANOVA", anova6)

par(mfrow=c(2,2))
s2 <- anova6$"Mean Sq"[4]

par(mfrow=c(2,2))
res <- rstudent(mod6) # extraindo resíduos
# Verificação de independência
plot(res, type="l") # res x índice das observações
title("Gráfico 58: Resíduos vs Preditos")
abline(h=0, col="red")

plot(mod6$fit, mod6$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 59: Resíduos vs Preditos")

hist(res, main=NULL)
title("Gráfico 60: Histograma dos resíduos")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico 61: QQPlot Normal")

respadr <- (mod6$residuals/sqrt(anova6$"Mean Sq"[4]))
par(mfrow=c(1,2))
plot.default(mercado$Estacionamento,respadr)
title("Gráfico 62: Resíduos vs B(A)")
plot.default(mercado$Cor,respadr)
title("Gráfico 63: Resíduos vs A")

mod6$coeff

mod6.tka <- TukeyHSD(mod6, "Cor")
mod6.tka

plot(mod6.tka)