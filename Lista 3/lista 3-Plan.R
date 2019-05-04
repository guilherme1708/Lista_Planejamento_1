# Lista de planejamento e Pesquisa MAE0317

# Exercício 1

library(reshape2)
library(dplyr)
library(ggplot2)
X <- c("SX","SX","SX","SX","CX","CX","CX","CX","SX","SX","SX","SX","CX","CX","CX","CX")
Y <- c("SY","SY","SY","SY","SY","SY","SY","SY","CY","CY","CY","CY","CY","CY","CY","CY")
Valores <- c(0.02,-0.01,-0.03,0.04,0.15,0.27,0.14,0.22,0.18,0.24,0.11,0.18,0.45,0.58,0.35,0.48)
tabela <- data.frame(X,Y,Valores)


SCY = c("SY","CY")
SX = c(0.005,0.1775)
CX = c(0.195,0.465)
media <- data.frame(SCY,SX,CX)

data2 <- melt(media, SCY.vars = "SCY")
data2 <- group_by(data2, SCY)
names(data2) <- c("Y","X","Medias")
ggplot(data2) +
  geom_line(aes(X, Medias, group = Y, linetype = Y))+ ggtitle("Gráfico 1: gráfico de perfis de médias") +ggtitle("Gráfico 1: gráfico de perfis de médias") 



# item b
tapply(tabela$Valores,tabela$Y, mean)
tapply(tabela$Valores,tabela$X, mean)

par(mfrow = c(1,2))
boxplot(Valores ~ X, data = tabela)
boxplot(Valores ~ Y, data = tabela)

par(mfrow = c(1,1))
boxplot(Valores ~ X:Y, data = tabela)

m1 = lm(Valores ~ X*Y, data = tabela)
an <- anova(m1)
knitr::kable(caption = "Tabela de ANOVA", an)

fit <- aov(Valores~X*Y, tabela)
anov <- anova(fit)
s2 <- anov$"Mean Sq"[2]
par(mfrow=c(2,2))
plot(fit$residuals, type="l") 
title("Gráfico 5:Resíduos vs indíce")
abline(h=0, col="red")

plot(fit$fit, fit$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 6:Resíduos vs Preditos")

res <- fit$res 
Respad <- (res/sqrt(s2))
hist(Respad, main=NULL)
title("Gráfico 7:Histograma dos resíduos padronizados")

qqnorm(res,ylab="Resíduos", main=NULL)
qqline(res)
title("Gráfico8:QQ-Plot Normal")

shapiro.test(res)

# item c

a1 = aov(Valores ~ X*Y, data = tabela)
TukeyHSD(a1)

# Exercício 2

P <- c("P1","P1","P1","P1","P1","P1","P1","P1","P1","P1","P1","P1","P2","P2","P2","P2","P2","P2","P2","P2","P2","P2","P2","P2","P3","P3","P3","P3","P3","P3","P3","P3","P3","P3","P3","P3")
D <- c("D1","D1","D1","D2","D2","D2","D3","D3","D3","D4","D4","D4","D1","D1","D1","D2","D2","D2","D3","D3","D3","D4","D4","D4","D1","D1","D1","D2","D2","D2","D3","D3","D3","D4","D4","D4")
Valores <- c(16,22,9,8,7,6,44,27,22,17,17,14,18,27,15,13,5,11,31,33,19,19,23,17,17,34,13,14,12,10,28,54,39,24,28,31)
companhia <- data.frame(P,D,Valores)
Ps <- c("P1","P2","P3")
D1 <- c(mean(Valores[1:3]),mean(Valores[13:15]),mean(Valores[25:27]))
D2 <- c(mean(Valores[4:6]),mean(Valores[16:18]),mean(Valores[28:30]))
D3 <- c(mean(Valores[7:9]),mean(Valores[19:21]),mean(Valores[31:33]))
D4 <- c(mean(Valores[10:12]),mean(Valores[22:24]),mean(Valores[34:36]))
Medias <- data.frame(Ps,D1,D2,D3,D4)

data2 <- melt(Medias, Ps.vars = "Ps")
data2 <- group_by(data2, Ps)
names(data2) <- c("Ps","Ds","Medias")
ggplot(data2) +
  geom_line(aes(Ds, Medias, group =Ps,
                linetype = Ps))+ggtitle("Gráfico 9: gráfico de perfis de médias")

tapply(companhia$Valores,companhia$P, mean)
tapply(companhia$Valores,companhia$D, mean)

par(mfrow = c(1,2))
boxplot(Valores ~ P, data = companhia)
boxplot(Valores ~ D, data = companhia)

boxplot(Valores ~ P:D, data = companhia)

m1 = lm(Valores ~ P*D, data = companhia)
a <- anova(m1)
knitr::kable(caption ="Tabela de ANOVA",a)

fit <- aov(Valores~P*D, companhia)
anov <- anova(fit)
s2 <- anov$"Mean Sq"[2]
par(mfrow=c(2,2))
plot(fit$residuals, type="l") 
title("Gráfico 13:Resíduos vs indíce")
abline(h=0, col="red")

plot(fit$fit, fit$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Gráfico 14:Resíduos vs Preditos")

res <- fit$res 
Respad <- (res/sqrt(s2))
hist(Respad, main=NULL)
title("Gráfico 15:Histograma dos resíduos padronizados")

qqnorm(res,ylab="Resíduos", main=NULL)
qqline(res)
title("Gráfico 16:QQ-Plot Normal")

shapiro.test(res)

a1 = aov(Valores ~ P, data = companhia)
TukeyHSD(a1)

a1 = aov(Valores ~ D, data = companhia)
TukeyHSD(a1)

# Exercício 3

# item a

# Preparação dos dados

library(car)
library(ggplot2)
library(asbio)

base.r <-c(0.29,0.27,0.22,0.22,
          0.26,0.20,0.18,0.12,
          0.24,0.16,0.10,0.10,
          0.19,0.19,0.09,0.09,
          0.17,0.12,0.09,0.06)
base.r <- as.data.frame((matrix(base.r, 4, 5)))
colnames(base.r) <- c("A", "B", "C", "D", "E")

base.r <- stack(base.r)
colnames(base.r) <- c("Resp", "Institutos")

# Análise dos dados 

base <- base.r

n <- dim(base)[1]
K <- length(levels(base$Institutos))

# box-plots
# verificar visualmente problemas:
# pontos atípicos, assimetria e variâncias heterogêneas
ggplot(base.r, aes(x=Institutos, y=Resp)) +
  geom_boxplot(fill='#A4A4A4', color="black") 

##########################################################
# Ajuste do modelo de ANOVA (DCA com 1 fator em K níveis)
##########################################################

fit <- aov(Resp~Institutos, base)

anov <- anova(fit)
names(anov)
anov

# O que podemos concluir dessa análise? Qual é a hipótese em teste?

# Diagnóstico - Análise de resíduos

s2 <- anov$"Mean Sq"[2] # estimativa da variância residual

par(mfrow=c(2,2))

# Verificação de independência
plot(fit$residuals, type="l") # res x índice das observações
title("Resíduos vs indíce")
abline(h=0, col="red")

plot(fit$fit, fit$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

res <- fit$res # extraindo resíduos
Respad <- (res/sqrt(s2)) # resíduos padronizados
hist(Respad, main=NULL)
title("Histograma dos resíduos padronizados")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("QQ-Plot Normal")

# Teste de normalidade de Shapiro-Wilk
# Qual é a hipótese H0
shapiro.test(res)  #conclusão?

# Teste de homogeneidade de variâncias
bartlett.test(Resp~Institutos,data=base)  

# Compações Múltiplas
fit.du <- with(base, pairw.anova(y=Resp, x=Institutos, control="B", method="dunnett"))
d<-as.data.frame(fit.du$summary)
row.names(d) <- fit.du$comp

# item b

base.r1 <-c(0.26,0.20,0.18,0.12,
            0.17,0.12,0.09,0.06)
base.r1 <- as.data.frame((matrix(base.r1, 4, 2)))
colnames(base.r1) <- c("B", "E")

base.r1 <- stack(base.r1)
colnames(base.r1) <- c("Resp", "Institutos")

fit <- aov(Resp~Institutos,base.r1)

res <- fit$res # extraindo resíduos
# Teste de normalidade de Shapiro-Wilk
shapiro.test(res)  

# Teste de homogeneidade de variâncias
bartlett.test(Resp~Institutos,data=base.r1)   

# Verificação de independência
plot(fit$residuals, type="l") # res x índice das observações
title("Resíduos vs indíce")
abline(h=0, col="red")
t.test(base.r1$Resp~base.r1$Institutos, var.equal=T)

# Exercício 4

#item b

A <- c(rep("-1",4),rep("1",4))
B <- c("-1","-1","1","1","-1","-1","1","1")
C <- c("-1","1","-1","1","-1","1","-1","1")
Y <- c(195,496,87,1371,102,1001,354,775)
tabela <- data.frame(A,B,C,Y)

with(tabela, {
  interaction.plot(A, B, Y)
})

with(tabela, {
  interaction.plot(A, C, Y)
})

with(tabela, {
  interaction.plot(B, C, Y)
})

with(tabela, {
  interaction.plot(A:B, C, Y)
})

CA <- 102
CB <- 87
CC <- 496
x <- c("A","B","Y")
y <- c(CA,CB,CC)
d <- data.frame(x,y)

ggplot(d, aes(x=x, y=y, group=1)) + 
  geom_line() +
  geom_point() +
  labs(x="Fatores", y="Qualidade da água")

# item c

fit <- aov(Y ~ A+B+C+A:B:C, tabela)
fit1 <- anova(fit)
knitr::kable(caption = "Tabela de ANOVA", fit1)
TukeyHSD(fit)

# Exercício 6


base.b <- c(21, '1', 'Baixo', 'Baixo', 12, '1', 'Baixo', 'Alto', 13, '1', 'Alto', 'Baixo', 1, '1', 'Alto', 'Alto', 21, '1', 'Baixo', 'Baixo', 18, '1', 'Baixo', 'Alto', 14, '1', 'Alto', 'Baixo', 8, '1', 'Alto', 'Alto', 23, '2', 'Baixo', 'Baixo', 14, '2', 'Baixo', 'Alto', 13, '2', 'Alto', 'Baixo', 1, '2', 'Alto', 'Alto', 23, '2', 'Baixo', 'Baixo', 17, '2', 'Baixo', 'Alto', 16, '2', 'Alto', 'Baixo', 11, '2', 'Alto', 'Alto', 17, '3', 'Baixo', 'Baixo', 20, '3', 'Baixo', 'Alto', 16, '3', 'Alto', 'Baixo', 14, '3', 'Alto', 'Alto', 23, '3', 'Baixo', 'Baixo', 17, '3', 'Baixo', 'Alto', 17, '3', 'Alto', 'Baixo', 5, '3', 'Alto', 'Alto')

aovbase.b <- as.data.frame(t(matrix(base.b, 4, 24))) 
colnames(aovbase.b) <- c("resp", "TE", "TA", "pH")
aovbase.b$resp <- as.numeric(levels(aovbase.b$resp))[aovbase.b$resp]

# GRÁFICO PERFIL DE MÉDIAS. 

par(mfrow=c(1,2))
with(aovbase.b, {
  interaction.plot(TE, TA, resp)
  interaction.plot(TE, pH, resp)
})
par(mfrow=c(1,1))
with(aovbase.b, {
  interaction.plot(TA, pH, resp)
})

#ANOVA TE*TA*pH E COMPARAÇÃO DE MÉDIAS

fit <- aov(resp ~ TE * TA * pH, aovbase.b)
fit1 <- anova(fit)
knitr::kable(caption = "Tabela de ANOVA", fit1)

fit1 <- aov(resp ~ TA + pH, aovbase.b)
TukeyHSD(fit1)

plot(TukeyHSD(fit))


ava<-anova(fit)
s2 <- ava$"Mean Sq"[8] # estimativa da variância residual

par(mfrow=c(2,2))
plot(fit)

plot(fit$fit, fit$resp, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

avres <- fit$residuals # extraindo resíduos
respad <- (avres/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

qqnorm(avres,ylab="Residuos", main=NULL)
qqline(avres)
title("Gráfico Normal de Probabilidade dos Resíduos")

shapiro.test(avres)
