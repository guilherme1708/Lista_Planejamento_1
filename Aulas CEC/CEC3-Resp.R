#################################################
#######   ALGUNS COMANDOS R               #######
#######   MAE0317 – 1o.SEM/2019           #######    
#######   Profa. Júlia M Pavan Soler      #######
####### #########################################
#Exemplo 1: DCA Fatorial 3x4
y <- c(0.31, 0.82, 0.43, 0.45, 0.45,1.1, 0.45,0.71, 0.46, 0.88, 0.63, 0.66, 0.43, 0.72, 0.76, 0.62, 0.36, 0.92, 0.44, 0.56, 0.29, 0.61, 0.35, 1.02, 0.4, 0.49, 0.31, 0.71, 0.23, 1.24, 0.4, 0.38, 0.22, 0.3, 0.23, 0.3, 0.21, 0.37, 0.25, 0.36, 0.18, 0.38, 0.24, 0.31, 0.23, 0.29, 0.22, 0.33)
y
dados <- data.frame(trat=factor(rep(1:12,each=4)), fa=factor(rep(1:3,each=16)), fb=factor(rep(1:4,each=4)), resp=y)
dados
str(dados)
attach(dados)

interaction.plot(fb, fa, y, main="Efeito de Interação dos Fatores A e B em Y")
par(mfrow=c(1,2)) 
interaction.plot(fb, fa, y)
interaction.plot(fa, fb, y)
#Interprete o gráfico de perfis de médias

#Há indicação de efeito de interação entre os fatores?
# Sim, pois existe cruzamento das linhas (fatores)

plot(y ~ fa,main="Efeito Principal do Fator A (3 níveis) em Y")
plot(y ~ fb,main="Efeito Principal do Fator B (4 níveis) em Y")

plot(y ~ trat,main="Efeito Principal do Fator Trat (3x4=12 níveis) em Y")

tapply(y, list(fa,fb), mean)
tapply(y, fa, mean)
tapply(y,fb, mean)

##Ajuste do modelo ANOVA Fatorial 3x4 com efeito de interação
fit1 <- aov(y ~ fa + fb + fa * fb)
summary(fit1)

##Ajuste do modelo ANOVA para Trat (Um Fator em 3x4=12 níveis)
fit0 <- aov(y ~ trat)
summary(fit0)

#Compare as duas Tabelas de ANOVA quanto ao
#Número de graus de liberdade e Soma de Quadrados
# os graus de liberdade da tabela de ANOVA do modelo fit0 (trat) é a soma dos graus de 
# liberdade do modelo fit1 (fa + fb + fa:fb)

#Realize uma análise dos resíduos do modelo de interação
par(mfrow=c(2,2))
plot(fit1)
#Os dados atendem às suposições do modelo?
# Não, podemos notar problemas com homocedasticidade e com a normalidade

# Teste de homogeneidade de variâncias
library(car)
leveneTest(fit0)  #p=0.5779 ==>Conclusão?
# não rejeitamos H0

bartlett.test(y~trat) #p=0.008089 ==>Conclusão?
# Rejeitamos H0, sob condição de normalidade (não é valido nesse caso especifico)

boxplot(y ~trat)

#Transformação Box-Cox
require(MASS)
boxcox(resp ~ trat, data=dados, plotit=T)
boxcox(resp ~ trat, data=dados, lam=seq(-1, 0, 1/10))
#O valor lambda=-0.5 pode ser adotado na transformação
# yt <- y^(lambda)

yt <- y^(-0.5)

fit2 <- aov(yt ~ fa + fb + fa * fb)
summary(fit2)
par(mfrow=c(2,2))
plot (fit2)
#Comente o ajuste do modelo ANOVA para os dados transformados
# As suposições do modelo estão satisfeitas

# Teste de homogeneidade de variâncias
library(car)
fit20 <- aov(yt ~ trat)
leveneTest(fit20)  #p=0.6934 ==>Conclusão?
# não rejeitamos H0

bartlett.test(yt~trat) #p=0.779 ==>Conclusão?
# não rejeitamos H0 (agora está okay)

interaction.plot(fb, fa, y, main="Efeito de Interação dos Fatores A e B em Yt")
#Há indicação de efeito de interação entre os fatores na variável Yt?
# Aparentemente existe interação entre os níveis 1 e 2 do fator A

plot(yt ~ fa,main="Efeito Principal do Fator A (3 níveis) em Yt")
plot(yt ~ fb,main="Efeito Principal do Fator B (4 níveis) em Yt")

##Qual é a conclusão desta análise?
# Apenas o fator A é significante
fit3 <- aov(yt ~ fa)
summary(fit3)

# Teste de Tukey: comparações múltiplas
fit3.tu <- TukeyHSD(fit3)
fit3.tu
plot(fit3.tu) # Conclusão?
# u2=u1; u3>u1; u3<u2

##Ajuste do modelo de ANOVA via Regressão com variáveis dummy
##Parametrização de Casela de Referência
model.matrix(fit2)
fit2$coefficients

X <- model.matrix(fit2)
dim(X)

fitreg2 <- lm(yt ~ X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12])
fitreg2  #fit2$coefficients
#Estimativas iguais ao ajuste ANOVA
summary(fitreg2) #Quais diferenças entre médias são significantes?

#Compare as Somas de Quadrados dos modelos de Regressão e ANOVA
anova(fit2)
anova(fitreg2) 
#Veja a decomposição das SQ dos efeitos da ANOVA
#Estas são as SQ Squenciais (Tipo I)

summary.lm(fit2) #comando equivalente summary(fitreg2)

fitreg3 <- lm(yt ~ X[,2]+X[,3])
fitreg3  #fit3$coefficients
summary(fitreg3)
anova(fitreg3)
anova(fit3)

#Mostre que:
#SQRegr(X2,X3)=SQRegr(X2)+ SQRegr(X3|X2)
#SQRegr(X3|X2)=SQErro(X2)-SQErro(X2,X3)

anova(lm(yt ~ X[,2]+X[,3]))
anova(lm(yt ~ X[,2]))
# SQX[,3] = SQ(X3|X2) = 6.1177 - 3.11437 = 3.00330


##Delineamento 2x2x2=2^3
resp <- c(60,72,54,68,52,83,45,80)
f1 <- (c(0,1,0,1,0,1,0,1))
f2 <- (c(0,0,1,1,0,0,1,1))
f3 <- (c(0,0,0,0,1,1,1,1))
dat <- data.frame(fa1=factor(f1), fa2=factor(f2), fa3=factor(f3), resp)
dat
str(dat)
attach(dat)

fiti2 <- aov(resp ~ fa1 + fa2 + fa3 + fa1*fa2 + fa1:fa3 + fa2:fa3 + fa1:fa2:fa3)
summary(fiti2)
anova(fiti2)  # Há graus de liberdade para o Resíduo? Há réplicas?
# Não há réplicas, nem graus de liberdade para o resíduo 

fiti2$coefficients  #Interprete as estimativas
model.matrix(fiti2)

fiti1 <- aov(resp ~ fa1 + fa2 + fa3 + fa1:fa2 + fa1:fa3)
summary(fiti1)
anova(fiti1) 

tapply(resp,list(fa1,fa3), mean)
# Teste de Tukey: comparações múltiplas
fiti1.tu13 <- TukeyHSD(fiti1, "fa1:fa3")
fiti1.tu13

tapply(resp,list(fa1,fa2), mean)
fiti1.tu12 <- TukeyHSD(fiti1, "fa1:fa2")
fiti1.tu12

##Estimativas dos efeitos em um DCA Fatorial 2^k
f1 <- c(-1,1,-1,1,-1,1,-1,1)
f2 <- c(-1,-1,1,1,-1,-1,1,1)
f3 <- c(-1,-1,-1,-1,1,1,1,1)
f12 <- f1*f2
f13 <-f1*f3
f23 <- f2*f3
f123 <- f1*f2*f3

ef1 <- (f1%*%resp)/4 #contraste (mi1_1 - mi1_0)
ef2 <- (f2%*%resp)/4
ef3 <- (f3%*%resp)/4
ef12 <- (f12%*%resp)/4
ef13 <- (f13%*%resp)/4
ef23 <- (f23%*%resp)/4
ef123 <- (f123%*%resp)/4
ef1
ef2
ef3
ef12
ef13
ef23
ef123

##Use o ajuste da ANOVA para calcular o erro padrão destes efeitos

