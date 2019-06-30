#################################################
#######   ALGUNS COMANDOS R               #######
#######   MAE0317 – 1o.SEM/2019           #######    
#######   Profa. Júlia M Pavan Soler      #######
####### #########################################
##CEC05: DCA - Fatorial Hierárquico com A e B(A) - Somente Fatores FIXOS
##Exemplo 1: Dados de desempenho escolar
y <- c(20, 18, 14, 19, 20, 20, 14, 18, 14, 12, 12, 9, 13, 16, 13, 9, 4, 4)

dados <- data.frame(fa=factor(rep(1:3,each=6)), fb=factor(rep(1:6,each=3)), resp=y)
dados
str(dados)
attach(dados)
plot(y ~ fa,main="Desempenho escolar de acordo com Escola")
plot(y ~ fb,main="Desempenho escolar - Método de Ensino")

mfa <- tapply(y,fa,mean)
plot.default(fa, y)
points(mfa, pch="x", col=2, cex=1.5)
#Há indicação de efeito principal de A?
# Descritivamente, podemos dizer que existe sim

mfb <- tapply(y,fb,mean)
plot.default(fb, y)
points(mfb, pch="x", col=2, cex=1.5)
#Há indicação de efeito principal de B em cada um dos três níveis de A?
# Descritivamente, podemos dizer que existe sim

#ANOVA do DCA - Hierárquico (somente efeitos FIXOS) 
fit1 <- aov(y ~ fa/fb)
anova(fit1)
#interprete as fontes de variação, número de graus de liberdade e SQ
# ...

#quais hipóteses estão sendo testadas? que conclusões são obtidas?
#Esta sendo testado o efeito principal de A e o efeito de B dentro de A
# E podemos concluir que os dois são significantes

par(mfrow=c(2,2))
plot(fit1)
#Qual é o modelo estrutural e distribucional adotado?

#As suposições estão satisfeitas?
shapiro.test(fit1$residuals) # normalidade ok
leveneTest(fit1) # Homocedasticidade ok
plot(fit1$residuals, type="l")# pelo método gráfico indepencia ok

names(fit1)
respadr <- (fit1$residuals/sqrt(anova(fit1)$"Mean Sq"[3]))
par(mfrow=c(2,1))
plot.default(dados$fb,respadr)
title("Resíduos vs B(A)")
plot.default(dados$fa,respadr)
title("Resíduos vs A")

fit1$coeff
#Interprete as estimativas dos parâmetros
#...

#Escreva o modelo estrutural adotado e as restrições de identificabilidade
#...

#Estudo do efeito principal do fator A (Escola)
fit1.tka <- TukeyHSD(fit1, "fa")
fit1.tka
plot(fit1.tka)

##Considerando o desempenho escolar médio, 
##que diferenças podem ser estabelecidas entre as Escolas 1, 2 e 3?

##Comparações Específicas: entre as médias de B em cada nível de A
#names(anova(fit1))
s2<-sum(resid(fit1)^2)/fit1$df.res
dt <- qtukey(0.95, 2, 12) * sqrt(s2/3)
fit1.m <- tapply(y, list(fa,fb), mean)
fit1.m
##Para fa.1
md1 <- fit1.m[1,2]-fit1.m[1,1]
data.frame(dif=md1, li=md1-dt, ls=md1+dt, sig=ifelse(abs(md1)>dt,"*","ns"))
##Para fa.2
md2 <- fit1.m[2,4]-fit1.m[2,3]
data.frame(dif=md2, li=md2-dt, ls=md2+dt, sig=ifelse(abs(md2)>dt,"*","ns"))
##Para fa.3
md3 <- fit1.m[3,6]-fit1.m[3,5]
data.frame(dif=md3, li=md3-dt, ls=md3+dt, sig=ifelse(abs(md3)>dt,"*","ns"))

detach(dados)

#Exercício: 
#Gere os seguintes dados:
dat <- data.frame(y = rnorm(20), 
                  A = rep(letters[1:5], each = 4),
                  B = rep(letters[1:4], 5)) 
dat
#Interprete e compare os seguintes ajustes:
fit1<-aov(y ~ A*B, dat)
summary(fit1)
fit2<-aov(y ~ B/A, dat)
summary(fit2)
fit3<-aov(y ~ A/B, dat)
summary(fit3)
#Como está definido o resíduo destes modelos? Há graus de liberdade?
#Gere r=3 réplicas e ajuste novamente estes modelos


