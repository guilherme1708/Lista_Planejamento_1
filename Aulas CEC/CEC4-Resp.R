#########################################
#######   ALGUNS COMANDOS R           ###
#######   MAE0317 – 1o.SEM/2019       ###    
#######   Profa. Júlia M Pavan Soler  ###
#########################################
##Aula CEC: DABC
##Exemplo 1: dados do crescimento de plantas

resp <- matrix(c(19.8, 21.9, 16.4, 14.7, 
                 16.7, 19.8, 15.4, 13.5,   
                 17.7, 21.0, 14.8, 12.8,               
                 18.2, 21.4, 15.6, 13.7,                
                 20.3, 22.1, 16.4, 14.6,               
                 15.5, 20.8, 14.6, 12.9),24,1)                           
resp
Bloco <- factor(c(rep(1:6,each=4)))
Bloco
Var <- factor(c(rep(1:4,6)))
Var
dat <- cbind(Bloco,Var,resp)
dat <- data.frame(dat)
str(dat)
attach(data.frame(dat))
Bloco <- factor(Bloco)
Var <- factor(Var)

plot(resp ~ Var) #Há interesse na comparação destas médias?
# Sim, pois descritivamente se ve diferença entre entre as médias

plot(resp ~ Bloco) #Há interesse na comparação destas médias?
# Descritivamente não há diferença entre as médias, e também são blocos e normalmente não há interesse em testar médias dos 
# fatores de controle (blocos)

interaction.plot(Bloco, Var, resp, main="Efeito de Interação dos Fatores Var e Bloco")
#Qual é a importância deste gráfico de perfis individuais?
# Avalia descritivamente o efeito de intração entre Var*Bloco

#O efeito de interação Var*Bloco define qual F.V. da tabela ANOVA?
# Não, o efeito de interação Var*Bloco define o resíduo na tabela ANOVA

boxplot(resp ~ Var, main="Crescimento de plantas de acordo com Variedade")
tapply(resp,Var,mean)
tapply(resp,Var,sd) #A variância comum estima qual parâmetro?
# Estima a variância comum aos grupos sem considerar efeito de bloco, ou seja, estima a variância no DCA

#ANOVA do DABC
fit1 <- aov(resp ~ Var + Bloco)
anova(fit1)

#Compare com o caso sem o efeito de bloco (DCA)
fit0 <- aov(resp ~ Var)
anova(fit0)

par(mfrow=c(2,2))
plot(fit1)
# Primeiro gráfico (Resíduos x preditos) curva vermelha chama "curva de LOESS"

plot(fit0) #Compare e comente!
# Suposições do modelo esta ok!

names(fit1)

par(mfrow=c(2,2))
plot(fit1$fitted.values,res)
title("Resíduos vs Preditos")
plot.default(dat$Bloco,fit1$residuals)
title("Resíduos vs Blocos")
plot.default(dat$Var,fit1$residuals)
title("Resíduos vs Tratamento")

qqnorm(fit1$residuals,ylab="Residuos", main=NULL)
qqline(fit1$residuals)
title("Gráfico de Probabilidade Normal - Resíduos")

#valores preditos (y_hat) = fit1$fitted.values
#res <- (fit1$residuals)
#respadr <- (fit1$residuals/sqrt(anova(fit1)$"Mean Sq"[2]))

#DABC: supõe modelo ADITIVO (resíduo=interação VAR*Bloco) 
#Neste caso, o resíduo tem 3x5=15 g.l.
#Podemos testar a "não aditividade" (entre Var e Bloco) usando 1 g.l.
#Construir um novo fator com as estimativas dos efeitos de Var*Bloco

fit1$coeff
dat
tr <- c(0, fit1$coeff[2:4])
tr <- rep(tr, 6)
bl <- c(0, fit1$coeff[5:9])
bl <- rep(bl, each=4)
trbl <- tr * bl #novo fator que mede a interação
datt<-cbind(dat,tr,bl, trbl)
datt

fitna <- aov(resp ~ Var + Bloco + trbl)
#fitna <- update(fit1, .~. + trbl) #comando alternativo
anova(fitna) #Interprete. O que está sendo testado?
#Note que a interação está sendo medida por ef.linear_Var*ef.linear_Bloco
#Como o valor-p do fator "trbl" é alto, o modelo aditivo pode ser aceito

anova(fit1) #este modelo pode então ser ser adotado

#Estudo do efeito de Tratamento
fit1.tk <- TukeyHSD(fit1, "Var")
fit1.tk
plot(fit1.tk)
#Conclusão das comparações de Tukey?
#Tente calcular estes intervalos de confiança

detach(dados)



##Exemplo 2: Dados de Clorofila a
y <- c(6.2, 12.7,	7.0,	8.3,
       4.8,	11.3,	4.4,	7.1,
       3.0,	9.3,	3.8,	11.7,
       5.6,	9.3,	5.0,	10.0,
       7.1,	11.7,	5.5,	8.5,
       4.8,	15.3,	3.2,	12.4)

dados <- data.frame(trat=factor(rep(1:4, 6)), bloco=factor(rep(1:6,each=4)), resp=y)
str(dados)
attach(dados)
summary(dados)
plot(y ~ trat)
plot(y ~ bloco)

interaction.plot(bloco, trat, y)
#Há indicação de efeito de interação entre bloco e trat?
#O efeito de interação trat*bloco define qual F.V. da tabela ANOVA?

mt <- tapply(y,trat,mean)
plot.default(trat, y)
points(mt, pch="x", col=2, cex=1.5)

mb <- tapply(y,bloco,mean)
plot.default(bloco, y)
points(mb, pch="x", col=2, cex=1.5)

#Grafico com o perfil das médias
plot(mt, type="l", main="Perfil de médias")
points(mt, pch="x", col=2, cex=1.5)
interaction.plot(bloco, trat, y, main="Perfis individuais")
#par(mfrow=c(1,2))

#ANOVA do DABC
fit1 <- aov(y ~ trat + bloco)
anova(fit1)

###Compare com o caso sem o efeito de bloco (DCA)
fit0 <- aov(y ~ trat)
anova(fit0)
###A blocagem foi eficaz neste caso? Justifique.

par(mfrow=c(2,2))
plot(fit1)

names(fit1)
par(mfrow=c(2,2))
plot(fit1$fitted.values,fit1$residuals)
title("Resíduos vs Preditos")
plot.default(dados$bloco,fit1$residuals)
title("Resíduos vs Blocos")
plot.default(dados$trat,fit1$residuals)
title("Resíduos vs Tratamento")

qqnorm(fit1$residuals,ylab="Residuos", main=NULL)
qqline(fit1$residuals)
title("Gráfico de Prob.Normal dos Resíduos")

#DABC: supõe modelo aditivo (resíduo=interação trat*bloco) 
#Neste caso, o resíduo tem 3x5=15 g.l.
#Podemos testar a "não aditividade" (entre trat e bloco) usando 1 g.l.
#construindo um novo fator com as estimativas dos efeitos de trat e bloco

fit1$coeff
dados
tr <- c(0, fit1$coeff[2:4])
bl <- c(0, fit1$coeff[5:9])
trbl <- rep(tr, 6) * rep(bl, rep(4,6)) #novo fator que mede a interação

cbind(dados,rep(tr,6),rep(bl,rep(4,6)), trbl)

fitna <- update(fit1, .~. + trbl)
anova(fitna) #Logo, o modelo aditivo pode ser aceito

anova(fit1)

#Estudo do efeito de Tratamento
fit1.tk <- TukeyHSD(fit1, "trat")
fit1.tk
plot(fit1.tk)

#Estudo do efeito de Tratamento
Fit0.tk <- TukeyHSD(fit0, "trat")
Fit0.tk
plot(fit0.tk)

detach(dados)

#Exemplo 3: Analise os dados a seguir.
Um estudo foi conduzido para avaliar o efeito de interação 
entre dois medicamentos (X e Y) usados para estimular o 
crescimento de crianças. Sabe-se que o efeito de cada medicamento 
é modesto, mas o efeito da combinação das duas drogas (X e Y) não tem 
sido investigado. Os seguintes resultados foram obtidos da avaliação 
da taxa de crescimento de 16 pacientes considerados no estudo:
                    Pacientes
FxEtária	1	      2	       3	       4
1     	0.02 (A)	0.15 (B)	 0.45 (D)	 0.18 (C)
2	      0.27 (B)	0.24 (C)	-0.01 (A)	 0.58 (D) 
3	      0.11 (C)	0.35 (D)	 0.14 (B)	-0.03 (A)
4	      0.48 (D)	0.04 (A)	 0.18 (C)	 0.22 (B)
A=Placebo     B= Droga X      C=Droga Y      D=Drogas X e Y



Resp <- c(0.02,-0.01,-0.03,0.04,0.15,0.27,0.14,0.22,0.18,0.24,0.11,0.18,0.45,0.58,0.35,0.48)
Bloco <- factor(c(rep(1:4,each=4)))
Fator <- factor(c(rep(c("X","Y"),4)))


