#################################################
#######   ALGUNS COMANDOS R               #######
#######   MAE0317 – 1o.SEM/2019           #######    
#######   Profa. Júlia M Pavan Soler      #######
####### #########################################


########################
# Base com dados reais 
########################

# Dados apresentados em Lowe (1935)
# da Iowa Agricultural Experiment Station
# Há interesse em estudar se há relação entre as variáveis
# "quantidade de gordura absorvida" e "tipo de gordura consumida".
# Neste caso, pense em gorduras como o óleo de milho, óleo de soja, 
# gordura vegetal hidrogenada e banha de porco.

base.r <-
  c(164,178,175,155,
    172,191,193,166,
    168,197,178,149,
    177,182,171,164,
    156,185,163,170,
    195,177,176,168)
base.r <- as.data.frame(t(matrix(base.r, 4, 6)))
colnames(base.r) <- c("gord1", "gord2", "gord3", "gord4")
base.r

# Arranjando os dados no formato esperado
# pelas funções que usaremos (aov e lm)

base.r <- stack(base.r)
colnames(base.r) <- c("resp", "treat")
base.r
#####################
# Análise dos dados #
#####################

base <- base.r
# base <- base.s
attach(base)
names(base)
is.factor(base$treat) # tratamento
is.numeric(base$resp) # resposta

n <- dim(base)[1]
K <- length(levels(base$treat))

# Análise descritiva dos dados
summary(base)

# box-plots
# verificar visualmente problemas:
# pontos atípicos, assimetria e variâncias heterogêneas
plot(resp~treat, data=base)


##########################################################
# Ajuste do modelo de ANOVA (DCA com 1 fator em K níveis)
##########################################################

fit <- aov(resp~treat, base)

# Outra alternativa: fit1 <- lm(resp~treat, base)

# objetos que podem ser inspecionados
names(fit)
summary(fit)

anov <- anova(fit)
names(anov)
anov

# O que podemos concluir dessa análise? Qual é a hipótese em teste?
# Que podemos comparar as médias, temos variâncias homogêneas. A hipótese de interesse é
# u1 = u2 = u3 = u

####################################
# Diagnóstico - Análise de resíduos
####################################

fit$df.residual # igual a n - K
fit$residuals
res<-residuals(fit)

s2 <- anov$"Mean Sq"[2] # estimativa da variância residual
s2

par(mfrow=c(2,2))
plot(fit)

# Verificação de independência
plot(res, type="l") # res x índice das observações
abline(h=0, col="red")

plot(fit$fit, fit$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

res <- fit$res # extraindo resíduos
respad <- (res/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

# stem-and-leaf plot (diagrama de ramos e folhas)
sort(round(respad, 1))
stem(respad)

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico Normal de Probabilidade dos Resíduos")

# Teste de normalidade de Shapiro-Wilk

# Qual é a hipótese H0
# H0: Os dados seguem uma distribuição Normal

shapiro.test(res)  #conclusão?
# p-value > 0.05 -> não rejeito H0

# Teste de homogeneidade de variâncias

# Qual é a hipótese H0
# H0: Os dados tem variancias homegêneas
library(car)
leveneTest(fit)  #conclusão? 
# p-value > 0.05 -> não rejeito H0

bartlett.test(resp~treat)   #conclusão?
# p-value > 0.05 -> não rejeito H0

####################################
# Compações Múltiplas
####################################

# Qual é a média do grupo 1?
tapply(resp,treat, mean)[1]

# Calcule a diferença das médias dos grupos 2,3 e 4 em relação ao grupo 1.
fit$coefficients

# Qual parametrização é usada no R? Interprete os coeficientes
# Casela de referencia
# (Intercept): média do tratamento gord1
# Trat2: Efeito do tratamento gord2 no valor esperado da resposta
# Trat3: Efeito do tratamento gord3 no valor esperado da resposta
# Trat4: Efeito do tratamento gord4 no valor esperado da resposta

summary(fit)
model.matrix(fit)
fit$coefficients

model.tables(fit, "means", se = TRUE)
sqrt(s2)*sqrt(1/6+1/6) #erro padrão da diferença entre médias


# Tukey HSD bands 
# Teste Tukey de comparações múltiplas
fit.tu <- TukeyHSD(fit)
fit.tu
plot(fit.tu) # Conclusão?
#Apenas o tratamento 2 e 4 são diferentes

# Calculando o valor crítico do método de Tukey
qtu <- qtukey(0.95,nmeans=K,df=n-K)
qtu

# I.C. para a diferença entre médias dos grupos
# Diferença entre os grupos 1 e 4
r <- 6 # número de réplicas (balanceado)
dif41 <- fit$coefficients[4]
deltatu <- (qtu/sqrt(2))*sqrt(s2)*sqrt(1/r+1/r)
ic.li41 <- dif41 - deltatu
ic.ls41 <- dif41 + deltatu
ic.li41
ic.ls41

# Diferença entre os grupos 2 e 4
dif42 <- fit$coefficients[4] - fit$coefficients[2]
ic.li42 <- dif42 - deltatu
ic.ls42 <- dif42 + deltatu
ic.li42
ic.ls42

deno <- sqrt(s2)*sqrt(1/r+1/r)
dif21 <- fit$coefficients[2]
dif31 <- fit$coefficients[3]
dif41 <- fit$coefficients[4]
dif32 <- fit$coefficients[3] - fit$coefficients[2]
dif42 <- fit$coefficients[4] - fit$coefficients[2]
dif43 <- fit$coefficients[4] - fit$coefficients[3]
t21 <- dif21/deno
t31 <- dif31/deno
t41 <- dif41/deno
t32 <- dif32/deno
t42 <- dif42/deno
t43 <- dif43/deno

pt21<- pt(c(abs(t21)), df=fit$df.residual, lower.tail=FALSE)
pt31<- pt(c(abs(t31)), df=fit$df.residual, lower.tail=FALSE)
pt41<- pt(c(abs(t41)), df=fit$df.residual, lower.tail=FALSE)
pt32<- pt(c(abs(t32)), df=fit$df.residual, lower.tail=FALSE)
pt42<- pt(c(abs(t43)), df=fit$df.residual, lower.tail=FALSE)
pt43<- pt(c(abs(t43)), df=fit$df.residual, lower.tail=FALSE)

results <- 2*c(pt21,pt31,pt41,pt32,pt42,pt43)
results  

p.adjust.methods
adjustp <- p.adjust(results,method="bonferroni")
cbind(results,adjustp)

adjustp <- p.adjust(results,method="fdr")
cbind(results,adjustp)

adjustp <- p.adjust(results,method="holm")
cbind(results,adjustp)
detach(base)
########################################
# Base de dados com Heterocedasticidade
########################################

resp <- c(2370,	1687,	2592,	2283,	2910,	3020, 1282,1527,	871,1025,825,	920, 562,321,636,317,	485,	842,173,127,132,	150,129,227)
treat <-factor(c(rep("gr1",6),rep("gr2",6),rep("gr3",6),rep("gr4",6)))
base<-data.frame(treat,resp)
base
attach(base)
# base<-data.frame(treat=factor(rep(1:4, each=6)), resp = resp)

boxplot(resp~treat)
#Note que a média e o desvio padrão decrescem juntos
#Qual o impacto disso na ANOVA?
# Fura com a suposição de homocedasticidade

tapply(resp,treat, mean)
tapply(resp,treat, sd)

fit1<- aov(resp ~ treat, data=base)
summary(fit1)

# Realize a análise de diagnóstico destes dados
# As hipóteses clássicas (QUAIS?) estão satisfeitas?

fit1$df.residual # igual a n - K
fit1$residuals
res<-residuals(fit1)
anov <- anova(fit1)

s2 <- anov$"Mean Sq"[2] # estimativa da variância residual
s2

par(mfrow=c(2,2))
plot(fit1)

# Verificação de independência
plot(res, type="l") # res x índice das observações
abline(h=0, col="red")

plot(fit1$fit, fit1$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

res <- fit1$res # extraindo resíduos
respad <- (res/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

# stem-and-leaf plot (diagrama de ramos e folhas)
sort(round(respad, 1))
stem(respad)

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico Normal de Probabilidade dos Resíduos")

# Teste de normalidade de Shapiro-Wilk

# Qual é a hipótese H0
# H0: Os dados seguem uma distribuição Normal

shapiro.test(res)  #conclusão?
# p-value > 0.05 -> não rejeito H0

# Teste de homogeneidade de variâncias

# Qual é a hipótese H0
# H0: Os dados tem variancias homegêneas
leveneTest(fit1)  #conclusão? 
# p-value < 0.05 -> rejeito H0

bartlett.test(resp~treat)   #conclusão?
# p-value < 0.05 -> rejeito H0

# Suposições
# Homocedasticidade: not ok
# Independencia: not ok
# Normalidade: ok

#Transformação Box-Cox (lambda não nulo): y*=(y^lambda - 1)/lambda
require(MASS)
boxcox(resp ~ treat, data=base, plotit=T)
boxcox(resp ~ treat, data=base, lam=seq(-0.5, 0.5, 1/10))
bc<-boxcox(resp ~ treat, data=base)
bc
max(bc$y) #max=-4.918963 ==> lambda=0.06
##O IC mostrado no gráfico contém o "0" logo y*=log(y)

# yt <- y^(lambda)

fit2<- aov(log(resp) ~ treat, data=base)

# Analise os dados transformados

fit2$df.residual # igual a n - K
fit2$residuals
res<-residuals(fit2)
anov <- anova(fit2)

s2 <- anov$"Mean Sq"[2] # estimativa da variância residual
s2

par(mfrow=c(2,2))
plot(fit2)

# Verificação de independência
plot(res, type="l") # res x índice das observações
abline(h=0, col="red")

plot(fit2$fit, fit2$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

res <- fit$res # extraindo resíduos
respad <- (res/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

# stem-and-leaf plot (diagrama de ramos e folhas)
sort(round(respad, 1))
stem(respad)

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico Normal de Probabilidade dos Resíduos")

# Teste de normalidade de Shapiro-Wilk

# Qual é a hipótese H0
# H0: Os dados seguem uma distribuição Normal

shapiro.test(res)  #conclusão?
# p-value > 0.05 -> não rejeito H0

# Teste de homogeneidade de variâncias

# Qual é a hipótese H0
# H0: Os dados tem variancias homegêneas
leveneTest(fit2)  #conclusão? 
# p-value > 0.05 -> não rejeito H0

bartlett.test(log(resp) ~ treat, data=base)   #conclusão?
# p-value > 0.05 -> não rejeito H0

# Suposições
# Homocedasticidade: ok
# Independencia: ok
# Normalidade: ok

detach(base)
############################
# Base com dados simulados 
############################

# Simularemos a partir do modelo linear com um fator em 3 níveis
# y = mu + tau + e

n <- 150 # número de réplicas
K <- 3 # número de grupos
#sigma = 2
mu <- 15

# dados do grupo 1
tau1 <- 10
sigma1 <- 2
e1 <- rnorm(n/K, 0, sigma1)
y1 <- mu + tau1 + e1

# dados do grupo 2
tau2 <- 20
sigma2 <- 4
e2 <- rnorm(n/K, 0, sigma2)
y2 <- mu + tau2 + e2

# dados do grupo 3
tau3 <- 30
sigma3 <- 8
e3 <- rnorm(n/K, 0, sigma3)
y3 <- mu + tau3 + e3

resp <- c(y1, y2, y3)
treat <- c(rep(1, n/K), rep(2,n/K), rep(3,n/K))
base.s <- as.data.frame(cbind(resp, treat))
base.s
base.s[,2] <- as.factor(base.s[,2])

base <- base.s

# Realize a análise de diagnóstico destes dados
# As hipóteses clássicas (QUAIS?) estão satisfeitas?

attach(base)

fit3<- aov(resp ~ treat, data=base)

# Analise os dados transformados

fit3$df.residual # igual a n - K
fit3$residuals
res<-residuals(fit3)
anov <- anova(fit3)

s2 <- anov$"Mean Sq"[2] # estimativa da variância residual
s2

par(mfrow=c(2,2))
plot(fit3)

# Verificação de independência
plot(res, type="l") # res x índice das observações
abline(h=0, col="red")

plot(fit3$fit, fit3$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

res <- fit3$res # extraindo resíduos
respad <- (res/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

# stem-and-leaf plot (diagrama de ramos e folhas)
sort(round(respad, 1))
stem(respad)

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico Normal de Probabilidade dos Resíduos")

# Teste de normalidade de Shapiro-Wilk

# Qual é a hipótese H0
# H0: Os dados seguem uma distribuição Normal

shapiro.test(res)  #conclusão?
# p-value < 0.05 -> rejeito H0

# Teste de homogeneidade de variâncias

# Qual é a hipótese H0
# H0: Os dados tem variancias homegêneas
leveneTest(fit3)  #conclusão? 
# p-value < 0.05 -> rejeito H0

bartlett.test(resp ~ treat, data=base)   #conclusão?
# p-value < 0.05 -> rejeito H0

# Suposições
# Homocedasticidade: not ok
# Independencia: not ok
# Normalidade: not ok

detach(base)

