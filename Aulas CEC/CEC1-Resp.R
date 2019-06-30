#################################################
#######   ALGUNS COMANDOS R               #######
#######   MAE0317 – 1o.SEM/2019           #######    
#######   Profa. Júlia M Pavan Soler      #######
####### #########################################

##CEC1: DCA
##Exemplo1
dados <- read.table('Clorofila.csv', head=T, sep=";", dec=".")
dados
str(dados)
dados$Trat <- factor(dados$Trat)
attach(dados)
boxplot(Resp ~Trat)
# verificar problemas: 
# pontos atípicos, assimetria e variância não homogênea

summary(dados[,1:4])
tapply(Resp,Trat,mean)
tapply(Resp,Trat,sd)
am<-tapply(Resp,Trat,mean)
plot(am)
points(am, pch="x", col=2, cex=1.5)

#Grafico com o perfil das médias
plot(am, type="l", main="Perfil de médias")
points(am, pch="x", col=2, cex=1.5)

#Ajuste do Modelo ANOVA-DCA
# Usando o comando aov
mod1 <- aov(Resp ~ Trat, data = dados)
names(mod1)
summary(mod1)
anova(mod1)
model.matrix(mod1)
mod1$coefficients 
# Matriz de delineamento com o intercepto (casela de referência)

#interprete os coeficientes do modelo
# (Intercept): média do tratamento T1
# Trat2: Efeito do tratamento T2 no valor esperado da resposta
# Trat3: Efeito do tratamento T3 no valor esperado da resposta
# Trat4: Efeito do tratamento T4 no valor esperado da resposta

#interprete descritivamente o valor F
#A fonte de variação do tratamento é cerca de 20 vezes maior 
# do que a fonte de variação do erro

#interprete as somas de quadrados
#SQTr é a variação entre os tratamentos
#SQRes é a variação dentro dos tratamentos

#QMRes estima qual parâmetro?
# Paramentro sigma^2

#há evidência para efeito significante de trat?
# sim pois o p-value é <0.05

#Quais suposições devem estar satisfeitas?
# Normalidade, homocedasticidade e independência

fit1<-anova(mod1)
round(fit1, digits=2)
dim(fit1)
fit1[1,]
fit1[,2]

model.tables(mod1, "means", se = TRUE)
#interprete os valores 3.31 e o valor 1.05
# 3.31 é a variação média dentro dos tratamentos
# 1.05 é o desvio padão dos tau's

names(mod1)

#construindo a coluna de tratamentos
trat<-gl(4,6) ##gl(k,r)=gera 1 fator em em k níveis com r réplicas
trat
trat<-gl(4,6,labels=c("Trat1", "Trat2", "Trat3","Trat4"))
trat
newdat<-cbind(dados$Resp,trat)
colnames(newdat)<-c("cla","trat")
newdat
detach(dados)
#Exemplo2
TA<-c(7.86,6.38,6.9,7.78,7.17)
TM<-c(6.2,7.82,8.5,6.5,7.09)
TS<-c(9.67,8.08,9.25,8.29,8.64)

##Realize a análise destes dados
##Refaça os comandos anteriores

trat <- gl(3,5) ##gl(k,r)=gera 1 fator em em k níveis com r réplicas

newdat<-stack(data.frame(cbind(TA,TM,TS)))
colnames(newdat)<-c("Resp","Trat")
newdat
attach(newdat)

boxplot(Resp ~Trat)
# verificar problemas: 
# pontos atípicos, assimetria e variância não homogênea

summary(data.frame(cbind(TA,TM,TS)))
tapply(Resp,Trat,mean)
tapply(Resp,Trat,sd)
am<-tapply(Resp,Trat,mean)
points(am, pch="x", col=2, cex=1.5)

#Grafico com o perfil das médias
plot(am, type="l", main="Perfil de médias")
points(am, pch="x", col=2, cex=1.5)

#Ajuste do Modelo ANOVA-DCA
# Usando o comando aov
mod2 <- aov(Resp ~ Trat, data = newdat)
summary(mod2)
anova(mod2)
model.matrix(mod2)
mod2$coefficients 
# Matriz de delineamento com o intercepto (casela de referência)

#interprete os coeficientes do modelo
# (Intercept): média do tratamento TA
# Trat2: Efeito do tratamento TM no valor esperado da resposta
# Trat3: Efeito do tratamento TS no valor esperado da resposta

#interprete descritivamente o valor F
#A fonte de variação do tratamento é cerca de 7 vezes maior 
# do que a fonte de variação do erro

#interprete as somas de quadrados
#SQTr é a variação entre os tratamentos
#SQRes é a variação dentro dos tratamentos

#QMRes estima qual parâmetro?
# Paramentro sigma^2

#há evidência para efeito significante de trat?
# sim pois o p-value é <0.05

#Quais suposições devem estar satisfeitas?
# Normalidade, homocedasticidade e independência

fit2<-anova(mod2)
round(fit2, digits=2)

model.tables(mod2, "means", se = TRUE)
#interprete os valores 3.31 e o valor 1.05
# 0.57 é a variação média dentro dos tratamentos
# 0.47 é o desvio padão dos tau's

#Exemplo 3: gere dados de um DCA com um fator em 3 níveis (grupos) e analise
#Use o comando rnorm(n, mean = 0, sd = 1)
#Simule cada grupo a partir do modelo linear y = a + bX + e
set.seed(23)
n <- 15 # número de réplicas
K <- 3 # número de grupos
sigma <- 2
mu <- 8

# dados do grupo 1
tau1 <- 10
e1 <- rnorm(n/K, 0, sigma)
y1 <- mu + tau1 + e1

# dados do grupo 2
tau2 <- 10
e2 <- rnorm(n/K, 0, sigma)
y2 <- mu + tau2 + e2

# dados do grupo 3
tau3 <- 20
e3 <- rnorm(n/K, 0, sigma)
y3 <- mu + tau3 + e3

resp <- c(y1, y2, y3)
treat <- c(rep(1, n/K), rep(2,n/K), rep(3,n/K))
base <- as.data.frame(cbind(resp, treat))
base[,2] <- as.factor(base[,2])

names(base)
is.factor(base$treat) # tratamento
is.numeric(base$resp) # resposta

n <- dim(base)[1]
K <- length(levels(base$treat))
summary(base)
plot(resp~treat, data=base)
av <- aov(resp~treat, base)
names(av)
summary(av)
# O que podemos concluir da análise?
# Existe diferença entre os tratamentos pois o p-value é <0.01

# Qual a diferença na média dos grupos
# 2 e 3 em relação ao grupo 1?
# u2-u1 =0.032 e u3-u1=7.87

av$coefficients
av$df.residual # igual a n - K
n-K
av$residuals
residuals(av)

ava<-anova(av)
dim(ava)
ava[2,3]
s2 <- ava$"Mean Sq"[2] # estimativa da variância residual

# Diagnóstico - Análise de resíduos
par(mfrow=c(2,2))
plot(av)

plot(av$fit, av$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

avres <- av$res # extraindo resíduos
respad <- (avres/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

# stem-and-leaf plot (diagrama de ramos e folhas)
sort(round(respad, 1))
stem(respad)

qqnorm(avres,ylab="Residuos", main=NULL)
qqline(avres)
title("Gráfico Normal de Probabilidade dos Resíduos")

# Teste de normalidade de Shapiro-Wilk
# H0: distribuição é normal
# p-valor > 0.05 não rejeita H0
shapiro.test(avres)

##########################################
# Usando o comando lm (regressão linear) #
##########################################

# Grupo 1 é a referência
# Qual a média do grupo 1?
# 19.38

# Qual a diferença na média dos grupos
# 2 e 3 em relação ao grupo 1?
# u2-u1 =0.032 e u3-u1=7.87

g <- lm(resp~treat, base)
summg <- summary(g)
summg
names(summg)

# Matriz de delineamento com o intercepto
# (casela de referência)
model.matrix(g)

# Ajuste removendo o intercepto
# Os resultados são os mesmos?
# Sim, o que muda é a parametrização (de médias)

# Podemos ler as médias dos grupos diretamente, mas
# os testes são adequados? Por que?
# Não, pois quer se testar se as médias são iguais ou os desvios são iguais a zero,
# com esse ajuste os testes perdem o sentido

gi <- lm(resp~treat -1, base)
summary(gi)

# Matriz de delineamento sem o intercepto
# (sem a casela de referência)
model.matrix(gi)
