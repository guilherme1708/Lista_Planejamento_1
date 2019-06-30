#################################################
#######   ALGUNS COMANDOS R               #######
#######   MAE0317 – 1o.SEM/2019           #######    
#######   Profa. Júlia M Pavan Soler      #######
####### #########################################
## Aula CEC06: Modelos de Efeitos Aleatórios
##Modelo de Um Único fator Aleatório

##Exemplo 1: Considere os dados a seguir do ganho de peso de bezerros #gerados do cruzamento de 
#uma amostra de 4 touros de uma fazenda de melhoramento genético de #bovinos de corte 

resp<-matrix(c(1.46, 1.17, .98, .95, 1.23, 1.08, 1.06, 1.10, 1.12, 1.20, 1.15, 1.07, 1.23,
               1.08, 1.11, 1.11, 1.02, 1.01, .83, .89, 1.15, .86, .86, 1.12),4,6)
resp <- t(resp)
resp 
#Neste caso, por que considerar modelos de efeitos aleatórios na análise destes dados?
# Os tratamentos provem de uma amostra aleatória

dat <- stack(as.data.frame(resp))
dat
dat <- cbind(c(rep(seq(1:6),4)),dat)
colnames(dat) <- c("ua","resp", "fa")
dat
str(dat)
attach(dat)

mfa <- tapply(resp,fa,mean)
mfa  #essas médias são valores do fator aletório fa (touro reprodutor)
mean(mfa) #estime mi, o ganho de peso médio desses animaos reprodutores
sd(mfa)/sqrt(6) #desvio padrão de mi

sfa <- tapply(resp,fa,sd)
sfa #estime o componente de variância sigma_e 

library(lattice)
dotplot(fa ~ resp, groups = ua, main = "Ganho de peso", dat=dat)

plot.default(fa, resp, main = "Ganho de peso")
points(mfa, pch="x", col=2, cex=1.5) #visualize a variação devido a sigma_A

#Ajuste do modelo de Efeito aleatório
#Qual modelo (estrutural) está sendo ajustado? Quais são as suposições adotadas?
# Y_ij = u + t_j + e_ij, t_j ~ N(0,s2_a) independente de e_ij ~ N(0, s2_e)
# Suposições: Normlidade, independência entre tratamentos, e homocedasticidade

library(nlme)
fit1<- lme(fixed = resp ~ 1, random = ~ 1|fa, data=dat)
fit1
summary(fit1) 
#Obtenha as estimativas de mi e dos componentes de variância
# mu = 1.0766
# s2_a = 0.005235
# s2_e = 0.015 -> fit1$sigma^2

#Obtenha estas estimativas via a Tabela de ANOVA
# mu = 1.076667 -> summary(fit1)$tTable[1]
# s2_a = 0.005077685
# s2_e = 0.015945 -> summary(fit1)$sigma^2


#fit <- lm(resp ~ as.factor(fa), data=dat); anova(fit)

#calcule o coef. de correlação intra-classe
#rho = s2_a/(s2_a+s2_e) = 0.2587102

a<-anova(fit1)
a  #Mostra o teste de H:mi=0

coef(fit1) 
#valores preditos de mi_j sob o modelo de ef. aleatório
#estes valores não correspondem às médias sob o modelo de ef. fixo (mfa)
random.effects(fit1) #BLUPs: valores preditos de tau_j

coef(fit1) #BLUPs de mi_j
random.effects(fit1) #BLUPs de tau_j
fitted(fit1, level = 0:1)

intervals(fit1)

intervals(fit1, which="fixed")
vcov(fit1) #variância de mi
fixed.effects(fit1)#BLUEs

plot(fit1) 
#análise de resíduos com base nos resíduos padronizados = res. de Pearson)

residuals(fit1,type="response") #resíduo condicional
#resíduo condicional = y_ij - mi_j = y_ij - mi - tau_j
residuals(fit1,type="p") #resíduos de Pearson = res. padronizado
#resíduo padronizado = (y_ij - mi_j)/sigma_e
residuals(fit1,level = 0:1) 
#"fixed" = resíduo condicional = e_ij
#"fa" = resíduo marginal (puro) = e_ij + tau_j

#residuals(fit1,type="normalized") : resíduo padronizado*fator de correção

summary(residuals(fit1,type="p"))
plot(fitted(fit1,level=1),residuals(fit1,type="p"))


#Outro recurso no R para ajuste de modelos de efeitos aleatórios
#Explore as facilidades destes recursos
library(lme4)
fit2<- lmer(resp ~ (1|fa),data=dat)
fit2
a2<-anova(fit2)
a2
summary(fit2)
coef(fit2)


########################################
#Supondo um modelo de efeito fixo
#Grafico com o perfil das médias
#Há interesse no caso do modelo de efeito fixo!!
plot(mfa, type="l", main="Perfil de médias")
points(mfa, pch="x", col=2, cex=1.5)

fit0<-aov(resp ~ fa,data = dat)
summary(fit0)
anova(fit0)
names(fit0)
coefficients(fit0)

fit0.tk <- TukeyHSD(fit0, "fa")
fit0.tk
plot(fit0.tk)

detach(dat)
########################################

##Analise os dados a seguir por meio de um modelon de ef. aleatórios
##Exemplo 2. Precisão de laboratórios: 25 amostras foram distribuídas a 5 labs para avaliação
Labs <- matrix(c(379, 357, 390, 376, 376,
                363, 367, 382, 381, 359,
                401, 402, 407, 402, 396,
                402, 387, 392, 395, 394,
                415, 405, 396, 390, 395),5,5)

dados <- stack(data.frame(Labs))
dados
dados <- cbind(c(rep(seq(1:5),5)),dados)
colnames(dados) <- c("ua","resp", "fa")
dados
str(dados)
attach(dados)

fit3 <- lmer(resp ~ (1|fa),data=dados)
fit3
a3 <-anova(fit3)
a3
summary(fit3)
coef(fit3)
