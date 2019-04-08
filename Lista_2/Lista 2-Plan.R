library(ggplot2)
library(car)
library(asbio)
library(gmodels)

#Exercício 3

# item a
dados <- c(96 , 71, 73 , 87 , 104, 85 , 96 , 76, 
           92 , 89, 83 , 90 , 101, 110, 97 , 85, 
           112, 85, 100, 94 , 100, 73 , 101, 93, 
           93 , 81, 88 , 85)

dados.df <- as.data.frame(t(matrix(dados, 4, 7)))
colnames(dados.df) <- c("T1", "T2", "T3", "T4")
dados.df <- stack(dados.df )
colnames(dados.df) <- c("resp", "treat")

summary(dados.df)

ggplot(dados.df, aes(x=treat, y=resp)) +
  geom_boxplot(fill='#A4A4A4', color="black") 

# item c
model <- lm(resp~treat, dados.df)
model1 <- aov(resp~treat, dados.df)

anov <- anova(model)

knitr::kable(caption = "Tabela de ANOVA", anov)

# Verificação de independência
s2 <- anov$"Mean Sq"[2] 
res <- model1$res
par(mfrow=c(2,2))
plot(res, type="l",xlab="Observações", ylab="Resíduos")
abline(h=0, col="red")

plot(model$fit, model1$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

respad <- (res/sqrt(s2))
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL, xlab="Resíduos padronizados",ylab="Frequência")
title("Histograma dos Resíduos padronizados")


# Teste de normalidade de Shapiro-Wilk
shapiro.test(res)  

# Teste de homocedasticidade
leveneTest(model) 
bartlett.test(resp~treat,data = dados.df) 

# item d

fit.tu <- TukeyHSD(model1)
fit.tu
plot(fit.tu,cex.axis = 0.5)


# Calculando o valor crítico do método de Tukey
n <- dim(dados.df)[1]
K <- length(levels(dados.df$treat))

qtu <- qtukey(0.95,nmeans=K,df=n-K)

# I.C. para a diferença entre médias dos grupos
# Diferença entre os grupos 1 e 4
r <- 7 # número de réplicas (balanceado)
dif41 <- model$coefficients[4]
deltatu <- (qtu/sqrt(2))*sqrt(s2)*sqrt(1/r+1/r)
ic.li41 <- dif41 - deltatu
ic.ls41 <- dif41 + deltatu

# Diferença entre os grupos 2 e 4
dif42 <- model$coefficients[4] - model$coefficients[2]
ic.li42 <- dif42 - deltatu
ic.ls42 <- dif42 + deltatu


deno <- sqrt(s2)*sqrt(1/r+1/r)
dif21 <- model$coefficients[2]
dif31 <- model$coefficients[3]
dif41 <- model$coefficients[4]
dif32 <- model$coefficients[3] - model$coefficients[2]
dif42 <- model$coefficients[4] - model$coefficients[2]
dif43 <- model$coefficients[4] - model$coefficients[3]
t21 <- dif21/deno
t31 <- dif31/deno
t41 <- dif41/deno
t32 <- dif32/deno
t42 <- dif42/deno
t43 <- dif43/deno

pt21<- pt(c(abs(t21)), df=model$df.residual, lower.tail=FALSE)
pt31<- pt(c(abs(t31)), df=model$df.residual, lower.tail=FALSE)
pt41<- pt(c(abs(t41)), df=model$df.residual, lower.tail=FALSE)
pt32<- pt(c(abs(t32)), df=model$df.residual, lower.tail=FALSE)
pt42<- pt(c(abs(t43)), df=model$df.residual, lower.tail=FALSE)
pt43<- pt(c(abs(t43)), df=model$df.residual, lower.tail=FALSE)

results <- 2*c(pt21,pt31,pt41,pt32,pt42,pt43)

adjustp <- p.adjust(results,method="bonferroni")
cbind(results,adjustp)

adjustp <- p.adjust(results,method="fdr")
cbind(results,adjustp)

# item e

with(dados.df, pairw.anova(y=resp, x=treat, control="T1", method="dunnett"))

# item f

cmat <- rbind('(T1,T4) vs (T2,T3)' = c( 1, -1, -1,  1),   # Define a matriz dos contrastes ortogonais
              'T1 vs T4'         = c( 1,  0,  0, -1),
              'T2 vs T3'         = c( 0,  1, -1,  0))

model2 <- aov(resp ~ treat,
           data=dados.df,
           contrasts=list(treat=make.contrasts(cmat)))  # make.contrasts (gmodels): gera matriz dos contrastes

summary(model2,                                          # ANOVA com a SQDtra e GLtra desdobrados em contrastes ortogonais
        split=list(treat=list('(T1,T4) vs (T2,T3)'=1,
                            'T1 vs T4  '     =2,
                            'T2 vs T3'        =3)))

# Exercício 4

#item a

base.r <- c(28,34,31,
            26,29,25,
            31,25,27,
            28,31,29,
            35,29,28)
base.r <- as.data.frame(t(matrix(base.r,3,5)))
rownames(base.r) <- c("1","2","3","4","5")
colnames(base.r) <- c("Azul","Verde","Laranja")

base <- stack(base.r)
colnames(base) <- c("resp", "treat")

n <- dim(base)[1]
K <- length(levels(base$treat))

summary(base)

ggplot(base, aes(x=treat, y=resp)) +
  geom_boxplot(fill='#A4A4A4', color="black")  

# item b
(base$resp-mean(base$resp))/sqrt(var(base$resp))

fit <- aov(resp~treat, base)
anov <- anova(fit)

fit$df.residual

fit$residuals

fit$residuals/sqrt(anov[3]$`Mean Sq`[2])

rstudent(lm(base$resp~base$treat))

# item c

par(mfrow=c(2,2))
plot(fit)

mean(base.r$Verde) 
mean(base.r$Laranja)
mean(base.r$Azul)

s2 <- anov$"Mean Sq"[2] 
res <- fit$res
par(mfrow=c(2,2))
plot(res, type="l",xlab="Observações", ylab="Resíduos")
abline(h=0, col="red")

plot(fit$fit, fit$res, xlab="Valores Ajustados", ylab="Resíduos")
title("Resíduos vs Preditos")

respad <- (res/sqrt(s2))
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL, xlab="Resíduos padronizados",ylab="Frequência")
title("Histograma dos Resíduos padronizados")

shapiro.test(res)

leveneTest(fit)  
bartlett.test(resp~treat,base) 


# item d

knitr::kable(caption = "Tabela de ANOVA", anov)

# item e

fit$coefficients
model.tables(fit, "means", se = TRUE)

fit.tu <- TukeyHSD(fit)
fit.tu
plot(fit.tu)

qtu <- qtukey(0.95,nmeans=K,df=n-K)
qtu

r <- 5
dif.av <- fit$coefficients[2]
deltatu <- (qtu/sqrt(2))*sqrt(s2)*sqrt(1/r+1/r)
ic.liav <- dif.av - deltatu
ic.lsav <- dif.av + deltatu
ic.liav
ic.lsav

dif.al <- fit$coefficients[3]
ic.lial <- dif.al - deltatu
ic.lsal <- dif.al + deltatu
ic.lial
ic.lsal

dif.vl <- fit$coefficients[3] - fit$coefficients[2]
ic.livl <- dif.vl - deltatu
ic.lsvl <- dif.vl + deltatu
ic.livl
ic.lsvl

# Exercício 5

res.NA <- c(-2,26,-2,0 ,-4,
            0 ,-4,-6,NA, 0,
            -4,-2,2 ,-2,-4,
            NA,-2,2 ,-2,-4,
            12,-6,NA,0 ,NA,
            -2,NA,-2,-4,-6,
            2 ,-2,2 ,0 ,-2)

res <-   c(-2,26 ,-2,0 ,-4,
           0 ,-4,-6,-1, 0,
           -4,-2,2 ,-2,-4,
           -1,-2,2 ,-2,-4,
           12,-6,0 ,0 ,-4,
           -2,-2,-2,-4,-6,
           2 ,-2,2 ,0 ,-2)

res.df <- as.data.frame(t(matrix(res, 5, 7)))
colnames(res.df) <- c("T1", "T2", "T3", "T4","T5")
res.df <- stack(res.df)
colnames(res.df) <- c("resp", "treat")


gl.res <- length(res)-5 # Graus de liberdade n-k
res

s2 <- sum(res^2)/gl.res # estimativa da variância residual

# Verificação de independência
plot(res, type="l") # res x índice das observações
abline(h=0, col="red")

respad <- (res/sqrt(s2)) # resíduos padronizados
boxplot(respad)
title("Resíduos Padronizados")

hist(respad, main=NULL)
title("Histograma dos resíduos padronizados")

qqnorm(res,ylab="Residuos", main=NULL)
qqline(res)
title("Gráfico Normal de Probabilidade dos Resíduos")

# Teste de normalidade de Shapiro-Wilk
# Qual é a hipótese H0
shapiro.test(res) 

# Teste de homogeneidade de variâncias
bartlett.test(resp~treat,data = res.df) 
