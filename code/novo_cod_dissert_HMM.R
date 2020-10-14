

# 
# setwd("C://Users//bella//Documents//Mestrado//Projeto")
# data.gru <- read.csv("dados_gr.csv", sep = ";")
# data.gru$Data<- NULL
# data.gru$Movimentações <- NULL
# head(data.gru)
# tail(data.gru)
# 
# 
# #Passo 2: Omitir dados não numéricos e fazer teste
# data.gru1 <- na.omit(data.gru)
# na.fail(data.gru1)
# 
# 
# 
# #Passo 3: Gerar gráficos  para analisar comportamentos
# windows()
# par(mfrow= c(4,1))
# plot(data.gru1[,2], type="l", xlab = "x" , ylab = "Frequência" , main = "Cancelados")
# plot(data.gru1[,3], type="l", xlab = "x" , ylab = "Frequência" , main = "Realizados")
# plot(data.gru1[,4], type="l", xlab = "x" , ylab = "Frequência" , main = "Realizados com atraso")
# plot(data.gru1[,5], type="l", xlab = "x" , ylab = "Frequência" , main = "Planejados")
# 
# 
# windows()
# plot(data.gru1[,2], data.gru1[,3], data.gru1[,4], data.gru1[,5], gpars)
# 
# 
# #Identificar outliers por coluna, encontrar mínimos e máximos e ordenar a coluna em crescente e decrescente para analisar os dados.
# # Vôos Cancelados
# min(data.gru1$Cancelado)
# max(data.gru1$Cancelado)
# 
# novo.vet1 <-order(data.gru1$Cancelado, decreasing = T)
# head(novo.vet1, n=10)
# data.gru1[novo.vet1, 2]
# 
# novo.vet2 <-order(data.gru1$Cancelado, decreasing = F)
# head(novo.vet2, n=10)
# data.gru1[novo.vet2, 2]
# 
# 
# #Vôos realizads
# min(data.gru1$Realizado)
# max(data.gru1$Realizado)
# 
# novo.vet3 <-order(data.gru1$Realizado, decreasing = TRUE)
# head(novo.vet3, n=10)
# data.gru1[novo.vet3, 3]
# 
# novo.vet4 <-order(data.gru1$Realizado, decreasing = F)
# data.gru1[novo.vet4, 3]
# head(novo.vet4, n=10)
# 
# 
# #Vôos realizados com atraso
# min(data.gru1$Realizado.com.atraso)
# max(data.gru1$Realizado.com.atraso)
# 
# novo.vet5 <-order(data.gru1$Realizado.com.atraso, decreasing = TRUE)
# head(novo.vet5, n=6)
# data.gru1[novo.vet5, 4]
# 
# novo.vet6 <-order(data.gru1$Realizado.com.atraso, decreasing = F)
# head(novo.vet6, n=6)
# data.gru1[novo.vet6, 4]
# 
# 
# #Retirando outliers
# data.gru2 <- data.gru1
# data.gru2<-data.gru2[!(data.gru2$"Realizado">=750 | 
#                          data.gru2$"Realizado"<=450 |
#                          data.gru2$"Realizado.com.atraso">=350 |
#                          data.gru2$"Cancelado" > 205), ]
# 
# #Métdosos de retirada de outliers
# # windows()
# # par(mfrow=c(2,2))
# # boxplot(log(data.gru2$Cancelado), main= "CanceladosLog")
# # boxplot(sqrt(data.gru2$Cancelado), main= "Cancelados")
# # boxplot(log(data.gru2$Realizado.com.atraso), main="AtrasoLog")
# # boxplot(sqrt(data.gru2$Realizado.com.atraso), main="Atraso")
# 
# # Na coluna de datas, converter factor em data
# head(data.gru2)
# Date <- data.gru2[,1]
# Date <- as.Date(Date, format = "%d/%m/%Y")
# 
# #windows()
# #plot.ts(data.gru2$Realizado)
# #Pré-Processamento de dados
# #Vôos Planejados 
# 
# 
# windows()
# par(mfrow=c(2,2))
# boxplot(data.gru2$Cancelado, main= "Cancelados")
# boxplot(data.gru2$Realizado, main="Realizados")
# boxplot(data.gru2$Realizado.com.atraso, main="Atraso")
# boxplot(data.gru2$Movimentos.Programados, main="Movimentos Programados" )
# 
# 
# 
# Planejados <- data.gru2$Movimentos.Programados 
# Planejados = data.gru2$Cancelado + data.gru2$Realizado + data.gru2$Realizado.com.atraso


#Continuação pré-processamento
###############CONTINUAÇÃO DE FIGURAS PRÉVIAS#######################
#Porcentagem de falhas nos vôos (cancelados + realizados com atraso) / total geral
data.gru2 <- data.gru1
data.fails<- data.frame(data.gru2$Cancelado+data.gru2$Realizado.com.atraso) / data.gru2$Movimentos.Programados
colnames(data.fails)= c("Fatia") #nomeando coluna

#add coluna data
Data <- data.gru2[,1] #variável data
data.fails["Data"]<-c(Data)#adcionando coluna

data.fails = data.fails[,c(2,1)] #reordenando



#variável para ggplot boxplot -OUTLIERS
plo.box.gg <- data.frame(data.fails[,-1])
colnames(plo.box.gg)= c("Fatia") #nomeando coluna

out<-ggplot(plo.box.gg, aes("", Fatia))+
  geom_boxplot(fill="gray")+
  labs(x= "")+
  theme_bw()+
  theme(text= element_text(size=15))


windows()#plot
out

#identificando outliers no boxplot e vetor 
outliers <- boxplot(data.fails, plot=F)$out #variável contento outliers
data.fails[which(data.fails$Fatia %in% outliers),] #finding outliers
data.fails <- data.fails[- which(data.fails$Fatia %in% outliers),]#removing outliers
nout.box.gg <- data.frame(data.fails[,-1])#variável para plotar, sem coluna data
colnames(nout.box.gg)= c("Fatia") #nomeando coluna

nout<-ggplot(nout.box.gg, aes("", Fatia))+
  geom_boxplot(fill="gray")+
  labs(x= "")+
  theme_bw()+
theme(text= element_text(size=15))

windows()
nout

#Plot dos dois gráficos em uma mesma figura
library(ggpubr)
X11(width = 10)
ggarrange(out, nout, labels = c("A", "B"))


# #Removendo outlier
# which.min(data.fails)
# ##[1] 1032
# data.fails <- data.fails[-c(1032)]
# #plot(data.fails
# windows()
# boxplot(data.fails, main="Fatia")
# windows()
# par(mfrow=c(3,1))
# plot(data.fails, main = "Fatia")
# data.fails <- as.data.frame(data.fails)
# colnames(data.fails)= c("Fatia") #nomeando coluna
# #Verificar se a série temporal é estacionária
# library(tseries)
# data.fails <- as.ts(data.fails)
# adf.test(data.fails)
# #O valor p sugere que os dados são muito improváveis, dada a hipótese nula, 
# #então acredita-se na hipótese alternativa (estacionária).
data.plot <- data.fails
#data.plot <- as.data.frame(data.plot)
#Date<- Date[-c(1032)]
#data.plot["Date"] <- c(Date) #incluindo coluna
#data.plot = data.plot[,c(2,1)]
#Coluna Planejados
# planejados <- data.gru2[-c(1032),5]
# planejados <- as.data.frame(planejados)
# colnames(planejados) <- c("Demanda")
# planejados["Date"] <- c(Date)
# planejados = planejados[, c(2,1)]
# #Plot Planejados
# windows()
# par(mfrow= c(2,1))
# plot(planejados, type="l", xlab = "Date" , ylab = "Frequência")

#Histograma observar o comportamento da série
# windows()
# h <-hist((data.fails[,2]),col="grey",main="Voos Atrasados e Cancelados\nHistogram of data", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência",
#          panel.first = grid(col = "gray"))
########################################
#Estatísticas Série Fatia
##################################
mean(data.fails[,2])
median(data.fails[,2])
max(data.fails[,2])
min(data.fails[,2])
sd(data.fails[,2])
quantile(data.fails[,2])

#coeficiente de variação da série
cv <-100*sd(data.fail)/mean(data.fail) #dado em porcentagem

#x <-unlist(data.fails)
# xfit<-seq(min(x),max(x),length= 50) 
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
#lines(xfit, yfit, col="red", lwd=2)




# windows()
# par(mfrow=c(2,2))
# plot(data.plot$Date, data.plot$Fatia, type = "l",xlab='Ano', ylab='Fatia de Voos Atrasados e Cancelados') 
# hist(data.fails,col="blue",main="Voos Atrasados e Cancelados\nHistogram of data", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# boxplot(data.fails,main="Voos Atrasados e Cancelados\nBoxplot of data", col = "blue")
# qqnorm(data.fails,main="Voos Atrasados e Cancelados\nGaussian QQ Plot")
#qqline(data.fails, col= "red")

#Converter em data frame
# data.matrix <- matrix(data.fails, length(data.fails), 1)
# data.matrix = data.frame(data.matrix)
# 


#Aplicação de quatro diferentes algorítimos para clusterizar o dataset: HMM, SOM, K-means e hierarchical clustering               
#Primeiro algorítmo: Cadeias de Markkov Escondidas (HMM, Hidden Markov Models)

#funcção para calcular aic and bic com kmeans
# aic.bic <- function(fit){
#   m = ncol(fit$centers)
#   n = length(fit$cluster)
#   k = nrow(fit$centers)
#   D = fit$tot.withinss
#   return(data.frame(AIC = D + 2*m*k,
#                     BIC = D + log(n)*m*k))}
# autocorrel <-acf(data.fails[,2])
# windows()
# plot(autocorrel, main= "")


#Plot série temporal
X11(width = 10)
  par(mfrow=c(2,1))
plot(data.fails, type = "l", ylab="Fatia", xlab="Ano", col="gray", panel.first = grid(col = "gray"), main="")


#HMM
set.seed(7)
library(depmixS4)
data.fail = data.frame(data.fails[,-1])
colnames(data.fail)= c("Fatia") #nomeando coluna


hmm.model2 <- depmix(Fatia~1, family = gaussian(),  nstates = 2, data=data.fail)
hmm.fit2 <- fit(hmm.model2, verbose=F) #encontra os melhores parâmetros (valores para os parâmetros)
##set.seed 33converged at iteration50 with logLik: 3703.556 
##7 52 with logLik: 3703.556 


#Modelos com 3 estados 
hmm.model3 <- depmix(Fatia~1, family = gaussian(), nstates = 3, data=data.fail)  #criar modelo
hmm.fit3 <- fit(hmm.model3, emcontrol=em.control(maxit = 5000)) #encontra os melhores parâmetros (valores para os parâmetros)
##converged at iteration102 with logLik: 3894.932 
##103 with logLik: 3894.932  

hmm.model4 <- depmix(Fatia~1, family = gaussian(), nstates = 4, data=data.fail)
hmm.fit4 <- fit(hmm.model4, verbose=F) #encontra os melhores parâmetros (valores para os parâmetros)
##converged at iteration 162 with logLik: 3944.6 
##  248 with logLik: 3941.263 

hmm.model5 <- depmix(Fatia~1, family = gaussian(), nstates = 5, data=data.fail)
hmm.fit5 <- fit(hmm.model5, verbose=F) #encontra os melhores parâmetros (valores para os parâmetros)
#post.prob5 <- posterior(hmm.fit5)
##converged at iteration  239 with logLik: 3993.325 
##225 with logLik: 3993.325  

hmm.model6 <- depmix(Fatia~1, family = gaussian(), nstates = 6, data=data.fail)
hmm.fit6 <- fit(hmm.model6, verbose=F) #encontra os melhores parâmetros (valores para os parâmetros)
##converged at iteration 215 with logLik: 4026.481 
##converged at  345 with logLik: 4026.482

#Plotar gráfico para confirmar se o número ideal de clusters para HMM é o mesmo
x11(width=10)
par(mfrow=c(1,2))
plot(2:6, c(AIC(hmm.fit2),AIC(hmm.fit3), AIC(hmm.fit4),AIC(hmm.fit5), AIC(hmm.fit6)),
     ty = "b", ylab = "AIC",xlab = "Número de grupos", main= "AIC", col="gray", lwd=2, panel.first = grid(col = "gray"))
plot(2:6, c(BIC(hmm.fit2),BIC(hmm.fit3), BIC(hmm.fit4),BIC(hmm.fit5), BIC(hmm.fit6)),
     ty = "b", ylab = "BIC",xlab = "Número de grupos", main = "BIC",  col="gray", lwd=2,panel.first = grid(col = "gray"))

fb <- forwardbackward(hmm.fit3)

#################################################################################################
# MÉDIA E DESVIO PADRÃO DE CADA ESTADO
################################################################################################
summary(hmm.fit3)
# Response parameters 
# Resp 1 : gaussian 
# Re1.(Intercept) Re1.sd
# St1           0.338  0.075
# St2           0.232  0.038
# St3           0.161  0.033


# Initial state probabilties model 
# pr1 pr2 pr3 
# 1   0   0 
# 
# Transition matrix 
# toS1  toS2  toS3
# fromS1 0.748 0.252 0.000
# fromS2 0.079 0.868 0.052
# fromS3 0.010 0.091 0.899
# ###########################################################################################
#set.seed7
# Initial state probabilties model 
# pr1 pr2 pr3 
# 1   0   0 
# 
# Transition matrix 
# toS1  toS2  toS3
# fromS1 0.746 0.254 0.000
# fromS2 0.127 0.804 0.069
# fromS3 0.015 0.119 0.866
# 
# Response parameters 
# Resp 1 : gaussian 
# Re1.(Intercept) Re1.sd
# St1           0.297  0.046
# St2           0.221  0.033
# St3           0.156  0.030

#Curva gaussian para testar os melhores valores loglikelihood (necessário menor BIC- Bayesian Information Criterion(número de parâmetros))

# Classification (inference task)
post.prob3 <- posterior(hmm.fit3) # Compute probability of being in each state
# windows()
# par(mfrow=c(3,1))
# matplot(post.prob3[,-1], type = "l", ylab = "Probability", xlab= "Days", main = "Probabilidades Posteriores dos Estados", col = c("red", "yellow", "green"))

rowSums(head(post.prob3)[,2:4]) # Check that probabilities sum to 1

# Output both the true regimes and the
# posterior probabilities of the regimes
X11(width = 10)
layout(1:2)
#plot(data.fails,main="Fatia de Voos Atrasados e Cancelados\nSérie Histórica", xlab='', ylab='Voos Atrasados e Cancelados')
#plot(data.fails, main="Fatia de Voos Atrasados e Cancelados", xlab='', ylab='Voos Atrasados e Cancelados')
matplot(post.prob3[,-1], type = "l", ylab = "Probabilidade", xlab= "Dias", main="",  col = c("red", "yellow", "green"))
legend(x='bottomright', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', bg= "gray90", box.col = "green4", xjust=0)
# matplot(post.prob3[1:350,-1], type = "l", ylab = "Probabilidade", xlab= "Dias", main = "Probabilidade Posterior dos Estados",  col = c("red", "yellow", "green"))
# legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)

windows()
layout(1:2)
plot(posterior(hmm.fit3)$state, type='s', main='Estados Escondidos', xlab='', ylab='Estado', col="gray", lwd=1)
plot(posterior(hmm.fit3)$state[1:350], type='s', main='Estados Escondidos', xlab='', ylab='Estado', col="gray", lwd=2)
######################################################



#PLOT SERIE TEMPORAL, REGIMES E PROBABILIDADE POSTERIOR DOS PRIMEIROS 250 DIAS DE 2011
X11(width = 10)
layout(1:3)
plot(data.fails[1:250,2], type = "l", ylab="Fatia", xlab="", col="gray", panel.first = grid(col = "gray"), main="Série Temporal Fatia")
plot(post.prob3$state[1:250], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[1:338,])], lwd=2)
legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
matplot(post.prob3[1:250,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2011)", main = "Probabilidade Posterior dos Regimes",  col = c("red", "yellow", "green"))
legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)



#PLOT COMPARANDO REGIMES E PROBABILIDADE POSTERIOR
yticks <- c(1, 2, 3)
X11(width = 10)
layout(1:3)
 plot(post.prob3$state[1:338], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[1:338,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[1:338,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2011)", main = "Probabilidade Posterior dos Regimes",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 g1 <-ggplot( adata[1:338,], aes(Data, Regime, color =(hmmregime[1:338,]))) +geom_bar(stat="identity", alpha=I(0.7))+  scale_color_gradientn(colours = mycols) + ylab("Regime") + labs(color = "Regimes")  + theme_bw()+
   geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+ labs(x="Ano 2011")
 
 
 X11(width = 10)
 layout(1:3)
 plot(post.prob3$state[339:693], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[339:693,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[339:693,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2012)", main = "Probabilidade Posterior dos Regimes",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 X11(width = 10)
 layout(1:3)
 plot(post.prob3$state[694:1047], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[694:1047,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[694:1047,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2013)", main = "Probabilidade Posterior dos Estados",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 X11(width = 10)
 layout(1:3)
 plot(post.prob3$state[1048:1331], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[1048:1331,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[1048:1331,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2014)", main = "Probabilidade Posterior dos Estados",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 X11(width = 10)
 layout(1:3)
 plot(post.prob3$state[1332:1689], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[1332:1689,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[1332:1689,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2015)", main = "Probabilidade Posterior dos Regimes",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 X11(width = 10)
 layout(1:3)
 plot(post.prob3$state[1690:2051], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[1690:2051,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[1690:2051,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2016)", main = "Probabilidade Posterior dos Estados",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 X11(width = 10)
 layout(1:3)
 plot(post.prob3$state[2052:2410], type='h', main='Regimes Estimados', xlab='', ylab='Regimes', col=c("red", "yellow", "green")[(hmmregime[2052:2410,])], lwd=2,  yaxt="none")
 axis( 2, at = yticks, labels = yticks)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 matplot(post.prob3[2052:2410,-1], type = "l", ylab = "Probabilidade", xlab= "Dias (2017)", main = "Probabilidade Posterior dos Estados",  col = c("red", "yellow", "green"))
 legend(x='topleft', c('R1','R2', 'R3'), fill=c("red", "yellow", "green"), bty='n', xjust=0)
 
 


# windows()
# hist(ts(posterior(hmm.fit3)[,1]), main="Frequencia Absoluta dos Regimes",xlab="Regimes", ylab = "Frequência", col = "grey")


#Plot séries separadas
windows()
par(mfrow=c(3,1))
plot(ts(posterior(hmm.fit3)[1:350,2]),ylab="probabilidade", xlab = "Dias", main="Probabilidade Posterior do Regime 1",frame=FALSE, col="red", lwd = 1 )
plot(ts(posterior(hmm.fit3)[1:350,3]),ylab="probabilidade", xlab = "Dias", main="Probabilidade Posterior do Regime 2",frame=FALSE, col="yellow", lwd = 1)
plot(ts(posterior(hmm.fit3)[1:350,4]),ylab="probabilidade", xlab = "Dias", main="Probabilidade Posterior do Regime 3",frame=FALSE, col="green", lwd = 1)


s1 <- which(posterior(hmm.fit3)[,1]==1)
s2 <- which(posterior(hmm.fit3)[,1]==2)
s3 <- which(posterior(hmm.fit3)[,1]==3)

a=cbind(data.fail, posterior(hmm.fit3)[,1])
a=as.data.frame(a)
a1 <- a[s1,]
a2 <- a[s2,]
a3 <- a[s3,]

# windows()
# par(mfrow=c(2,3))
# qqnorm(a1[,1],main="Voos Atrasados e Cancelados Regime 1\nGaussian QQ Plot") 
# #qqline(a1[,1], col="red")
# qqnorm(a2[,1],main="Voos Atrasados e Cancelados Regime 2\nGaussian QQ Plot")
# #qqline(a2[,1], col="red")
# qqnorm(a3[,1],main="Voos Atrasados e Cancelados Regime 3\nGaussian QQ Plot")
# #qqline(a3[,1], col="red")
# hist(a1[,1], main="Voos Atrasados e Cancelados Regime 1\nHistogram", col ="blue")
# hist(a2[,1], main="Voos Atrasados e Cancelados Regime 2\nHistogram", col ="blue")
# hist(a3[,1], main="Voos Atrasados e Cancelados Regime 3\nHistogram", col ="blue")

library(gridExtra)# for generating the bin width comparison plot
library(ggplot2) 


compare <- a
compare["Fatia"] <- a[,1] 
compare["Regime"] <- a[,2]
#compare [,c(1,3)]<- NULL

value <- a$Regime
#HOSTOGRAMA REGIMES MESMA ESCALA
windows()
  ggplot(compare,aes(Fatia, fill = Regime)) +
  geom_histogram(position = "identity", binwidth = 0.02, colour="gray")+
  labs( y="Frequência de voos")+
  facet_grid(Regime ~ .) +
  scale_fill_gradient(limits = c(1,3), breaks = c(1,2,3),high="green",  low = "red")+ 
  labs(title = "Histogramas Regimes") +
  theme_bw()



#histigramas e qq plot
# x11(width=10)
# par(mfrow=c(1,2))
# h <-hist(a1[,1],col="blue",main="Voos Atrasados e Cancelados Regime 1\nHistogram", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# x <- a1[,1]
# xfit<-seq(min(x),max(x),length= 50) 
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
#lines(xfit, yfit, col="red", lwd=2)
#qqnorm(a1[,1],main="Voos Atrasados e Cancelados Regime 1\nGaussian QQ Plot") 
#qqline(a1[,1], col="red")


# windows()
# par(mfrow=c(1,2))
# h2 <-hist(a2[,1],col="blue",main="Voos Atrasados e Cancelados Regime 2 \nHistogram", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# x2 <- a2[,1]
# xfit2<-seq(min(x2),max(x2),length= 50) 
# yfit2<-dnorm(xfit2,mean=mean(x2),sd=sd(x2)) 
# yfit2 <- yfit2*diff(h2$mids[1:2])*length(x2) 
#lines(xfit2, yfit2, col="red", lwd=2)
#qqnorm(a2[,1],main="Voos Atrasados e Cancelados Regime 2\nGaussian QQ Plot")
#qqline(a2[,1], col="red")

# windows()
# par(mfrow=c(1,2))
# h3<-hist(a3[,1],col="blue",main="Voos Atrasados e Cancelados\nHistogram ", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# x3<- a3[,1]
# xfit3<-seq(min(x3),max(x3),length= 50) 
# yfit3<-dnorm(xfit3,mean=mean(x3),sd=sd(x3)) 
# yfit3 <- yfit3*diff(h3$mids[1:2])*length(x3) 
#lines(xfit3, yfit3, col="red", lwd=2)
#qqnorm(a3[,1],main="Voos Atrasados e Cancelados Regime 3\nGaussian QQ Plot")
#qqline(a3[,1], col="red")

# x11(width=10)
# par(mfrow=c(1,3))
# h <-hist(a1[,1],col="blue",main="Voos Atrasados e Cancelados Regime 1\nHistogram", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# #lines(xfit, yfit, col="red", lwd=2)
# h2 <-hist(a2[,1],col="blue",main="Voos Atrasados e Cancelados Regime 2 \nHistogram", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# #lines(xfit2, yfit2, col="red", lwd=2)
# h3<-hist(a3[,1],col="blue",main="Voos Atrasados e Cancelados\nHistogram ", xlab = "Fatia de Atrasos e Cancelamentos", ylab = "Frequência")
# #lines(xfit3, yfit3, col="red", lwd=2)
# qqnorm(a1[,1],main="Voos Atrasados e Cancelados Regime 1\nGaussian QQ Plot") 
# #qqline(a1[,1], col="red")
# qqnorm(a2[,1],main="Voos Atrasados e Cancelados Regime 2\nGaussian QQ Plot")
# #qqline(a2[,1], col="red")
# qqnorm(a3[,1],main="Voos Atrasados e Cancelados Regime 3\nGaussian QQ Plot")
# #qqline(a3[,1], col="red")
# 



#box plot

# x11(width=10)
# par(mfrow=c(1,3))
# boxplot(a1[,1],main="Voos Atrasados e Cancelados Regime 1\nBoxplot", col = "blue")
# boxplot(a2[,1],main="Voos Atrasados e Cancelados Regime 2\nBoxplot", col = "blue")
# boxplot(a3[,1],main="Voos Atrasados e Cancelados Regime 3\nBoxplot ", col = "blue")
# 
#comparando boxplot



#Plotar gráficos hmm.fit3
#Adicionando a coluna Data.1 ao data
# data.matrix.date <- data.matrix
# data.matrix.date["Date"] <- c(Date)
# data.matrix.date["Value"] <- c(data.matrix)
# data.matrix.date = data.matrix.date[c(2,3)]
# 
# 
# dfu <- cbind(data.matrix.date, post.prob3[,2:4])
# #Colocando em um longo formato
# library(reshape2)
# dfu<- melt(dfu, id="Date")
# 
# #Plotando o dataset com séries temporais de probabilidades
# library(ggplot2)
# windows()
# qplot(Date, value, data=dfu, geom ="line",
#       main = paste("States"),ylim = c(0, 1),
#       ylab = "State Probabilities") +
#   facet_grid(variable~., scales = "free_y") + theme_bw()
# 

data.matrix.date <- as.data.frame(data.fails)
data.matrix.date["Anos"] <- c(Date)
data.matrix.date["Fatia"] <- c(data.fails)
data.matrix.date = data.matrix.date[c(2,1)]


dfu <- cbind(data.matrix.date, post.prob3[,2:4])
#Colocando em um longo formato
library(reshape2)
dfu<- melt(dfu, id="Anos")

#Plotando o dataset com séries temporais de probabilidades
library(ggplot2)

# windows()
# par(mfrow = c(1,2))
# qplot(Anos, value, data=dfu, geom ="line",
#       main = paste("Regimes"),ylim = c(0, 1),
#       ylab = "Probabilidade dos Regimes") +
#   facet_grid(variable~., scales = "free_y") + theme_bw()
# 


#coeficiente de variação da série
cv <-100*sd(data.fail)/mean(data.fail) #dado em porcentagem
##[1] 29.11543

#coeficiente de variação dos regimes
data.cv1 <- a1[,1]
cv1 <-sd(a1[,1]/mean(a1[,1]))
##[1] 0.1939392

data.cv2 <- a2[,1]
cv2 <-sd(a2[,1]/mean(a2[,1])) 
##[1] 0.1609325

data.cv3 <- a3[,1]
cv3 <-sd(a3[,1]/mean(a3[,1])) 
##[1] 0.1566196


adata <- as.data.frame(data.fail)
hmmregime <-as.data.frame(hmm.fit3@posterior$state)
as.data.frame(hmmregime)
hmmregime[1:350,]
adata["Regime"] <- c(hmmregime)
adata["Data"] <- c(data.fails$Data) #incluindo colona
adata <- adata[,c(3,1,2)]
# windows()
# plot(adata, col=hmm.fit3@posterior$state)


#adata <- cbind(Date, data.fails, hmmregime)
#windows()
#plot(adata, col= hmm.fit3@posterior$state, type ="l")
# 
library(ggplot2)
library(gridExtra)
# adata <-as.data.frame(adata)
# g1 <-ggplot( adata[1:338,], aes(Date, Fatia, color =(hmmregime[1:338,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3)) + ylab("Fatia") + labs(color = "Regimes")  + theme_bw()
# g2 <-ggplot( adata[339:693,], aes(Date, Fatia, color =(hmmregime[339:693,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes") + theme_bw()
# g3 <-ggplot( adata[694:1047,], aes(Date, Fatia, color =(hmmregime[694:1047,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes") + theme_bw()
# x11(width=10)
# grid.arrange(g1, g2, g3, nrow = 3)
#  
# 
# 
# g4 <-ggplot( adata[1048:1331,], aes(Date, Fatia, color =(hmmregime[1048:1331,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes") + theme_bw()
# #g4.1 <- ggplot( adata[1050:1080,], aes(Date, Fatia, color =(hmmregime[1050:1080,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes") + theme_bw()
# 
# x11(width=10)
# grid.arrange(g4, g4.1, nrow=3)
# 
# g5 <-ggplot( adata[1332:1689,], aes(Date, Fatia, color =(hmmregime[1332:1689,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes") +theme_bw()
# g6 <-ggplot( adata[1690:2051,], aes(Date, Fatia, color =(hmmregime[1690:2051,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes")+theme_bw()
# g7<-ggplot( adata[2052:2410,], aes(Date, Fatia, color =(hmmregime[2052:2410,]))) +geom_line() + scale_color_gradientn(colours = rainbow(3))+ ylab("Fatia") + labs(color = "Regimes") +theme_bw()
# 
# x11(width=10)
# grid.arrange(g5, g6, g7, nrow = 3)


#Plotar toda a serie
# windows()
# ggplot( adata, aes(Date, Fatia, color =(Regimes))) +geom_line() + scale_color_gradientn(colours = rainbow(3))
# 



library(e1071)                    
skewness(data.fails) #coeficiente de variância
##[1] 0.6614355

#Gráfico dispersão de quartis com limites
#library(car)
#windows()
#qqPlot(data.fails, distribution = "norm")

#x11(width=10)
#par(mfrow =c(1,3))
#qqPlot(a1[,1], distribution = "norm")
#qqPlot(a2[,1], distribution = "norm")
#qqPlot(a3[,1], distribution = "norm")
# 
# windows()
# boxplot(a1[,1], a2[,1], a3[,1],names= c("Regime 1", "Regime 2", "Regime 3"),col = "blue", main= "Voos atrasados e cancelados")
# 
# r1<-data.frame(a1[,1])
# names(r1)[1] <-"Fatia" #renomeando coluna
# 
# r2<-data.frame(a2[,1])
# names(r2)[1] <-"Fatia" #renomeando coluna
# 
# r3<-data.frame(a3[,1])
# names(r3)[1] <-"Fatia" #renomeando coluna
# 

# 
# 
# 
# 
# R1<-ggplot(r1, aes("", Fatia))+
#   geom_boxplot(fill="gray")+
#   labs(x= "")+
#   theme_bw()
# 
# 
# R2<-ggplot(r2, aes("", Fatia))+
#   geom_boxplot(fill="gray")+
#   labs(x= "")+
#   theme_bw()
# 
# R3<-ggplot(r3, aes("", Fatia))+
#   geom_boxplot(fill="gray")+
#   labs(x= "")+
#   theme_bw()
# 
# x11(width=10)
# ggarrange(R1, R2, R3, labels = c("R1", "R2", "R3"))

names(a)[2] <-"Regime"

r.melt <- post.prob3$state
r.melt <-melt(a, id.vars = "Regime")
g.melt<- mutate(group_by(r.melt, Regime)) %>%
  ungroup()

####BOXPLOT##############################
windows()
ggplot(a, aes(x=factor(Regime), y=Fatia))+
  labs(x="Regime")+
  geom_boxplot(fill= c("red", "yellow", "green"))+
  theme_minimal()

#BARRAS, valor absoluto
windows()
ggplot(a, aes(x=factor(Regime)))+
  labs(x="Regime", y="Frequência")+
  geom_bar( fill="gray", width=0.5)+
  #theme(axis.title=element_text(size=14))+
  #theme(axis.text.x =element_text(size=20))+ tentativa de aumentar a fonte
  theme_minimal()

df <- adata
df$Regimes=as.factor(df$Regimes)
df.melt <-melt(adata, id.vars = "Regimes")
g.melt<- mutate(group_by(r.melt, Regime )) %>%
  ungroup()

#PLOT MENSAIS DOS REGIMES###################################################
library(depmixS4)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)


mycols <- c("red", "yellow", "green" )

adata <-as.data.frame(adata)

g1 <-ggplot( adata[1:338,], aes(Data, Regime, color =(hmmregime[1:338,]))) +geom_bar(stat="identity", alpha=I(0.7))+  scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3")) + ylab("Regime") + labs(color ="Regimes")  + theme_bw()+
geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+ labs(x="Ano 2011")
g2 <-ggplot( adata[339:693,], aes(Data, Regime, color =(hmmregime[339:693,]))) +geom_bar(stat="identity", alpha=I(0.7))+  scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3"))+ ylab("Regime") + labs(color = "Regimes") + theme_bw()+
geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+ labs(x="Ano 2012")
g3 <-ggplot( adata[694:1047,], aes(Data, Regime, color =(hmmregime[694:1047,]))) +geom_bar(stat="identity", alpha=I(0.7))+  scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3"))+ ylab("Regime") + labs(color = "Regimes") + theme_bw()+
geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+ labs(x="Ano 2013")
g4 <-ggplot( adata[1048:1331,], aes(Data,  Regime, color =(hmmregime[1048:1331,])))  +geom_bar(stat="identity", alpha=I(0.7))+ scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3"))+ ylab("Regime") + labs(color = "Regimes") + theme_bw()+
  geom_line() +  scale_x_date(date_labels = "%b", date_breaks = "1 month")+labs(x="Ano 2014")
x11(width=10)
grid.arrange(g1, g2, g3, g4, g5, g6, g7, nrow = 7)



g5 <-ggplot( adata[1332:1689,], aes(Data,  Regime, color =(hmmregime[1332:1689,]))) + geom_bar(stat="identity", alpha=I(0.7)) + scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3"))+ ylab("Regime") + labs(color = "Regimes") +theme_bw()+
  geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+ labs(x="Ano 2015")
g6 <-ggplot( adata[1690:2051,], aes(Data,  Regime, color =(hmmregime[1690:2051,])))  +geom_bar(stat="identity", alpha=I(0.7)) + scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3"))+ ylab("Regime") + labs(color = "Regimes")+theme_bw()+
  geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+ labs(x="Ano 2016")
g7<-ggplot( adata[2051:2410,], aes(Data,  Regime, color =(hmmregime[2051:2410,])))  +geom_bar(stat="identity", alpha=I(0.7)) + scale_color_gradientn(colours = mycols, breaks = c(1,2,3), labels=c("1","2","3"))+ ylab("Regime") + labs(color = "Regimes") +theme_bw()+
  geom_line() + scale_x_date(date_labels = "%b", breaks = "month")+labs(x="Ano 2017")
x11(width=10)
grid.arrange(g5, g6, g7, nrow = 4)
## Warning message:
##  Removed 3 rows containing missing values (position_stack)

