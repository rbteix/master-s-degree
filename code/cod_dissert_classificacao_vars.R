#Continuação código novo_dissertação a partir de HMM

library(openxlsx)#import, export
library(depmixS4) #função posterior
getwd()# encontrar pasta work directory
setwd("C:/Users/bella/Documents/Mestrado/Projeto")

import <-posterior(hmm.fit3)$state
export <- write.xlsx(import, 'regime_anterior_export.xlsx')
imported <-read.xlsx('regime_anterior_import.xlsx') # arquivo localizado  na pasta default (Projeto)

#SELEÇÃO DOS DADOS
#data.gru.class <- data.gru2[-c(1032,2410),]
data.gru.class.vars <-data.fails.vars[c(-2410),] # pois retirando o regime anterior restam 2409 variáveis
data.gru.class.vars <- cbind(data.gru.class.vars, adata[c(-2410),3])
names(data.gru.class.vars)[7] = "Regime"

#Acrescentando dia da semana e mês
data.gru.class.vars$Dia_Semana <- weekdays(as.Date(data.gru.class.vars[,1]))
data.gru.class.vars$Mês <- months(as.Date(data.gru.class.vars[,1]))


#Adicionando coluna com 2411 linhas
data.gru.class.vars["Regime.anterior"] = c(imported)

data.gru.class.vars$Fatia = NULL

#Ordenando colunas
data.gru.class.vars = data.gru.class.vars[,c(1,7, 8, 6, 9, 2, 3, 4, 5)]
#Colocando valores da coluna movimentos entre 0 e 1
#data.gru.class$Movimentos_programandos = scales::rescale(data.gru.class$Movimentos_programandos , to=c(0,1))

#Transformando em  factor
data.gru.class.vars$Dia_Semana = as.factor(data.gru.class.vars$Dia_Semana)
data.gru.class.vars$Mês = as.factor(data.gru.class.vars$Mês)
data.gru.class.vars$Regime = as.factor(data.gru.class.vars$Regime)
data.gru.class.vars$Regime.anterior = as.factor(data.gru.class.vars$Regime.anterior)


#Reordenando  levels das  variáveis fator
data.gru.class.vars$Dia_Semana = factor(data.gru.class.vars$Dia_Semana, levels(data.gru.class.vars$Dia_Semana)[c(1,5,7,2,3,6,4)])
data.gru.class.vars$Mês = factor(data.gru.class.vars$Mês, levels(data.gru.class.vars$Mês)[c(5,4,9,1,8,7,6,2,12,11,10,3)])

#Convertendo para numérico

#Análise Descritiva
summary(data.gru.class.vars)

library(rpart)
library(partykit)
## Carregando pacotes exigidos: grid
## Carregando pacotes exigidos: libcoin
## Carregando pacotes exigidos: mvtnorm
## Warning messages:
##   1: package 'partykit' was built under R version 3.5.3
## 2: package 'libcoin' was built under R version 3.5.3
#library(ROSE)
#Loaded ROSE 0.0-3
#Warning message:package 'ROSE' was built under R version 3.5.3 

#visualização variável resposta
# windows()
# barplot(table(data.gru.class$Regime), col = "gray", main = "Variável resposta", space =c(2, 2, 2))
# Necessidade de fazer balanceamento dos dados
## R1, variável S1, 594/2409 = 0,24657
##R2, 1217/2412 =  0.1853234 = 0,5045
##R3, 599/2412 = 0.5480929 = 0,2483

# windows()
# #boxplot(Fatia~Regime_t, data = data.gru.class, main = "Fatia" ,col= "green" )
# boxplot(Movimentos_programandos~Regime_t, data = data.gru.class, main= "Movimentações Programadas", col="gray")
# 
#pal <- colorRampPalette(colors = c("lightblue", "blue"))(3)

windows()
barplot(table(data.gru.class.vars$Regime, data.gru.class.vars$Mês), main = "Mês", xlab="Classes", ylab="Frequência", legend=T)
windows()
barplot(table(data.gru.class$Regime, data.gru.class$Dia_Semana), main="Dia da Semana", xlab = "classes", ylab="Frequência", legend=T)
windows()
barplot(table(data.gru.class$Regime, data.gru.class$Regime.anterior), main="Regime Dia Anterior", xlab="classes", ylab="Frequência", legend=T, space =c(2, 2, 2)) 

library(reshape)
library(reshape2)#melt, cast function
library(dplyr)#pipeline
library(scales)#escalas de porcentagem
library(ggplot2)
library(scales)#scales porecentagem
library(tidyr)#function separate
library(plyr)#count function
library(gridExtra)



#TRANSFORMAÇÃO DOS DADOS
#Balanceamento da base de dados utilizando o tipo smoote, pacote não lida com ordered factors
library(UBL)
table(data.gru.class.vars$Regime)

dat <-data.gru.class.vars[,c(-1)]


dat$Mês <- factor(dat$Mês, ordered = FALSE)
dat$Dia_Semana <- factor(dat$Dia_Semana, ordered = FALSE)
dat$Regime <- factor(dat$Regime, ordered = FALSE)
data.gru.balanced<- AdasynClassif(Regime~.,dat, dist= "HEOM")
table(data.gru.balanced$Regime)
##1    2    3 
##1235 1216 1204 

#Amostras de treino e teste do modelo
#70% dos dados para treinamento e 30% para teste
set.seed(002)#Set seed so that same sample can be reproduced in future 
amostra <- sample.int(n = nrow(data.gru.balanced), size = floor(.7*nrow(data.gru.balanced)), replace=F)
treino <- data.gru.balanced[amostra,]
teste <- data.gru.balanced[-amostra,]
table(treino$Regime)
##1   2   3 
##894 819 845 

table(teste$Regime)

##1   2   3 
##341 397 359 


#MINERAÇÃO DE DADOS
#Etapa 1: conostrução da árvore de regressão e classificação

 fit <- rpart(Regime~., data=treino)
# # Call:
# #   rpart(formula = Regime ~ ., data = treino)
# # n= 2558 
# # 
# # CP nsplit rel error    xerror       xstd
# # 1 0.40204327      0 1.0000000 1.0000000 0.01449245
# # 2 0.25300481      1 0.5979567 0.5979567 0.01481793
# # 3 0.11117788      2 0.3449519 0.3449519 0.01268011
# # 4 0.01442308      3 0.2337740 0.2337740 0.01091443
# # 5 0.01081731      5 0.2049279 0.2109375 0.01045806
# # 6 0.01000000      6 0.1941106 0.2013221 0.01025389
# # 
# # Variable importance
# # Regime.anterior   Média.de.Spacing DesvPad.de.Spacing                HHI 
# #             52                 14                 14                  9 
# # Av.ConMov                Mês         Dia_Semana 
# #        7                  2                  2 
 library(ggplot2)
 library(tidyverse)
 
#Plot importância das variáveis
plot.vars <- data.frame(imp=fit$variable.importance)
x11(width=10)
plot.var.cart <- plot.vars %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(plot.var.cart) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  labs(y= "Importância", x="Variáveis")+
  ggtitle("Importância Variáveis CART")+
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

X11(width = 25)# Plot árvore
plot(as.party(fit), type="extended", use.n=F, ylab="Classificação")




#Etapa 2: validação cruzada
printcp(fit)##10-fold é a opçãop default 
# # Classification tree:
# #   rpart(formula = Regime ~ ., data = treino)
# # 
# # Variables actually used in tree construction:
# #   [1] DesvPad.de.Spacing Dia_Semana         HHI               
# # [4] Média.de.Spacing   Regime.anterior   
# # 
# # Root node error: 1664/2558 = 0.65051
# # 
# # n= 2558 
# # 
# #         CP nsplit  rel.error    xerror  xstd
# # 1 0.402043      0   1.00000   1.00000 0.014492
# # 2 0.253005      1   0.59796   0.59796 0.014818
# # 3 0.111178      2   0.34495   0.34495 0.012680
# # 4 0.014423      3   0.23377   0.23377 0.010914
# # 5 0.010817      5   0.20493   0.21094 0.010458
# # 6 0.010000      6   0.19411   0.20132 0.010254

#plot cp
windows()
plotcp(fit)

#plot árvore
library(rpart.plot)
X11(width = 20)
rpart.plot(fit)

#pela regra one-standard-deviation a árvore deve ser podada no
#cp=0.012 (continuando com 6 nós terminais). 
#Etapa 3: poda da árvore
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
poda_fit <- prune(fit, cp=bestcp)
#poda_fit <- prune(fit, cp=0.01)
summary(poda_fit)
#the same result as fit

windows()
plot(as.party(poda_fit), type="extended", use.n=F, ylab="Classificação")


###############################################################################3
#Etapa 4: Avaliação dos resultados
#Dados treino
poda.Treino <-predict(fit, type="class", treino)
conf.treino=table(treino$Regime, poda.Treino)
conf.treino #Matriz de confusão

# # poda.Treino
# # 1   2   3
# # 1 792  91  11
# # 2  79 697  43
# # 3  59  40 746


sum(diag(prop.table(conf.treino)))#taxa de acerto
##   0.8737295


#Dados teste
poda.Teste=predict(fit, type="class", teste)
conf.teste = table(teste$Regime, poda.Teste)
conf.teste #Matriz de confusão

# # poda.Teste
# # 1   2   3
# # 1 304  28   9
# # 2  34 345  18
# # 3  19  12 328


accuracy.test <- sum(diag(prop.table(conf.teste)))#taxa de acerto
##  0.8906108



#TUNING PARAMETERS
#########################cp=0#####################
# # rpart(formula = Regime ~ ., data = treino, method = "class", 
# #       control = control)
# # n= 2558 
# # 
# # CP nsplit rel error    xerror       xstd
# # 1 0.402043269      0 1.0000000 1.0000000 0.01449245
# # 2 0.253004808      1 0.5979567 0.5979567 0.01481793
# # 3 0.111177885      2 0.3449519 0.3449519 0.01268011
# # 4 0.014423077      3 0.2337740 0.2337740 0.01091443
# # 5 0.010817308      5 0.2049279 0.2091346 0.01042035
# # 6 0.008413462      6 0.1941106 0.2025240 0.01027982
# # 7 0.008000000      7 0.1856971 0.1977163 0.01017535
# # 
# # Variable importance
# # Regime.anterior DesvPad.de.Spacing   Média.de.Spacing                HHI 
# #             51                 15                 14                  8 
# # Av.ConMov                Mês         Dia_Semana 
# # 7                  2                  2 



accuracy.tune <- function(fit){
  poda.Teste=predict(fit, type="class", teste)
  conf.teste = table(teste$Regime, poda.Teste)
  accuracy.test <- sum(diag(prop.table(conf.teste)))#taxa de acerto
  accuracy.test
}

control <- rpart.control(  cp=0.008 ) #não muito distante de 0,01 proposto no cp ploy

tune.fit <- rpart(Regime~., data=treino, method = "class", control = control)
accuracy.tune(tune.fit)
##  0.8933455 #leve melhora

X11(width = 20)
rpart.plot(tune.fit) #agora com 7 nós

#######################################################################
#Árvore multiplicada por 100
#######################################################################
data.gru.balanced100 <- data.gru.balanced
data.gru.balanced100$Média.de.Spacing <- data.gru.balanced100$Média.de.Spacing*100
data.gru.balanced100$DesvPad.de.Spacing <- data.gru.balanced100$DesvPad.de.Spacing*100


set.seed(002)#Set seed so that same sample can be reproduced in future 
amostra100 <- sample.int(n = nrow(data.gru.balanced100), size = floor(.7*nrow(data.gru.balanced100)), replace=F)
treino100 <- data.gru.balanced100[amostra100,]
teste100 <- data.gru.balanced100[-amostra100,]
table(treino100$Regime)
##1   2   3 
##894 819 845 

table(teste100$Regime)

##1   2   3 
##341 397 359 


#MINERAÇÃO DE DADOS
#Etapa 1: conostrução da árvore de regressão e classificação

fit100 <- rpart(Regime~., data=treino100)

# # Call:
# #   rpart(formula = Regime ~ ., data = treino100)
# # n= 2558 
# # 
# #           CP nsplit rel error    xerror       xstd
# # 1 0.40204327      0 1.0000000 1.0000000 0.01449245
# # 2 0.25300481      1 0.5979567 0.5979567 0.01481793
# # 3 0.11117788      2 0.3449519 0.3449519 0.01268011
# # 4 0.01442308      3 0.2337740 0.2337740 0.01091443
# # 5 0.01081731      5 0.2049279 0.2109375 0.01045806
# # 6 0.01000000      6 0.1941106 0.2013221 0.01025389
# # 
# # Variable importance
# # Regime.anterior   Média.de.Spacing DesvPad.de.Spacing                HHI          Av.ConMov                Mês         Dia_Semana 
# #             52                 14                 14                  9                  7                  2                  2 

printcp(fit100)##10-fold é a opçãop default 
windows()
plotcp(fit100)

bestcp100 <- fit100$cptable[which.min(fit100$cptable[,"xerror"]),"CP"]
poda_fit100 <- prune(fit100, cp=0.012)
#poda_fit <- prune(fit, cp=0.01)
summary(poda_fit100)
#the same result as fit

x11(width=10)
windows()
plot(as.party(poda_fit100), type="extended", use.n=F, ylab="Classificação")
