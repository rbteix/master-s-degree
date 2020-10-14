library(caret)
set.seed(29)

data.svm <-data.gru.class.vars[,c(-1)]

#seleção de atributos
library(Boruta)
boruta.train <- Boruta(Regime~., data=data.svm, doTrace=2)

x11(width = 17)
plot(boruta.train)
## Boruta performed 10 iterations in 10.92638 secs.
## 7 attributes confirmed important: Av.ConMov, DesvPad.de.Spacing, Dia_Semana, HHI, Média.de.Spacing and 2 more;
## No attributes deemed unimportant.


boruta.sifnif <- getSelectedAttributes(boruta.train, withTentative=T)
imps <- attStats(boruta.train)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
imps2[order(-imps2$meanImp), ]  # descending sort

# # meanImp  decision
# # Regime.anterior    136.20668 Confirmed
# # HHI                 30.45459 Confirmed
# # DesvPad.de.Spacing  28.77559 Confirmed
# # Média.de.Spacing    28.75996 Confirmed
# # Mês                 22.80694 Confirmed
# # Av.ConMov           15.20862 Confirmed
# # Dia_Semana          14.74015 Confirmed

#Criando dummies
# dummies <- model.matrix(data.svm$Regime_t ~ data.svm$Regime_ant+  data.svm$Movimentos_programandos+
#                           data.svm$Dia_Semana+ data.svm$Mês, data.svm)
# dummies<- as.data.frame(dummies)


library(dummies)
dummies <- dummy.data.frame(data.svm, names = c("Dia_Semana", "Mês", "Regime.anterior"))
table(dummies$Regime) 


#Balanceamento base de dados
library(UBL)
dummies.balanced<- AdasynClassif(Regime~.,dummies, dist= "HEOM")
table(dummies.balanced$Regime)

# # 1    2    3 
# # 1234 1216 1204 


library(e1071)
set.seed(005)#Set seed so that same sample can be reproduced in future 
sample.svm <- sample.int(n = nrow(dummies.balanced), size = floor(.7*nrow(dummies.balanced)), replace=F)
treino.svm <- dummies.balanced[sample.svm,]
teste.svm <- dummies.balanced[-sample.svm,]



#Modelo inicial
svm.model <- svm(Regime~., data=treino.svm, cross=10, probability=T)

# # Call:
# #   svm(formula = Regime ~ ., data = treino.svm, cross = 10)
# # 
# # 
# # Parameters:
# #   SVM-Type:  C-classification
# # SVM-Kernel:  radial
# # cost:  1
# # gamma:  0.03846154
# # 
# # Number of Support Vectors:  1102
# # 
# # ( 400 329 373 )
# # 
# # 
# # Number of Classes:  3
# # 
# # Levels:
# #   1 2 3
# # 
# # 10-fold cross-validation on training data:
# # 
# #   Total Accuracy: 86.93782
# # Single Accuracies:
# #   89.80392 85.54688 89.0625 87.84314 86.32812 83.20312 89.01961 85.9375 86.32812 86.32812



svm.Treino=predict(svm.model,treino.svm)
svm.predic.Treino <- table(treino.svm$Regime, svm.Treino)
svm.predic.Treino
#svm.Treino
# # 1   2   3
# # 1 741 118  20
# # 2  81 725  55
# # 3  14  54 730


sum(diag(prop.table( svm.predic.Treino)))#taxa de acerto
## 0.8775909


svm.Teste=predict(svm.model, teste.svm, probability = T)
svm.predic.Teste <- table(teste.svm$Regime, svm.Teste)
svm.predic.Teste
# # svm.Teste
# #     1   2   3
# # 1 313  41   1
# # 2  36 315  25
# # 3  12  21 333


sum(diag(prop.table( svm.predic.Teste)))#taxa de acerto
##   0.8760255

library(caret)
confusionMatrix(svm.Teste, teste.svm$Regime )




#Tune
svm.tune.result <- tune(svm, Regime~., data=treino.svm, kernel="radial",
                        ranges = list(gamma=seq(0.01, 0.08, 0.0025),
                                      cost=2^(-1:1)))

# # Parameter tuning of 'svm':
# #   
# #   - sampling method: 10-fold cross validation 
# # 
# # - best parameters:
# #   gamma cost
# # 0.025    2
# # 
# # - best performance: 0.1294378  

windows()
plot(svm.tune.result)


#Desempenho
svm.Treino.tune=predict(svm.tune.result$best.model,treino.svm)
svm.predic.Treino.tune <- table(treino.svm$Regime, svm.Treino.tune)
svm.predic.Treino.tune
# # svm.Treino.tune
# # 1   2   3
# # 1 763 115   1
# # 2  73 731  36
# # 3  19  62 757


sum(diag(prop.table( svm.predic.Treino.tune)))#taxa de acerto
## 0.8803285


svm.Teste.tune=predict((svm.tune.result$best.model), type = "class",teste.svm)
svm.predic.Teste.tune <- table(teste.svm$Regime, svm.Teste.tune)
svm.predic.Teste.tune
# # svm.Teste.tune
# #     1   2   3
# # 1 315  40   0
# # 2  36 315  25
# # 3  13  21 332


sum(diag(prop.table( svm.predic.Teste.tune)))#taxa de acerto
##  0.8769371 


#############################################
#Treinando modelo com variáveis significativas cart
###################################################
treino.svm.cart <- treino.svm[,c(1,2,3,4,5,6,7,20,21,22,23,24,25)]
model.svm.cart <- svm(Regime~., data=treino.svm.cart, cross=10, probability=T )
# # Total Accuracy: 84.94329 
# # Single Accuracies:
# #   85.09804 85.9375 86.71875 80.78431 86.71875 82.8125 85.09804 86.71875 84.76562 84.76562 



svm.Treino.cart=predict(model.svm.cart,treino.svm.cart)
svm.predic.Treino.cart <- table(treino.svm.cart$Regime, svm.Treino.cart)
svm.predic.Treino.cart
#svm.Treino
# # 1   2   3
# # 1 744 121  14
# # 2  73 731  36
# # 3  52  67 719


sum(diag(prop.table( svm.predic.Treino.cart)))#taxa de acerto
##  0.8580368


#MODELO NÃO OBTEVE MELHOR PERFORMANCE

