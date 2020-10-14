library(caret)
set.seed(29)

data.svm <-data.gru.class[,c(-1,-2)]

#seleção de atributos
library(Boruta)
boruta.train <- Boruta(Regime~., data=data.svm, doTrace=2)

x11(width = 10)
plot(boruta.train)
# # Boruta performed 48 iterations in 23.95752 secs.
# # 3 attributes confirmed important: Dia_Semana, Mês, Regime.anterior;
# # No attributes deemed unimportant.

boruta.sifnif <- getSelectedAttributes(boruta.train, withTentative=T)
imps <- attStats(boruta.train)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# # meanImp  decision
# # Regime.anterior 259.036440 Confirmed
# # Mês              20.185794 Confirmed
# # Dia_Semana        4.164016 Confirmed

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
# # 1238 1216 1173 


library(e1071)


set.seed(005)#Set seed so that same sample can be reproduced in future 
sample.svm <- sample.int(n = nrow(dummies.balanced), size = floor(.7*nrow(dummies.balanced)), replace=F)
treino.svm <- dummies.balanced[sample.svm,]
teste.svm <- dummies.balanced[-sample.svm,]



#Modelo inicial
svm.model <- svm(Regime~., data=treino.svm, cross=10)

# # Call:
# #   svm(formula = Regime ~ ., data = treino.svm, kerel = "radial", 
# #       cross = 10)
# # 
# # 
# # Parameters:
# #   SVM-Type:  C-classification 
# # SVM-Kernel:  radial 
# # cost:  1 
# # gamma:  0.04545455 
# # 
# # Number of Support Vectors:  1159
# # 
# # ( 419 414 326 )
# # 
# # 
# # Number of Classes:  3 
# # 
# # Levels: 
# #   1 2 3
# # 
# # 10-fold cross-validation on training data:
# #   
# #   Total Accuracy: 84.67297 
# # Single Accuracies:
# #   85.37549 88.18898 82.28346 89.76378 81.88976 81.02767 85.03937 77.95276 86.61417 88.58268 




svm.Treino=predict(svm.model,treino.svm)
svm.predic.Treino <- table(treino.svm$Regime, svm.Treino)
svm.predic.Treino
##svm.Treino
# # 1   2   3
# # 1 741 118  20
# # 2  81 725  55
# # 3  14  54 730

sum(diag(prop.table( svm.predic.Treino)))#taxa de acerto
## 0.8652482


svm.Teste=predict(svm.model, type = "class",teste.svm)
svm.predic.Teste <- table(teste.svm$Regime, svm.Teste)
svm.predic.Teste
##svm.Teste
# # 1   2   3
# # 1 301  53   5
# # 2  28 309  18
# # 3  17  32 326

sum(diag(prop.table( svm.predic.Teste)))#taxa de acerto
##  0.8595041

library(caret)
confusionMatrix(svm.Teste, teste.svm$Regime )

# # Confusion Matrix and Statistics
# # 
# # Reference
# # Prediction   1   2   3
# # 1 301  28  17
# # 2  53 309  32
# # 3   5  18 326
# # 
# # Overall Statistics
# # 
# # Accuracy : 0.8595          
# # 95% CI : (0.8374, 0.8796)
# # No Information Rate : 0.3444          
# # P-Value [Acc > NIR] : < 2.2e-16       
# # 
# # Kappa : 0.7893          
# # 
# # Mcnemar's Test P-Value : 0.0004035       
# # 
# # Statistics by Class:
# # 
# # Class: 1 Class: 2 Class: 3
# # Sensitivity            0.8384   0.8704   0.8693
# # Specificity            0.9384   0.8842   0.9678
# # Pos Pred Value         0.8699   0.7843   0.9341
# # Neg Pred Value         0.9219   0.9338   0.9338
# # Prevalence             0.3297   0.3260   0.3444
# # Detection Rate         0.2764   0.2837   0.2994
# # Detection Prevalence   0.3177   0.3618   0.3205
# # Balanced Accuracy      0.8884   0.8773   0.9186



#Tune
svm.tune.result <- tune(svm, Regime~., data=treino.svm, kernel="radial",
                        ranges = list(gamma=seq(0.02, 0.09, 0.0025),
                                                cost=2^(-1:1)))

# # Parameter tuning of 'svm':
# #   
# #   - sampling method: 10-fold cross validation 
# # 
# # - best parameters:
# #   gamma cost
# # 0.035    2
# # 
# # - best performance: 0.1493309 

windows()
plot(svm.tune.result, main= "Desempenho SVM" )

#Desempenho
svm.Treino.tune=predict(svm.tune.result$best.model,treino.svm)
svm.predic.Treino.tune <- table(treino.svm$Regime, svm.Treino.tune)
svm.predic.Treino.tune
# # svm.Treino.tune
# #    1   2   3
# # 1 748 114  17
# # 2  81 725  55
# # 3  15  50 733

sum(diag(prop.table( svm.predic.Treino.tune)))#taxa de acerto
##0.8691883


svm.Teste.tune=predict((svm.tune.result$best.model), type = "class",teste.svm)
svm.predic.Teste.tune <- table(teste.svm$Regime, svm.Teste.tune)
svm.predic.Teste.tune
# # svm.Teste.tune
# # 1   2   3
# # 1 301  53   5
# # 2  28 309  18
# # 3  17  32 326

sum(diag(prop.table( svm.predic.Teste.tune)))#taxa de acerto
## 0.8604224