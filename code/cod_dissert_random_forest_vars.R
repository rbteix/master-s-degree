
library(randomForest)

#Cross Validation
set.seed(050)

#criando bases de treino e teste
library(caTools)
split.rf <- sample.split(data.gru.balanced$Regime, SplitRatio = 0.7)

treino.rf <- subset(data.gru.balanced, split.rf==TRUE)
table(treino.rf$Regime)
# #   1   2   3 
# # 864 851 843 


teste.rf<- subset(data.gru.balanced, split.rf==F)
table(teste.rf$Regime)
# #   1   2   3 
# # 371 365 361 



#Plot tamanho das árvores
windows()
plot(model.rf,  main="Modelo", col=c("blue", "black", "purple", "green"))
legend("topright", colnames(model.rf$err.rate), col=c("blue", "black", "purple", "green"), cex=0.8, 
       fill=c("blue", "black", "purple", "green"))

##########################MODELO 2MTRY 400 E 500 ÁRVORES######################
#Mineração de dados
#Random Forest modelo

model.rf <- randomForest(Regime~., data=treino.rf, importance=T)
# # OOB estimate of  error rate: 11.34%


model.rf.tree <- randomForest(Regime~., data=treino.rf, importance=T, ntree=400)

# # Call:
# #   randomForest(formula = Regime ~ ., data = treino.rf, importance = T,      ntree = 400) 
# # Type of random forest: classification
# # Number of trees: 400
# # No. of variables tried at each split: 2
# # 
# # OOB estimate of  error rate: 10.87%
# # Confusion matrix:
# #     1   2   3 class.error
# # 1 773  83   8  0.10532407
# # 2  75 731  45  0.14101058
# # 3  21  46 776  0.07947805


################################################################
#Importância das variáveis (modelo com 1 mtry e 400 árvores)
##################################################################
var.imp <-model.rf.tree$importance
var.imp <- data.frame(imp=var.imp[c(1:7),4])
x11(width=10)
plot.var.rf <- var.imp %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(plot.var.rf) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  labs(y= "Importância", x="Variáveis")+
  ggtitle("Importância Variáveis RF")+
  coord_flip() +
  scale_fill_grey() +
  theme_bw()



#########################################################
#Avaliação dos Resultados
##############################################################
# pred.rf.tree<- predict(model.rf.tree, treino.rf, type="class" )
# table(pred.rf.tree, treino.rf$Regime)
# ## pred.rf.tree   1   2   3
#             # 1 864   0   0
#             # 2   0 851   0
#             # 3   0   0 843
# mean(pred.rf.tree == treino.rf$Regime)
# ##1
pred.test <- predict(model.rf.tree, teste.rf, type = "class")
# Checking classification accuracy
table(pred.test, teste.rf$Regime)
# # pred.test   1   2   3
# #         1 329  26   6
# #         2  40 318  18
# #         3   2  21 337 

mean(pred.test == teste.rf$Regime)
#  0.8969918

confusionMatrix(pred.test, teste.rf$Regime)

# # Confusion Matrix and Statistics
# # 
# # Reference
# # Prediction   1   2   3
# #          1 329  26   6
# #          2  40 318  18
# #          3   2  21 337
# # 
# # Overall Statistics
# # 
# # Accuracy : 0.897           
# # 95% CI : (0.8775, 0.9144)
# # No Information Rate : 0.3382          
# # P-Value [Acc > NIR] : <2e-16          
# # 
# # Kappa : 0.8455          
# # 
# # Mcnemar's Test P-Value : 0.1577          
# # 
# # Statistics by Class:
# # 
# #                      Class: 1 Class: 2 Class: 3
# # Sensitivity            0.8868   0.8712   0.9335
# # Specificity            0.9559   0.9208   0.9688
# # Pos Pred Value         0.9114   0.8457   0.9361
# # Neg Pred Value         0.9429   0.9348   0.9674
# # Prevalence             0.3382   0.3327   0.3291
# # Detection Rate         0.2999   0.2899   0.3072
# # Detection Prevalence   0.3291   0.3428   0.3282
# # Balanced Accuracy      0.9214   0.8960   0.9511












#######################################TUNING PARAMETERS####################
############ Mtry 1
model.rf2 <- randomForest(Regime~., data=treino.rf, ntree= 500, mtry=1, importance=T)

##OOB estimate of  error rate: 11.42%

#modelo de como observar um tipo de medida da importância da variável.
#print(importance(model.rf2))# com type = 2 somente Gini será mostrado.
set.seed(050)
model.rf2.tree <- randomForest(Regime~., data=treino.rf, ntree= 400, mtry=1, importance=T)
# # Call:
# #   randomForest(formula = Regime ~ ., data = treino.rf, ntree = 400,      mtry = 1, importance = T) 
# # Type of random forest: classification
# # Number of trees: 400
# # No. of variables tried at each split: 1
# # 
# # OOB estimate of  error rate: 11.02%
# # Confusion matrix:
# #     1   2   3 class.error
# # 1 769  86   9  0.10995370
# # 2  78 730  43  0.14218566
# # 3  26  40 777  0.07829181



model.rf3 <- randomForest(Regime~., data=treino.rf, ntree= 500, mtry=3, importance=T)
##OOB estimate of  error rate: 11.1% , mtry=3
model.rf3.tree <- randomForest(Regime~., data=treino.rf, ntree= 400, mtry=3, importance=T)
##OOB estimate of  error rate: 11.18%

############################################
#Preditcion model 2 treino mtry=1 , tree=400
##############################################
pred.rf2<- predict(model.rf2.tree, treino.rf, type="class" )
#checking classification accuracy
table(pred.rf2, treino.rf$Regime)
## pred.rf2   1   2   3
##        1 810  29   5
##        2  54 797  23
##        3   0  25 815
mean(pred.rf2 == treino.rf$Regime)
##0.9468335

#Preditcion model 2 validação
pred.test2 <- predict(model.rf2.tree, teste.rf, type = "class")
# Checking classification accuracy

table(pred.test2, teste.rf$Regime)
# # pred.test3   1   2   3
# #          1 329  25   6
# #          2  39 320  17
# #          3   3  20 338

mean(pred.test2 == teste.rf$Regime)
## 0.8997265
confusionMatrix(pred.test2, teste.rf$Regime)

######## MTRY 3 #############################################

pred.rf3<- predict(model.rf3, treino.rf, type="class" )
#checking classification accuracy
table(pred.rf3, treino.rf$Regime)


pred.test3 <- predict(model.rf3, teste.rf, type = "class")
# Checking classification accuracy
table(pred.test3, teste.rf$Regime)
mean(pred.test3 == teste.rf$Regime)
## 0.8951686




#PLOTANTO  ERROS OOB E TEST (não foi possível definir o mtry)
set.seed(050)
windows()
t.rf <- tuneRF(treino.rf[,-3], treino.rf[,3],
               plot=T,
               #ntreeTry = 400,
               trace=T)


t.rf

# oob.err=double(4)
# test.err=double(4)
# 
# for(mtry in 1:4)
#   
# {
#   rf=randomForest(Regime~., data=treino.rf, mtry=mtry, ntree=400)
#   oob.err[mtry]= rf$err.rate[400]
#   
#   pred <- predict(rf, teste.rf)
#   test.err[mtry] = 1-(with(teste.rf,(mean(pred == teste.rf$Regime))))
#   
#   cat(mtry, "")
# }
# 
# windows()
# matplot(1:mtry , cbind(oob.err, test.err), pch=20 , col=c("darkgray","red"),type="b",ylab="MSE",xlab="Número de Preditores Considerados em cada Divisão")
# legend("bottomright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("darkgray", "red"))

# Now what we observe is that the Red line is the Out of Bag Error Estimates
# and the Blue Line is the Error calculated on Test Set. Both curves are quite 
# smooth and the error estimates are somewhat correlated too. The Error Tends 
# to be minimized at around mtry = 1 e 2.