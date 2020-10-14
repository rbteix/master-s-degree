#Após balanceamento dos dados na classificação
View(data.gru.balanced)

#Regime referência=1
data.gru.balanced$Regime_t <- relevel(data.gru.balanced$Regime_t, ref=1)

#MINERAÇÃO DOS DADOS
library(nnet)

#Criar modelo
model.multin <- multinom(Regime_t~ ., data=treino)
# # weights:  66 (42 variable)
# # initial  value 3083.804694 
# # iter  10 value 1408.906306
# # iter  20 value 1324.884095
# # iter  30 value 1318.971265
# # iter  40 value 1317.982471
# # iter  50 value 1317.650890
# # iter  60 value 1317.370597
# # final  value 1317.369622 
# # converged

summary(model.multin)
step(model.multin, trace=1, direction = c("backward"))
step(model.multin, trace=1, direction = c("both"))


model.multin = multinom(formula = Regime_t ~ Mês + Regime_ant + Movimentos_programandos, data=treino ) 
summary(model.multin)
# # weights:  48 (30 variable)
# # initial  value 3083.804694 
# # iter  10 value 1414.049610
# # iter  20 value 1352.625357
# # iter  30 value 1348.649417
# # iter  40 value 1348.000650
# # iter  50 value 1347.905241
# # final  value 1347.905131 
# # converged


# Remember, to interpret logistic regression results, we want to first transform the coefficients to odds ratios by raising e - or Euler's constant 
# - to the coefficients. Luckily, that's pretty easy in R.

probabilidades.Treino=predict(model.multin,treino)
predic.classes.Treino <- table(treino$Regime_t, probabilidades.Treino)
predic.classes.Treino
# # probabilidades.Treino
# # 1   2   3
# # 1 796  70  41
# # 2 172 681  34
# # 3 133  53 827


sum(diag(prop.table( predic.classes.Treino)))#taxa de acerto
##  0.8208051

#Dados teste
probabilidades.Teste=predict(model.multin, type = "class",teste)
predic.classes.Teste <- table(teste$Regime_t, probabilidades.Teste)
predic.classes.Teste
probabilidades.Teste
# # 1   2   3
# # 1 364  34  15
# # 2  83 291  19
# # 3  27  21 349

sum(diag(prop.table( predic.classes.Teste)))#taxa de acerto
##0.8345802






#Variável contendo os coeficientes 
coefs <- coef(model.multin)

#Aumentar "e" para os  coeficientes transformando-os em índice de probabilidades (chances)
exp(coefs)
exp((coefs)-1)*100 #porcentagem de chance

#Valor z e p para encontrar significância estatística
z<- summary(model.multin)$coefficients/summary(model.multin)$standard.errors
p <- (1- pnoprm(abs(z),0,1))*2








