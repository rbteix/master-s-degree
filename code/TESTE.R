

#CÓDIGO CRIAR CPD COMO EXEMPLO DISSERTAÇÃO
y_ts_CP <- ts(c(rnorm(250,mean=1,sd=.5), rnorm(250,mean=3,sd=1), rnorm(250,mean=2 ,sd=1))) # rand signal w\ changepoint


options(warn=-1)
library(changepoint)

cptfn <- function(data, pen) {
  ans <- cpt.mean(data, test.stat="Normal", method = "PELT", penalty = "Manual", pen.value = pen) 
  length(cpts(ans)) +1
}

# evaluate and plot results:
windows()
plot.new()
frame()

# run cptfn for the signal with a known change point
pen.vals <- seq(0, 12,.2)
elbowplotData <- unlist(lapply(pen.vals, function(p) 
  cptfn(data = y_ts_CP, pen = p)))
plot.ts(y_ts_CP,type='l',col='red',
        xlab = "time",
        ylab = " Y(t)",
        main = "Change in mean signal")
plot(pen.vals,elbowplotData,
     xlab = "PELT penalty parameter",
     ylab = " ",
     main = " ")


penalty.val <- 8# this value is determined from elbow plots


cptm_CP         <- cpt.mean(y_ts_CP, penalty='Manual',pen.value=penalty.val,method='PELT') 
cpts_CP         <- cpts(cptm_CP) # change point time points
cpts_CP


windows()
#plot.new()
frame()
plot(cptm_CP, ylab="Y", xlab="Tempo", col="gray", panel.first = grid(col = "gray"))


####################################ROC CURVE#################################
library(pROC)

cart.pred.Teste=predict(poda_fit, teste, type = "vector", ordered=T)
windows()
cart<-(multiclass.roc(teste$Regime, cart.pred.Teste, plot=F, col=4, main="", xlab="Especificidade", ylab="Sensibilidade"))
# # Data: multivariate predictor cart.pred.Teste with 3 levels of teste$Regime: 1, 2, 3.
# # Multi-class area under the curve:0.917

# Draw a legend.
legend(0.0, 0.2, c('CART', 'RF','SVM'), 4:6)



# add=TRUE draws on the existing chart 
rf.pred.test <- predict(model.rf.tree, teste.rf, type = 'response', ordered=T)
windows()
rf <-multiclass.roc(teste.rf$Regime~as.numeric(rf.pred.test), col=5, plot=F)
# # Data: multivariate predictor rf.pred.test with 3 levels of teste.rf$Regime: 1, 2, 3.
# # Multi-class area under the curve: 0.9421


svm.pred.test <-predict(svm.tune.result$best.model, teste.svm, ordered=T )
windows()
svm <-multiclass.roc(teste.svm$Regime~as.numeric(svm.Teste) , plot=F, col=6)
# # Data: as.numeric(svm.Teste) with 3 levels of teste.svm$Regime: 1, 2, 3.
# # Multi-class area under the curve:  0.9268





#########################################################################

#Plot curvas juntas
#cart.pred=predict(fit, teste, type = "prob")

# 
cart.rs <- cart[["rocs"]]

x11(width=20)
par(mfrow=c(1,3))
plot.roc(cart.rs[[1]],  main="", col=5, xlab = "Especificidade", ylab="Sensibilidade")
legend(0.2, 0.2, c('CART', 'RF'), 5:6)

# sapply(2:length(cart.rs),function(i) lines.roc(cart.rs[[i]], col=i))
# # Draw a legend.
# legend(0.2, 0.2, c('R1', 'R2','R3'), 1:3)


 rf.rs <- rf[["rocs"]]

plot.roc(rf.rs[[3]],add=T,  col=6)
# sapply(2:length(rf.rs),function(i) lines.roc(rf.rs[[i]],col=i))
# legend(0.2, 0.2, c('R1', 'R2','R3'), 1:3)

svm.rs <- svm[["rocs"]]

plot.roc(svm.rs[[1]], main="", col=6, add=T)
sapply(2:length(svm.rs),function(i) lines.roc(svm.rs[[i]],col=i ))#plot três classes juntas




####################################################
#Replicando conjuntos
##############################################
train_svm <- function( dummies.balanced, svm.tune.result){
  sample.svm <- sample.int(n = nrow(dummies.balanced), size = floor(.7*nrow(dummies.balanced)), replace=F)
  treino.svm <- dummies.balanced[sample.svm,]
  teste.svm <- dummies.balanced[-sample.svm,]  
  svm.Teste.tune=predict((svm.tune.result$best.model), type = "class",teste.svm)
  svm.predic.Teste.tune <- table(teste.svm$Regime, svm.Teste.tune)
  accuracy_value <-sum(diag(prop.table(  svm.predic.Teste.tune)))#taxa de acerto
  
  
  return(accuracy_value)
  
}


results_func <- replicate(100, train(data.gru.balanced, poda_fit))
windows()
boxplot(results_func, main="CART")





#####################################################
#Figuras juntas para a artigo
###################################################
x11(width=20)
par(mfrow=c(1,3))
plot.roc(cart.rs[[1]],  main="", col=5, xlab = "Especificidade", ylab="Sensibilidade", cex.lab=1.7)
plot.roc(rf.rs[[1]],add=T,  col=6 )
plot.roc(svm.rs[[1]], add=T, col=7)
legend(0.2, 0.2, c('CART', 'RF', 'SVM'), 5:7, cex=1.5)
plot.roc(cart.rs[[2]],  main="", col=5, xlab = "Especificidade", ylab="Sensibilidade", cex.lab=1.7)
plot.roc(rf.rs[[2]],add=T,  col=6)
plot.roc(svm.rs[[2]], add=T, col=7)
legend(0.2, 0.2, c('CART', 'RF', 'SVM'), 5:7, cex=1.5)
plot.roc(cart.rs[[3]],  main="", col=5, xlab = "Especificidade", ylab="Sensibilidade", cex.lab=1.7)
plot.roc(rf.rs[[3]],add=T,  col=6)
plot.roc(svm.rs[[3]], add=T, col=7)
legend(0.2, 0.2, c('CART', 'RF', 'SVM'), 5:7, cex=1.5)

# sapply(2:length(cart.rs),function(i) lines.roc(cart.rs[[i]], col=i))
# # Draw a legend.
# legend(0.2, 0.2, c('R1', 'R2','R3'), 1:3)
myPaths <- .libPaths()   # get the paths
myPaths <- c(myPaths[2], myPaths[1])  # switch them
.libPaths(myPaths)  # reassign them
