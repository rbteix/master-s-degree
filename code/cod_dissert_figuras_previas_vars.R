setwd("C://Users//bella//Documents//Mestrado//Projeto")
data.gru <- read.csv("dados_gr.csv", sep = ";")
vars <- read.csv("vars.csv", header=T, sep=",", dec=",")

data.gru$Data<- NULL
data.gru$Movimenta��es <- NULL
head(data.gru)
tail(data.gru)
data.gru <- data.gru[1:2496,]

vars <- vars[-1248,]
data.gru <- cbind(data.gru, vars[,-1])

#Tratando missing values
library(mice)
md.pattern(data.gru)  # pattern or missing values in data.

library(VIM)
missingvalue <- data.gru[,-1]
names(missingvalue)[3] <- "Atrasado"  #renomeando coluna

missingvalue= missingvalue[,c(2,3,1,4, 5,6,7,8)]
names(missingvalue)[4] <- "Mov.progr"  #renomeando coluna
names(missingvalue)[6] <- "M.Spac"  #renomeando coluna
names(missingvalue)[7] <- "DP.Spac"  #renomeando coluna
names(missingvalue)[8] <- "M.Con"  #renomeando coluna



X11(width=10) #plot
matrixplot(missingvalue) #Sorted by mov.programados

is.na(data.gru[,2]) #observa��o faltante: 1037 de cancelado
data.gru1 <- na.omit(data.gru)#Passo 2: Omitir dados n�o num�ricos (s� h� 1) e fazer teste
na.fail(data.gru1) # verificando se todos os valores faltantes foram omitidos 


#Alterando o formato da coluna data
data.gru1$Data.1  <- as.Date(data.gru1$Data.1, format = "%d/%m/%Y")
names(data.gru1)[1] <-"Data" #renomeando coluna


#Renomeando coluna Total.geral
colnames(data.gru1)[colnames(data.gru1)=="Total.geral"] <- "Movimentos.Programados"

data.gru3 <- data.gru1 #base de dados para trabalhar com todas as vari�veis
###################################################################
#Ordenando colunas -- VARI�VEL BASE
data.gru1 = data.gru1[, c(1,3,4,2,5)] 
######################################################################

#Porcentagem de falhas nos v�os (cancelados + realizados com atraso) / total geral
data.gru2 <- data.gru1
data.fails<- data.frame(data.gru2$Realizado.com.atraso + data.gru2$Cancelado) / data.gru2$Movimentos.Programados
colnames(data.fails)= c("Fatia") #nomeando coluna

#add coluna data
Data <- data.gru2[,1] #vari�vel data
data.fails["Data"]<-c(Data)#adcionando coluna

data.fails = data.fails[,c(2,1)] #reordenando
data.fails.vars <- cbind(data.fails, data.gru3[,c(6,7,8,9)])



#vari�vel para ggplot boxplot -OUTLIERS
plo.box.gg.vars <- data.frame(data.fails.vars[,-1])

out<-ggplot(plo.box.gg.vars, aes("", Fatia))+
  geom_boxplot(fill="gray")+
  labs(x= "")+
  theme_bw()

windows()#plot
out

#identificando outliers no boxplot e vetor 
outliers <- boxplot(data.fails.vars$Fatia, plot=F)$out #vari�vel contento outliers, 85 outliers
data.fails.vars[which(data.fails.vars$Fatia %in% outliers),] #finding outliers
data.fails.vars <- data.fails.vars[-which(data.fails.vars$Fatia %in% outliers),]#removing outliers
nout.box.gg.vars <- data.frame(data.fails.vars[,-1])#vari�vel para plotar, sem coluna data

nout<-ggplot(nout.box.gg.vars, aes("", Fatia))+
  geom_boxplot(fill="gray")+
  labs(x= "")+
  theme_bw()

#Outliers das vari�veis explicativas
mean.HHI <-mean(data.fails.vars$HHI)
outliers.HHI <- boxplot(data.fails.vars$HHI, plot=F)$out #vari�vel contento outliers, 85 outliers
data.fails.vars[which(data.fails.vars$HHI %in% outliers.HHI),] #finding outliers
outliers.HHI <- data.frame(outliers.HHI )
data.fails.vars[which(data.fails.vars$HHI%in% outliers.HHI[c(127,128,168),]),"HHI" ] =mean.HHI#finding outliers

mean.spacing<- mean(data.fails.vars$M�dia.Spacing)
outliers.spacing <- boxplot(data.fails.vars$M�dia.Spacing, plot=F)$out #vari�vel contento outliers, 85 outliers
data.fails.vars[which(data.fails.vars$M�dia.Spacing%in% outliers.spacing),"M�dia.Spacing" ] =mean.spacing#finding outliers

mean.sd.spacing <- mean(data.fails.vars$DesvPad.Spacing)
outliers.spacing.sd <- boxplot(data.fails.vars$DesvPad.Spacing, plot=F)$out #vari�vel contento outliers, 85 outliers
outliers.spacing.sd <- data.frame(outliers.spacing.sd )
data.fails.vars[which(data.fails.vars$DesvPad.Spacing %in% outliers.spacing.sd),] #finding outliers
data.fails.vars[which(data.fails.vars$DesvPad.Spacing%in% outliers.spacing.sd[c(11:14),]),"DesvPad.Spacing" ] =mean.sd.spacing#finding outliers

mean.conmov <- mean(data.fails.vars$M�dia.COnMov)
outliers.conmov <- boxplot(data.fails.vars$M�dia.COnMov, plot=F)$out #vari�vel contento outliers, 85 outliers
data.fails.vars[which(data.fails.vars$M�dia.COnMov%in% outliers.conmov),"M�dia.COnMov" ] =mean.conmov#finding outliers



















#Plot dos dois gr�ficos em uma mesma figura
library(ggpubr)
X11(width = 10)
ggarrange(out, nout, labels = c("A", "B"))

#VERIFIGA��O GR�FICA PARA OBSERVAFR SE O COMPORTAMENTO DA BASE � O MESMO

#HISTOGRAM DATA.FAILS
mean.fatia <- mean(data.fails.vars$Fatia)
median.fatia <- median(data.fails.vars$Fatia)
windows()
ggplot(data.fails.vars,aes(data.fails.vars$Fatia))+
  geom_histogram(  col="lightgray", fill="gray", binwidth = 0.02)+
  geom_vline(data=data.fails.vars, aes(xintercept=mean.fatia, color="m�dia"),
             linetype="dashed")+
   geom_vline(data=data.fails.vars, aes(xintercept=median.fatia, color="mediana"),
              linetype="dashed")+
  geom_vline(data=data.fails.vars, aes(xintercept=0.08, color="outliers" ),
             linetype="dashed")+
  geom_vline(data=data.fails.vars, aes(xintercept=0.4, color="outliers" ),
             linetype="dashed")+
  scale_color_manual(name = "", values = c(mediana = "green", m�dia = "blue", outliers = "red"))+ 
 labs(x="Fatia", y="Frequ�ncia")+
  theme_minimal()



#Plot s�rie temporal
windows()
par(mfrow=c(2,1))
plot(data.fails.vars$Fatia, type = "l", ylab="Fatia", xlab="Data", col="gray", panel.first = grid(col = "gray"), main="S�rie Temporal Fatia de Voos")


data.plot <- data.fails.vars[,c(1,2)]
windows()
par(mfrow=c(2,1))
plot(data.plot$Data, data.plot$Fatia, type = "l",xlab='Ano', ylab='Fatia', col="gray", panel.first = grid(col = "gray"),  main="S�rie Temporal Fatia de Voos") 



#VARI�VEIS EXPLICATIVAS
#Renomeando colunas
colnames(data.fails.vars)[4] <- "M�dia.Spacing"
colnames(data.fails.vars)[5] <- "DesvPad.Spacing"
colnames(data.fails.vars)[6] <- "M�dia.COnMov"



#Plot vari�veis explicativas
X11(width = 8)
plot(data.fails.vars$Data, data.fails.vars$HHI, type = "l", ylab="HHI", xlab="Ano", col="gray", panel.first = grid(col = "gray"), main="")
mtext("A", side = 3, adj=0) #adicionando identifica��o 

X11(width = 8)
plot(data.fails.vars$Data, data.fails.vars$M�dia.Spacing*100, yaxt="n", type = "l", ylab="", xlab="Ano", col="gray", panel.first = grid(col = "gray"), main="")
axis(2, at=pretty(data.fails.vars$M�dia.de.Spacing), lab=pretty(data.fails.vars$M�dia.de.Spacing), las=TRUE)
mtext("B", side = 3, adj=0)


X11(width = 8)
plot(data.fails.vars$Data, data.fails.vars$DesvPad.Spacing, yaxt="n", type = "l", ylab="DesvPad.Spacing", xlab="Ano", col="gray", panel.first = grid(col = "gray"), main="")
axis(2, at=pretty(data.fails.vars$M�dia.de.Spacing), lab=pretty(data.fails.vars$M�dia.de.Spacing), las=TRUE)
mtext("C", side = 3, adj=0)

X11(width = 8)
plot(data.fails.vars$Data, data.fails.vars$M�dia.COnMov,  type = "l", ylab="M�dia.ConMov", xlab="Ano", col="gray", panel.first = grid(col = "gray"), main="")
mtext("D", side = 3, adj=0)



#BASE ID�NTICA

