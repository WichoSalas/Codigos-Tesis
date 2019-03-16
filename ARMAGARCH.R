rm(list=ls()) ##Borrar variables y datos almacenados en la memoria##
ls() 
library(rugarch) ##requiere de intalacion previa##
library(fGarch)
bd<-read.csv(file.choose(), header=T)
attach(bd)
summary(bd)
dlipc<-diff(log(DATOS))

##Calibración de modelos GARCH##
spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), mean.model=list(armaOrder=c(1,1)))
fit1=ugarchfit(data=dlipc,spec=spec1)
show(fit1)
x1=residuals(fit1, standardize = T)
resx1=data.frame(x1)
jarque.bera.test(x1)
normplotx1 <- ggplot(resx1, aes(sample = x1))
NP1=normplotx1 + stat_qq() + stat_qq_line() + ggtitle("Grafica QQ - residuos GARCH(1,2)")
NP1
histmod1= ggplot(resg12, aes(x2)) + geom_histogram(bins=40, colour="black", fill="darkslategray4")
HP1= histmod1 + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma residuales estd. GARCH(1,2)")
HP1





spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 2)), mean.model=list(armaOrder=c(1,2)))
fit2=ugarchfit(data=dlipc,spec=spec2)
fit2
x2=residuals(fit2, standardize = T)
resg12=data.frame(x2)
jarque.bera.test(x2)

spec3=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 1)), mean.model=list(armaOrder=c(0,0)))
fit3=ugarchfit(data=dlipc,spec=spec3)
fit3
x3=residuals(fit3, standardize = T)
resg3=data.frame(x3)
jarque.bera.test(x3)


spec4=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 2)), mean.model=list(armaOrder=c(0,0)))
fit4=ugarchfit(data=dlipc,spec=spec4)
show(fit4)
x4=residuals(fit4, standardize = T)
resg4=data.frame(x4)
jarque.bera.test(x4)




v2=ts(sigma(fit2), freq= 251, end=c(2018, 251))
ts.plot(v2)

##Calibración de modelos GARCH-M##

spec5=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), mean.model=list(armaOrder=c(1,1),archm=TRUE))
fit5=ugarchfit(data=dlipc,spec=spec5)
fit5
x5=residuals(fit5, standardize = T)
resx5=data.frame(x5)
jarque.bera.test(x5)
plot.ts(x5*x5)
spec6=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 2)), mean.model=list(armaOrder=c(0,0),archm=TRUE))
fit6=ugarchfit(data=dlipc,spec=spec6)
fit6

spec7=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 1)), mean.model=list(armaOrder=c(0,0),archm=TRUE))
fit7=ugarchfit(data=dlipc,spec=spec7)
fit7

spec8=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 2)), mean.model=list(armaOrder=c(0,0),archm=TRUE))
fit8=ugarchfit(data=dlipc,spec=spec8)
fit8

##Calibración de modelos EGARCH##

spec9=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 1)),mean.model=list(armaOrder=c(1,1)))
fit9=ugarchfit(data=dlipc,spec=spec9)
fit9
x9=residuals(fit9, standardize = T)
resx9=data.frame(x9)
jarque.bera.test(x9)

spec10=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 2)),mean.model=list(armaOrder=c(0,0)))
fit10=ugarchfit(data=dlipc,spec=spec10)
fit10

spec11=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(2, 1)),mean.model=list(armaOrder=c(0,0)))
fit11=ugarchfit(data=dlipc,spec=spec11)
fit11

spec12=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(2, 2)),mean.model=list(armaOrder=c(0,0)))
fit12=ugarchfit(data=dlipc,spec=spec12)
fit12

##Calibración de modelos TGARCH o GJR##
spec13=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1, 1)), mean.model=list(armaOrder=c(0,0)))
fit13=ugarchfit(data=dlipc,spec=spec13)
fit13
x13=residuals(fit13, standardize= T)
resx13=data.frame(x13)
jarque.bera.test(x13)

spec14=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(2, 1)), mean.model=list(armaOrder=c(0,0)))
fit14=ugarchfit(data=dlipc,spec=spec14)
fit14

spec15=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1, 2)), mean.model=list(armaOrder=c(0,0)))
fit15=ugarchfit(data=dlipc,spec=spec15)
fit15

spec16=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(2, 2)), mean.model=list(armaOrder=c(0,0)))
fit16=ugarchfit(data=dlipc,spec=spec16)
fit16

install.packages("MSGARCH")
library(MSGARCH)

specrs <- CreateSpec(variance.spec = list(model = c("sGARCH")), distribution.spec = list(distribution = c("std")), switch.spec = list(do.mix = FALSE, K = 2))
print(specrs)
fitrs= FitML(spec = specrs, data = dlipc)
fitrs
SR.fit <- ExtractStateFit(fitrs)
print(SR.fit)
xrs=residuals(fitrs, standardize = T)
resxrs=data.frame(xrs)
resxrs



specrs2 <- CreateSpec(variance.spec = list(model = c("sGARCH")), distribution.spec = list(distribution = c("std")), switch.spec = list(do.mix = FALSE, K = 3))
print(specrs2)
fitrs2= FitML(spec = specrs2, data = dlipc)
fitrs2

specrs4 <- CreateSpec(variance.spec = list(model = c("sGARCH")), distribution.spec = list(distribution = c("norm")), switch.spec = list(do.mix = FALSE, K = 5))
fitrs4= FitML(spec = specrs4, data = dlipc)
fitrs3
alpha01=fitrs3$Estimate
alpha01






library(qpcR)
install.packages("qpcR")
rsresid=fitrs$residuals
HQIC(rsresid)
dats=data.frame(WN)
WN=ts(rnorm(10000,0,1))
PLOTY1=autoplot(WN, ylab="Valores", xlab="Tiempo", colour='darkslategray4', main = "Simulación de Ruido Blanco con media cero y varianza 1.")
gwn= ggplot(dats, aes(WN)) + geom_histogram(bins=15, colour="black", fill="darkslategray4")
PLOTY2=gwn + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma Ruido Blanco")
library(gridExtra)
grid.arrange(PLOTY1, PLOTY2, ncol=2)

normplot1 <- ggplot(resg12, aes(sample = x2))
NP1=normplot1 + stat_qq() + stat_qq_line() + ggtitle("Grafica QQ - residuos GARCH(1,2)")
NP1
histmod1= ggplot(resg12, aes(x2)) + geom_histogram(bins=40, colour="black", fill="darkslategray4")
HP1= histmod1 + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma residuales estd. GARCH(1,2)")
HP1

normplot2 <- ggplot(resx5, aes(sample = x5))
NP2=normplot2 + stat_qq() + stat_qq_line() + ggtitle("Grafica QQ - residuos GARCH-M(1,1)")
NP2
histmod2= ggplot(resx5, aes(x5)) + geom_histogram(bins=40, colour="black", fill="darkslategray4")
HP2= histmod2 + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma residuales estd. GARCH-M(1,1)")
HP2

normplot3 <- ggplot(resx9, aes(sample = x9))
NP3=normplot3 + stat_qq() + stat_qq_line() + ggtitle("Grafica QQ - residuos EGARCH(1,1)")
NP3
histmod3= ggplot(resx9, aes(x9)) + geom_histogram(bins=40, colour="black", fill="darkslategray4")
HP3= histmod3 + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma residuales estd. EGARCH(1,1)")
HP3

normplot4 <- ggplot(resx13, aes(sample = x13))
NP4=normplot4 + stat_qq() + stat_qq_line() + ggtitle("Grafica QQ - residuos TGARCH(1,1)")
NP4
histmod4= ggplot(resx13, aes(x13)) + geom_histogram(bins=40, colour="black", fill="darkslategray4")
HP4= histmod4 + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma residuales estd. TGARCH(1,1)")
HP4
xrs
normplot5 <- ggplot(resxrs, aes(sample = xrs))
NP5=normplot5 + stat_qq() + stat_qq_line() + ggtitle("Grafica QQ - residuos RS(2)GARCH(1,1)")
NP5
histmod5= ggplot(resxrs, aes(xrs)) + geom_histogram(bins=40, colour="black", fill="darkslategray4")
HP5= histmod5 + xlab("Rangos") + ylab ("Conteo") + ggtitle("Histograma residuales estd. RS(S)GARCH(1,1)")
HP5


library(gridExtra)
grid.arrange(NP1, HP1, ncol=5)

