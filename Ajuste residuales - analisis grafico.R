#######NORMALIDAD DE RESIDUALES - MODELOS SELECCIONADOS########

rm(list=ls()) #Borra variables y datos almacenados en la memoria
ls() 

###Paquetes utilizados###

library(rugarch)
library(MSGARCH)
library(ggplot2)
library(gridExtra)

###Cargar datos###

bd=read.csv(file.choose(), header=T)
attach(bd)
summary(bd)
dlipc<-diff(log(DATOS))

#####Modelo - GARCH(1,1)####

spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
		     mean.model=list(armaOrder=c(0,1)), distribution.model="std")

fitm1=ugarchfit(data=dlipc,spec=spec1)
fitm1
x1=residuals(fitm1, standardize = T)
resm1=data.frame(x1)

params=as.list(MASS::fitdistr(x1, "t")$estimate)
TP1=ggplot(resm1, aes(sample = x1)) +
  stat_qq(distribution = qt, dparams = params["df"]) +
  stat_qq_line(distribution = qt, dparams = params["df"])+
  ggtitle("QQ MA1-GARCH1,1")
TP1

custom1=function(x) {dt(x - mean(resm1$x1), 4)}
HT1=ggplot(data = resm1, aes(x1)) +
    geom_histogram(aes(y = ..density.., fill = ..count..)) +
    scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
    stat_function(fun = custom1, colour = "firebrick", lwd=1.3) +
    ggtitle("Histograma + curva t student") +
    theme_bw()
HT1

####GARCH-M(1,1)#####

spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
		     mean.model=list(armaOrder=c(0,1),archm=TRUE), 
		     distribution.model="std")

fitm2=ugarchfit(data=dlipc,spec=spec2)
fitm2

x2=residuals(fitm2, standardize = T)
resm2=data.frame(x2)

params2=as.list(MASS::fitdistr(x2, "t")$estimate)
TP2=ggplot(resm2, aes(sample = x2)) +
  stat_qq(distribution = qt, dparams = params2["df"]) +
  stat_qq_line(distribution = qt, dparams = params2["df"])+
  ggtitle("QQ MA1-GARCH-M1,1")
TP2

custom2=function(x) {dt(x - mean(resm2$x2), 5)}
HT2=ggplot(data = resm2, aes(x2)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = custom2, colour = "firebrick", lwd=1.3) +
  ggtitle("Histograma + curva t student") +
  theme_bw()
HT2

####EGARCH(1,1)####

spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 1)), 
		     mean.model=list(armaOrder=c(0,1)), 
		     distribution.model="std")

fitm3=ugarchfit(data=dlipc,spec=spec3)
fitm3
x3=residuals(fitm3, standardize = T)
resm3=data.frame(x3)

params3=as.list(MASS::fitdistr(x3, "t")$estimate)
TP3=ggplot(resm3, aes(sample = x3)) +
  stat_qq(distribution = qt, dparams = params3["df"]) +
  stat_qq_line(distribution = qt, dparams = params3["df"])+
  ggtitle("QQ MA1-EGARCH1,1")
TP3

custom3=function(x) {dt(x - mean(resm3$x3), 6)}
HT3=ggplot(data = resm3, aes(x3)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = custom3, colour = "firebrick", lwd=1.3) +
  ggtitle("Histograma + curva t student") +
  theme_bw()
HT3

#####TGARCH(2,1)####

spec4=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(2, 1)), 
		     mean.model=list(armaOrder=c(0,1)),
		     distribution.model="std")

fitm4=ugarchfit(data=dlipc,spec=spec4)
fitm4

x4=residuals(fitm4, standardize = T)
resm4=data.frame(x4)

params4=as.list(MASS::fitdistr(x4, "t")$estimate)
TP4=ggplot(resm4, aes(sample = x4)) +
  stat_qq(distribution = qt, dparams = params4["df"]) +
  stat_qq_line(distribution = qt, dparams = params4["df"])+
  ggtitle("QQ MA1-TGARCH2,1")
TP4

custom4=function(x) {dt(x - mean(resm4$x4), 4)}
HT4=ggplot(data = resm4, aes(x4)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = custom4, colour = "firebrick", lwd=1.3) +
  ggtitle("Histograma + curva t student") +
  theme_bw()
HT4

####RS(2)GARCH(1,1)#####

specrs=CreateSpec(variance.spec = list(model = c("sGARCH")), 
	 distribution.spec = list(distribution = c("std")), 
	 switch.spec = list(do.mix = FALSE, K = 2))
fitrs=FitML(spec = specrs, data = dlipc)
fitrs

x5=PIT(object = fitrs, do.norm = TRUE, do.its = TRUE)
resm5=data.frame(x5)

params5=as.list(MASS::fitdistr(x5, "t")$estimate)
TP5=ggplot(resm5, aes(sample = x5)) +
  stat_qq(distribution = qt, dparams = params5["df"]) +
  stat_qq_line(distribution = qt, dparams = params5["df"])+
  ggtitle("QQ RS2-GARCH11")
TP5

custom5=function(x) {dt(x - mean(resm5$x5), 8)}
HT5=ggplot(data = resm4, aes(x4)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = custom5, colour = "firebrick", lwd=1.3) +
  ggtitle("Histograma + curva t student") +
  theme_bw()
HT5

####Poner varias gráficas en una sola###
#Para poner varias graficas en una sola, si se trata de graficas genericas
#de R, suele utilizarse el comando "par(mfrow=c(columnas,filas))". Para el
#caso de graficos generados con "ggplot2" no funciona dicho comando, y para
#generar graficos conjuntos puede utilizarse el paquete "gridExtra". Aquí
#aquí se muestra un ejemplo de su uso.

grid.arrange(TP1, TP2, TP3, TP4, TP5, ncol=5)
