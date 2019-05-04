rm(list=ls()) #Borrar variables y datos almacenados en la memoria
ls() 

library(forecast)
library(rugarch)
library(MSGARCH)

-----------------------------------------------------------------------------

###Acceder a datos (IPC diario)###

bd<-read.csv(file.choose(), header=T)
attach(bd)
summary(bd)

dlipc<-diff(log(DATOS)) #Rendimiento logaritmico del IPC

-----------------------------------------------------------------------------
  
###############################forecast#####################################

fit.mod=auto.arima(dlipc, max.p=12, max.q=12, max.P=12, 
                     max.Q=12, max.d=1, max.D=1, start.p=2, start.q=2, 
                     start.P=1, start.Q=1,stationary=FALSE , seasonal=TRUE, 
                     ic=c("bic"),stepwise=T, test=c("kpss"), 
                     seasonal.test=c("ocsb")) 

fit.mod

-----------------------------------------------------------------------------

###############################rugarch#####################################

##############################GARCH(p,q)###################################

###GARCH(1,1)###

spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
	                mean.model=list(armaOrder=c(0,1)), 
                  distribution.model="std")

fit1=ugarchfit(data=dlipc,spec=spec1)
fit1

###GARCH(1,2)###

spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 2)), 
	              mean.model=list(armaOrder=c(0,1)), 
	              distribution.model="std")

fit2=ugarchfit(data=dlipc,spec=spec2)
fit2

###GARCH(2,1)###

spec3=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 1)), 
	              mean.model=list(armaOrder=c(0,1)), 
	              distribution.model="std")

fit3=ugarchfit(data=dlipc,spec=spec3)
fit3

###GARCH(2,2)###

spec4=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 2)), 
	               mean.model=list(armaOrder=c(0,1)), 
                 distribution.model="std")

fit4=ugarchfit(data=dlipc,spec=spec4)
fit4

------------------------------------------------------------------------------

##################################GARCH-M(p,q)#################################

###GARCH-M(1,1)###

spec5=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
	              mean.model=list(armaOrder=c(0,1), archm=TRUE), 
	              distribution.model="std")

fit5=ugarchfit(data=dlipc,spec=spec5)
fit5

###GARCH-M(1,2)###

spec6=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 2)), 
	              mean.model=list(armaOrder=c(0,1),archm=TRUE), 
	              distribution.model="std")

fit6=ugarchfit(data=dlipc,spec=spec6)
fit6

###GARCH-M(2,1)###

spec7=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 1)), 
	              mean.model=list(armaOrder=c(0,1),archm=TRUE), 
	              distribution.model="std")

fit7=ugarchfit(data=dlipc,spec=spec7)
fit7

###GARCH-M(2,2)###

spec8=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(2, 2)), 
	              mean.model=list(armaOrder=c(0,1),archm=TRUE), 
	              distribution.model="std")

fit8=ugarchfit(data=dlipc,spec=spec8)
fit8

-------------------------------------------------------------------------------

##############################EGARCH(p,q)######################################

###EGARCH(1,1)###

spec9=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 1)),
	              mean.model=list(armaOrder=c(0,1)), 
	              distribution.model="std")

fit9=ugarchfit(data=dlipc,spec=spec9)
fit9

###EGARCH(1,2)###

spec10=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 2)),
	              mean.model=list(armaOrder=c(0,1)), 
	              distribution.model="std")

fit10=ugarchfit(data=dlipc,spec=spec10)
fit10

###EGARCH(2,1)###

spec11=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(2, 1)),
	              mean.model=list(armaOrder=c(0,1)), 
	              distribution.model="std")

fit11=ugarchfit(data=dlipc,spec=spec11)
fit11

###EGARCH(2,2)###

spec12=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(2, 2)),
	                mean.model=list(armaOrder=c(0,1)), 
                  distribution.model="std")

fit12=ugarchfit(data=dlipc,spec=spec12)
fit12

-------------------------------------------------------------------------------

###############################TGARCH(p,q)#####################################

###TGARCH(1,1)###

spec13=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1, 1)), 
	                mean.model=list(armaOrder=c(0,1)), 
                  distribution.model="std")

fit13=ugarchfit(data=dlipc,spec=spec13)
fit13

###TGARCH(2,1)###

spec14=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(2, 1)), 
	                mean.model=list(armaOrder=c(0,1)), 
                  distribution.model="std")

fit14=ugarchfit(data=dlipc,spec=spec14)
fit14

###TGARCH(1,2)###

spec15=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1, 2)), 
	                mean.model=list(armaOrder=c(0,1)), 
                  distribution.model="std")

fit15=ugarchfit(data=dlipc,spec=spec15)
fit15

###TGARCH(2,2)###

spec16=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(2, 2)), 
	                mean.model=list(armaOrder=c(0,1)), 
                  distribution.model="std")

fit16=ugarchfit(data=dlipc,spec=spec16)
fit16

-------------------------------------------------------------------------------

##############################RS(k)GARCH(1,1)##################################

###RS(2)GARCH(1,1)###

spec17=CreateSpec(variance.spec = list(model = c("sGARCH")), 
	      distribution.spec = list(distribution = c("std")), 
	      switch.spec = list(do.mix = FALSE, K = 2))

fit17=FitML(spec = spec17, data = dlipc)
fit17

###RS(3)GARCH(1,1)###

spec18=CreateSpec(variance.spec = list(model = c("sGARCH")), 
	      distribution.spec = list(distribution = c("std")), 
	      switch.spec = list(do.mix = FALSE, K = 3))

fit18=FitML(spec = spec18, data = dlipc)
fit18