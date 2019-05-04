###Cargar paquetes utilizados###

library(MSGARCH)
library(rugarch)
library(qrmtools)
library(MASS)
library(GAS)

###Acceso a los datos utilizados (DATOS TESIS)###

bd=read.csv(file.choose(), header=T)
attach(bd)
summary(bd)
dlipc<-diff(log(DATOS))

--------------------------------------------------------------------------

###VaR Delta - Normal####

VaR(dlipc, p=.95, method="historical")

alpha=c(0.01, 0.05, 0.1) 
VaRN=quantile(dlipc, alpha)
VaRN

--------------------------------------------------------------------------

##########Value at Risk - ARMA(0,1)GARCH(1,1)##############

#Se especifica el modelo#
spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
		    mean.model=list(armaOrder=c(0,1)), distribution.model="std")
fit1=ugarchfit(data=dlipc,spec=spec1)
fit1

#Se hace el pronostico para T+1#
pred1= ugarchforecast(fit1,n.ahead=1,data=dlipc)

#Se obtienen los parametros para el calculo del VaR#
vol1=sigma(pred1)
res1=residuals(fit1, standardize=TRUE)
par.norm1=fitdistr(res1, "normal")$estimate
nu1=par.norm1["mean"]
sigma1=par.norm1["sd"]
qz1=qnorm(alpha, nu1, sigma1)
VaR1=qz1*vol1
VaR1

##Prueba de consistencia del VaR##

ctrl=list(tol=1e-7, delta=1e-9)
ctrl
groll1=ugarchroll(spec1, dlipc, n.ahead=1, na.start=6958 , forecast.length=251, refit.every=1,  
refit.window="moving", solver ="hybrid", Fit.control=lista(scale=1), 
solver.control=ctrl, calculate.VaR=TRUE, VaR.alpha=0.01)
report(groll1, type="VaR", VaR.alpha=0.01, conf.level=.99)
groll1


---------------------------------------------------------------------------

##########Value at Risk - ARMA(0,1)GARCH-M(1,1)##############

#Se especifica el modelo#
spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
		     mean.model=list(armaOrder=c(0,1),archm=TRUE), 
		     distribution.model="std")

fit2=ugarchfit(data=dlipc,spec=spec2)
fit2

#Se hace el pronostico para T+1#
pred2=ugarchforecast(fit2,n.ahead=1,data=dlipc)

#Se obtienen los parametros para el calculo del VaR#
vol2=sigma(pred2)
res2=residuals(fit2, standardize=TRUE)
res2
par.norm2=fitdistr(res2, "normal")$estimate
nu2=par.norm2["mean"]
nu2
sigma2=par.norm2["sd"]
sigma2
qz2=qnorm(alpha, nu2, sigma2)
VaR2=qz2*vol2
VaR2

##Prueba de consistencia del VaR##

groll2=ugarchroll(spec2, dlipc, n.ahead=1, na.start=6958 , forecast.length=251, refit.every=1,  
refit.window="moving", solver ="hybrid", Fit.control=lista(scale=1), 
solver.control=ctrl, calculate.VaR=TRUE, VaR.alpha=0.01)
report(groll2, type="VaR", VaR.alpha=0.01, conf.level=.99)

----------------------------------------------------------------------------

##########Value at Risk - ARMA(0,1)EGARCH(1,1)##############

#Se especifica el modelo#
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 1)), 
		     mean.model=list(armaOrder=c(0,1)), 
		     distribution.model="std")

fit3=ugarchfit(data=dlipc,spec=spec3)
fit3

#Se hace el pronostico para T+1#
pred3=ugarchforecast(fit3,n.ahead=1,data=dlipc)

#Se obtienen los parametros para el calculo del VaR#
vol3=sigma(pred3)
res3=residuals(fit3, standardize=TRUE)
res3
par.norm3=fitdistr(res3, "normal")$estimate

nu3=par.norm3["mean"]
nu3
sigma3=par.norm3["sd"]
sigma3
qz3=qnorm(alpha, nu3, sigma3)
VaR3=qz3*vol3
VaR3

##Prueba de consistencia del VaR##

groll3=ugarchroll(spec3, dlipc, n.ahead=1, na.start=6958 , forecast.length=251, refit.every=1,  
refit.window="moving", solver ="hybrid", Fit.control=lista(scale=1), 
solver.control=ctrl, calculate.VaR=TRUE, VaR.alpha=0.01)

report(groll3, type="VaR", VaR.alpha=0.01, conf.level=.99)

------------------------------------------------------------------------------

##########Value at Risk - ARMA(0,1)TGARCH(2,1)##############

#Se especifica el modelo#
spec4=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(2, 1)), 
		     mean.model=list(armaOrder=c(0,1)), 
		     distribution.model="std")

fit4=ugarchfit(data=dlipc,spec=spec4)
fit4

#Se hace el pronostico para T+1#
pred4=ugarchforecast(fit4,n.ahead=1,data=dlipc)

#Se obtienen los parametros para el calculo del VaR#
vol4=sigma(pred4)
res4=residuals(fit4, standardize=TRUE)
res4
par.norm4=fitdistr(res4, "normal")$estimate
nu4=par.norm4["mean"]
nu4
sigma4=par.norm4["sd"]
sigma4
qz4=qnorm(alpha, nu4, sigma4)
VaR4=qz4*vol4
VaR4

##Prueba de consistencia del VaR##

groll4=ugarchroll(spec4, dlipc, n.ahead=1, na.start=6958 , forecast.length=251, refit.every=1,  
refit.window="moving", solver ="hybrid", Fit.control=lista(scale=1), 
solver.control=ctrl, calculate.VaR=TRUE, VaR.alpha=0.01)
report(groll4, type="VaR", VaR.alpha=0.01, conf.level=.99)

------------------------------------------------------------------------------

##########Value at Risk - RS(2)GARCH(1,1)##############

#Se especifica el modelo#
spec5=CreateSpec(variance.spec = list(model = c("sGARCH")), 
	 distribution.spec = list(distribution = c("std")), 
	 switch.spec = list(do.mix = FALSE, K = 2))
fit5=FitML(spec = spec5, data = dlipc)
fit5

#Se hace el pronostico para T+1#

pred5=predict(object = fit5, nahead = 1L, do.return.draw = F)
pred5

#Se obtienen los parametros para el calculo del VaR#
vol5=UncVol(fit5)
res5=PIT(object = fit5, do.norm = TRUE, do.its = TRUE)
res5=as.numeric(res5)
par.norm5=fitdistr(res5, "normal")$estimate
nu5=par.norm5["mean"]
sigma5=par.norm5["sd"]
qz5=qnorm(alpha, nu5, sigma5)
VaR5=qz5*vol5
VaR5