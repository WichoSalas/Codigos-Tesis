#######Simulación - modelos seleccionados##########

rm(list=ls()) #Borra variables y datos almacenados en la memoria
ls() 

library(rugarch)
library(MSGARCH)
library(ggplot2)

-----------------------------------------------------------------------------

###Acceder a datos (IPC diario)###

cd=read.csv(file.choose(), header=TRUE)
attach(cd)
summary(cd)
dlipc<-diff(log(DATOS))

------------------------------------------------------------------------------

###GARCH(1,2)###

spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 2)), 
	mean.model=list(armaOrder=c(0,0)), distribution.model="std",
	fixed.pars=list(mu=.000791, omega=0.000002, alpha1=0.119947,
	beta1=.515096, beta2=.359048, shape=6.541907))

sim.1=ugarchpath(spec1, n.sim=7209)
sim.2=ugarchpath(spec1, n.sim=7209)
sim.3=ugarchpath(spec1, n.sim=7209)
sim.4=ugarchpath(spec1, n.sim=7209)
sim.5=ugarchpath(spec1, n.sim=7209)
sim.6=ugarchpath(spec1, n.sim=7209)
sim.7=ugarchpath(spec1, n.sim=7209)
sim.8=ugarchpath(spec1, n.sim=7209)
sim.9=ugarchpath(spec1, n.sim=7209)
sim.10=ugarchpath(spec1, n.sim=7209)
t=c(1:7209)

sims1=data.frame(x=t, y=as.numeric(sim.1@path$seriesSim))
sims2=data.frame(x=t, y=as.numeric(sim.2@path$seriesSim))
sims3=data.frame(x=t, y=as.numeric(sim.3@path$seriesSim))
sims4=data.frame(x=t, y=as.numeric(sim.4@path$seriesSim))
sims5=data.frame(x=t, y=as.numeric(sim.5@path$seriesSim))
sims6=data.frame(x=t, y=as.numeric(sim.6@path$seriesSim))
sims7=data.frame(x=t, y=as.numeric(sim.7@path$seriesSim))
sims8=data.frame(x=t, y=as.numeric(sim.8@path$seriesSim))
sims9=data.frame(x=t, y=as.numeric(sim.9@path$seriesSim))
sims10=data.frame(x=t, y=as.numeric(sim.10@path$seriesSim))
sims11=data.frame(x=t, y=dlipc)

plotot=ggplot(sims1,aes(x,y))+geom_line(aes(color="Simulacion 1"))+
  geom_line(data=sims2,aes(color="Simulacion 2"))+
  geom_line(data=sims3,aes(color="Simulacion 3"))+
  geom_line(data=sims4,aes(color="Simulacion 4"))+
  geom_line(data=sims5,aes(color="Simulacion 5"))+
  geom_line(data=sims6,aes(color="Simulacion 6"))+
  geom_line(data=sims7,aes(color="Simulacion 7"))+
  geom_line(data=sims8,aes(color="Simulacion 8"))+
  geom_line(data=sims9,aes(color="Simulacion 9"))+
  geom_line(data=sims10,aes(color="Simulacion 10"))+
  geom_line(data=sims11,aes(color="Rendimientos IPC"))+
  labs(color="Legend text")
plotot

library(gridExtra)
grid.arrange(plotot, plotot2, nrow=2)
------------------------------------------------------------------------------

##################################GARCH-M(p,q)#################################

###GARCH-M(1,1)###

spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1, 1)), 
                 mean.model=list(armaOrder=c(0,0), archm=TRUE), 
                 distribution.model="std",
                 fixed.pars=list(mu=-0.000303, archm=0.107763, 
                                 omega=0.000002, alpha1=0.092673,
                                 beta1=0.9026250, shape=6.627119))

sim2.1=ugarchpath(spec2, n.sim=7209)
sim2.2=ugarchpath(spec2, n.sim=7209)
sim2.3=ugarchpath(spec2, n.sim=7209)
sim2.4=ugarchpath(spec2, n.sim=7209)
sim2.5=ugarchpath(spec2, n.sim=7209)
sim2.6=ugarchpath(spec2, n.sim=7209)
sim2.7=ugarchpath(spec2, n.sim=7209)
sim2.8=ugarchpath(spec2, n.sim=7209)
sim2.9=ugarchpath(spec2, n.sim=7209)
sim2.10=ugarchpath(spec2, n.sim=7209)
t=c(1:7209)

sim2s1=data.frame(x=t, y=as.numeric(sim2.1@path$seriesSim))
sim2s2=data.frame(x=t, y=as.numeric(sim2.2@path$seriesSim))
sim2s3=data.frame(x=t, y=as.numeric(sim2.3@path$seriesSim))
sim2s4=data.frame(x=t, y=as.numeric(sim2.4@path$seriesSim))
sim2s5=data.frame(x=t, y=as.numeric(sim2.5@path$seriesSim))
sim2s6=data.frame(x=t, y=as.numeric(sim2.6@path$seriesSim))
sim2s7=data.frame(x=t, y=as.numeric(sim2.7@path$seriesSim))
sim2s8=data.frame(x=t, y=as.numeric(sim2.8@path$seriesSim))
sim2s9=data.frame(x=t, y=as.numeric(sim2.9@path$seriesSim))
sim2s10=data.frame(x=t, y=as.numeric(sim2.10@path$seriesSim))
sim2s11=data.frame(x=t, y=dlipc)

plotot2=ggplot(sim2s1,aes(x,y))+geom_line(aes(color="Simulacion 1"))+
  geom_line(data=sim2s2,aes(color="Simulacion 2"))+
  geom_line(data=sim2s3,aes(color="Simulacion 3"))+
  geom_line(data=sim2s4,aes(color="Simulacion 4"))+
  geom_line(data=sim2s5,aes(color="Simulacion 5"))+
  geom_line(data=sim2s6,aes(color="Simulacion 6"))+
  geom_line(data=sim2s7,aes(color="Simulacion 7"))+
  geom_line(data=sim2s8,aes(color="Simulacion 8"))+
  geom_line(data=sim2s9,aes(color="Simulacion 9"))+
  geom_line(data=sim2s10,aes(color="Simulacion 10"))+
  geom_line(data=sim2s11,aes(color="Rendimientos IPC"))+
  labs(color="Legend text")
plotot2

-------------------------------------------------------------------------------

##############################EGARCH(p,q)######################################

###EGARCH(1,1)###

spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1, 1)), 
                 mean.model=list(armaOrder=c(0,0)), 
                 distribution.model="std",
                 fixed.pars=list(mu=-0.000563, omega=-0.125957, alpha1=-0.070711,
                                 beta1=0.985696, gamma1=0.175741, shape=7.184073))

sim3.1=ugarchpath(spec3, n.sim=7209)
sim3.2=ugarchpath(spec3, n.sim=7209)
sim3.3=ugarchpath(spec3, n.sim=7209)
sim3.4=ugarchpath(spec3, n.sim=7209)
sim3.5=ugarchpath(spec3, n.sim=7209)
sim3.6=ugarchpath(spec3, n.sim=7209)
sim3.7=ugarchpath(spec3, n.sim=7209)
sim3.8=ugarchpath(spec3, n.sim=7209)
sim3.9=ugarchpath(spec3, n.sim=7209)
sim3.10=ugarchpath(spec3, n.sim=7209)
t=c(1:7209)

sim3s1=data.frame(x=t, y=as.numeric(sim3.1@path$seriesSim))
sim3s2=data.frame(x=t, y=as.numeric(sim3.2@path$seriesSim))
sim3s3=data.frame(x=t, y=as.numeric(sim3.3@path$seriesSim))
sim3s4=data.frame(x=t, y=as.numeric(sim3.4@path$seriesSim))
sim3s5=data.frame(x=t, y=as.numeric(sim3.5@path$seriesSim))
sim3s6=data.frame(x=t, y=as.numeric(sim3.6@path$seriesSim))
sim3s7=data.frame(x=t, y=as.numeric(sim3.7@path$seriesSim))
sim3s8=data.frame(x=t, y=as.numeric(sim3.8@path$seriesSim))
sim3s9=data.frame(x=t, y=as.numeric(sim3.9@path$seriesSim))
sim3s10=data.frame(x=t, y=as.numeric(sim3.10@path$seriesSim))
sim3s11=data.frame(x=t, y=dlipc)

plotot3=ggplot(sim3s1,aes(x,y))+geom_line(aes(color="Simulacion 1"))+
  geom_line(data=sim3s2,aes(color="Simulacion 2"))+
  geom_line(data=sim3s3,aes(color="Simulacion 3"))+
  geom_line(data=sim3s4,aes(color="Simulacion 4"))+
  geom_line(data=sim3s5,aes(color="Simulacion 5"))+
  geom_line(data=sim3s6,aes(color="Simulacion 6"))+
  geom_line(data=sim3s7,aes(color="Simulacion 7"))+
  geom_line(data=sim3s8,aes(color="Simulacion 8"))+
  geom_line(data=sim3s9,aes(color="Simulacion 9"))+
  geom_line(data=sim3s10,aes(color="Simulacion 10"))+
  geom_line(data=sim3s11,aes(color="Rendimientos IPC"))+
  labs(color="Legend text")
plotot3


-------------------------------------------------------------------------------

###############################TGARCH(p,q)#####################################

###TGARCH(1,1)###

spect=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1, 1)), 
                 mean.model=list(armaOrder=c(0,0)), 
                 distribution.model="std", 
		     fixed.pars=list(mu=-0.000627, omega=0.000002, 
	           alpha1=-0.042519, beta1=0.904524, gamma1=0.094753, shape=7.146085))

sim4.1=ugarchpath(spect, n.sim=7209)
sim4.2=ugarchpath(spect, n.sim=7209)
sim4.3=ugarchpath(spect, n.sim=7209)
sim4.4=ugarchpath(spect, n.sim=7209)
sim4.5=ugarchpath(spect, n.sim=7209)
sim4.6=ugarchpath(spect, n.sim=7209)
sim4.7=ugarchpath(spect, n.sim=7209)
sim4.8=ugarchpath(spect, n.sim=7209)
sim4.9=ugarchpath(spect, n.sim=7209)
sim4.10=ugarchpath(spect, n.sim=7209)
t=c(1:7209)

sim4s1=data.frame(x=t, y=as.numeric(sim4.1@path$seriesSim))
sim4s2=data.frame(x=t, y=as.numeric(sim4.2@path$seriesSim))
sim4s3=data.frame(x=t, y=as.numeric(sim4.3@path$seriesSim))
sim4s4=data.frame(x=t, y=as.numeric(sim4.4@path$seriesSim))
sim4s5=data.frame(x=t, y=as.numeric(sim4.5@path$seriesSim))
sim4s6=data.frame(x=t, y=as.numeric(sim4.6@path$seriesSim))
sim4s7=data.frame(x=t, y=as.numeric(sim4.7@path$seriesSim))
sim4s8=data.frame(x=t, y=as.numeric(sim4.8@path$seriesSim))
sim4s9=data.frame(x=t, y=as.numeric(sim4.9@path$seriesSim))
sim4s10=data.frame(x=t, y=as.numeric(sim4.10@path$seriesSim))
sim4s11=data.frame(x=t, y=dlipc)

plotot4=ggplot(sim4s1,aes(x,y))+geom_line(aes(color="Simulacion 1"))+
  geom_line(data=sim4s2,aes(color="Simulacion 2"))+
  geom_line(data=sim4s3,aes(color="Simulacion 3"))+
  geom_line(data=sim4s4,aes(color="Simulacion 4"))+
  geom_line(data=sim4s5,aes(color="Simulacion 5"))+
  geom_line(data=sim4s6,aes(color="Simulacion 6"))+
  geom_line(data=sim4s7,aes(color="Simulacion 7"))+
  geom_line(data=sim4s8,aes(color="Simulacion 8"))+
  geom_line(data=sim4s9,aes(color="Simulacion 9"))+
  geom_line(data=sim4s10,aes(color="Simulacion 10"))+
  geom_line(data=sim4s11,aes(color="Rendimientos IPC"))+
  labs(color="Legend text")
plotot4


-------------------------------------------------------------------------------

##############################RS(k)GARCH(1,1)##################################

###RS(2)GARCH(1,1)###

spec5=CreateSpec(variance.spec = list(model = c("sGARCH")), 
	distribution.spec = list(distribution = c("std")), 
	switch.spec = list(do.mix = FALSE, K = 2))

fit5=FitML(spec = spec5, data = dlipc)
fit5

simt=simulate(object = fit5, nsim = 10L, nahead = 7209L, 
                nburnin = 500L, seed = 1)

MSRIES=read.csv(file.choose(), header=T)
attach(MSRIES)

sim5s1=data.frame(x=t, y=Sim1)
sim5s2=data.frame(x=t, y=Sim2)
sim5s3=data.frame(x=t, y=Sim3)
sim5s4=data.frame(x=t, y=Sim4)
sim5s5=data.frame(x=t, y=Sim5)
sim5s6=data.frame(x=t, y=Sim6)
sim5s7=data.frame(x=t, y=Sim7)
sim5s8=data.frame(x=t, y=Sim8)
sim5s9=data.frame(x=t, y=Sim9)
sim5s10=data.frame(x=t, y=Sim10)
sim5s11=data.frame(x=t, y=dlipc)

plotot5=ggplot(sim5s1,aes(x,y))+geom_line(aes(color="Simulacion 1"))+
  geom_line(data=sim5s2,aes(color="Simulacion 2"))+
  geom_line(data=sim5s3,aes(color="Simulacion 3"))+
  geom_line(data=sim5s4,aes(color="Simulacion 4"))+
  geom_line(data=sim5s5,aes(color="Simulacion 5"))+
  geom_line(data=sim5s6,aes(color="Simulacion 6"))+
  geom_line(data=sim5s7,aes(color="Simulacion 7"))+
  geom_line(data=sim5s8,aes(color="Simulacion 8"))+
  geom_line(data=sim5s9,aes(color="Simulacion 9"))+
  geom_line(data=sim5s10,aes(color="Simulacion 10"))+
  geom_line(data=sim5s11,aes(color="Rendimientos IPC"))+
  labs(color="Legend text")
plotot5


