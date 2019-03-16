# GraficasTesis
#Codigo en R de las gráficas plasmadas en la tesis. sp hace referencia a la base de datos del índice S$P 500 y x hace referencia a la #base de datos del IPC.

####SCRIPT GRAFICAS###

library(stats)
library(normtest)
library(ggplot2)
library(timeDate)
library(timeSeries)
library(rugarch)
library(fBasics)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggfortify)
library(hrbrthemes)
library(forecast)

#####GRAFICAS CAPITULO SERIES DE TIEMPO###
sp<- read.csv(file.choose(), header=TRUE)
attach(sp)
head(sp)
poors<- Adj.Close
tsp<- ts(Adj.Close, frequency = 252, end = c(2019, 1,02))
tsp

ggplot2::autoplot
autoplot(stl(tsp, s.window = 'periodic'), ts.colour="blue")
autoplot(tsp, ylab="Valor", xlab="Tiempo", colour="darkslategray4", main="S&P 500")
autoplot(acf(tsp, plot = FALSE, colour='darkslategray4'))
autoplot(pacf(tsp, plot = FALSE))
ggtsdiag(auto.arima(tsp))

ar1a <- arima.sim(model = list(order = c(1, 0, 0), ar = .78), n = 1000)
ar1b <- arima.sim(model = list(order = c(1, 0, 0), ar = -.78), n = 1000)
par(mfrow=c(2,1))
acf1(ar1a, max.lag=20)
acf1(ar1b, max.lag=20)

ar2a<-arima.sim(model=list(ar=c(.9,-.2)),n=1000)
ar2b<-arima.sim(model=list(ar=c(1.4,-.7)),n=1000)
ar2c<-arima.sim(model=list(ar=c(.4, .3)),n=1000)
ar2d<-arima.sim(model=list(ar=c(-.35,.5)),n=1000)
par(mfrow=c(2,2))
acf1(ar2a, max.lag=20)
acf1(ar2b, max.lag=20)
acf1(ar2c, max.lag=20)
acf1(ar2d, max.lag=20)

###GRAFICAS CAPITULO CALIBRACION IPC###
x= read.csv(file.choose(),header=TRUE)
attach(x)
summary(x)
xdat= ts(DATOS, frequency = 251, end = c(2018, 251))

ggplot2::autoplot
autoplot(stl(xdat, s.window = 'periodic'), ts.colour="blue")
autoplot(xdat, ylab="IPC BMV", xlab="Time", colour="darkslategray4", main="S&P/BMV IPC")
autoplot(acf(xdat, plot = FALSE, colour='darkslategray4'))
autoplot(pacf(xdat, plot = FALSE))
ggtsdiag(auto.arima(xdat))

lxdat=log(xdat)
lxdat
dlxdat=diff(lxdat)
dlxdat
summary(dlxdat)
length(dlxdat)
length(xdat)
length(DATOS)
autoplot(stl(dlxdat, s.window = 'periodic'), colour = 'darkslategray4', main= "Descomposicion tradicional - rendimientos logaritmicos IPC")
autoplot(dlxdat, ylab="IPC BMV", xlab="Time", colour='darkslategray4', main = "Rendimientos logaritmicos IPC", conf.int.fill = "#0000FF", conf.int.value = 0.8)
acf1= acf(dlxdat, ylab="valor", xlab="rezago", main="FAC - rendimientos del IPC")
pacf1= pacf(dlxdat, ylab="valor", xlab="rezago", main="FACP - rendimientos del IPC" )
ggtsdiag(auto.arima(dlxdat))
grid.arrange(acf1, pacf1, ncol=2)
