library(fpp2)
library(splines)
library(ggplot2)

#---Load Monthly TS---
m_gcag<-read.csv(file="M_GCAG.csv")
m_gcag_ts <-ts(m_gcag[,-1], start=1880, end=2016, frequency=12)
autoplot(m_gcag_ts)+ xlab("Time") + ylab("Temperature") + ggtitle("GCAG Monthly Temperature Change")

#
m_gistemp<-read.csv(file="M_GISTEMP.csv")
m_gistemp_ts <-ts(m_gistemp[,-1], start=1880, end=2016, frequency=12)
autoplot(m_gistemp_ts)+ xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Monthly Temperature Change")

#---Load Annual TS---
a_gcag<-read.csv(file="A_GCAG.csv") #years and values
a_gcag_ts <-ts(a_gcag[,-1], start=1880, end=2016)
autoplot(a_gcag_ts)+ xlab("Time") + ylab("Temperature") + ggtitle("GCAG Annual Temperature Change")

#
a_gistemp<-read.csv(file="A_GISTEMP.csv") #years and values
a_gistemp_ts <-ts(a_gistemp[,-1], start=1880, end=2016)
autoplot(a_gistemp_ts)+ xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Annual Temperature Change")

#---Neural Network Analysis---
NeuralPredict_M_GCAG <- nnetar(m_gcag_ts, P=12, lambda = "auto")
autoplot(forecast(NeuralPredict_M_GCAG,h=240)) + xlab("Time") + ylab("Temperature") + ggtitle("GCAG Monthly Temperature w/ 20 year NN forecast")

#
NeuralPredict_M_GISTEMP <- nnetar(m_gistemp_ts, P=12, lambda = "auto")
autoplot(forecast(NeuralPredict_M_GISTEMP,h=240)) + xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Monthly Temperature w/ 20 year NN forecast")

#
NeuralPredict_A_GCAG <- nnetar(a_gcag_ts, lambda = "auto")
autoplot(forecast(NeuralPredict_A_GCAG,h=20)) + xlab("Time") + ylab("Temperature") + ggtitle("GCAG Annual Temperature w/ 20 year NN forecast")

#
NeuralPredict_A_GISTEMP <- nnetar(a_gistemp_ts, lambda = "auto")
autoplot(forecast(NeuralPredict_A_GISTEMP,h=20)) + xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Annual Temperature w/ 20 year NN forecast")

#---MultiSeries---
Sim_M_GCAG <- ts(matrix(0, nrow=20L, ncol=5L),
                    start=end(m_gcag_ts)[1L]+1L)
for(i in seq(5))
  Sim_M_GCAG[,i] <- simulate(NeuralPredict_M_GCAG, nsim=20L)
autoplot(m_gcag_ts) + autolayer(Sim_M_GCAG) + xlab("Time") + ylab("Temperature") + ggtitle("GCAG Monthly Temperature w/ 20 year NN forecast")

#
Sim_M_GISTEMP <- ts(matrix(0, nrow=20L, ncol=5L),
                    start=end(m_gistemp_ts)[1L]+1L)
for(i in seq(5))
  Sim_M_GISTEMP[,i] <- simulate(NeuralPredict_M_GISTEMP, nsim=20L)
autoplot(m_gistemp_ts) + autolayer(Sim_M_GISTEMP) + xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Monthly Temperature w/ 20 year NN forecast")

#
Sim_A_GCAG <- ts(matrix(0, nrow=20L, ncol=9L),
                    start=end(a_gcag_ts)[1L]+1L)
for(i in seq(9))
  Sim_A_GCAG[,i] <- simulate(NeuralPredict_A_GCAG, nsim=20L)
autoplot(a_gcag_ts) + autolayer(Sim_A_GCAG) + xlab("Time") + ylab("Temperature") + ggtitle("GCAG Annual Temperature w/ 20 year NN forecast")

#
Sim_A_GISTEMP <- ts(matrix(0, nrow=20L, ncol=9L),
          start=end(a_gistemp_ts)[1L]+1L)
for(i in seq(9))
  Sim_A_GISTEMP[,i] <- simulate(NeuralPredict_A_GISTEMP, nsim=20L)
autoplot(a_gistemp_ts) + autolayer(Sim_A_GISTEMP) + xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Annual Temperature w/ 20 year NN forecast")

#---Prediction Intervals---
M_GCAG_PI <- forecast(NeuralPredict_M_GCAG, PI=TRUE, h=240, P=12, lambda = "auto")
autoplot(M_GCAG_PI) + xlab("Time") + ylab("Temperature") + ggtitle("GCAG Monthly Temperature Change w/ PIs")

#
M_GISTEMP_PI <- forecast(NeuralPredict_M_GISTEMP, PI=TRUE, h=240, P=12, lambda = "auto")
autoplot(M_GISTEMP_PI) + xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Monthly Temperature Change w/ PIs")

#
A_GCAG_PI <- forecast(NeuralPredict_A_GCAG, PI=TRUE, h=20, lambda = "auto")
autoplot(A_GCAG_PI) + xlab("Time") + ylab("Temperature") + ggtitle("GCAG Annual Temperature Change w/ PIs")

#
A_GISTEMP_PI <- forecast(NeuralPredict_A_GISTEMP, PI=TRUE, h=20, lambda = "auto")
autoplot(A_GISTEMP_PI) + xlab("Time") + ylab("Temperature") + ggtitle("GISTEMP Annual Temperature Change w/ PIs")