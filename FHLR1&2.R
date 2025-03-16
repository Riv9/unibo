rm(list = ls())
load(file = "C:\\Users\\virpa\\Documents\\UNIBO\\LMEC_2°\\Machine Learning for Economists\\Project\\act_data.Rda")
library(stats)
library(readr)
library(pracma)
library(writexl)
library(fbi)
library(R.matlab)
library(dplyr)
library(rlang)

data2 <- act_data[1:70,] 

#import estimates from MatLab
chi <- readMat("C:\\Users\\virpa\\Documents\\UNIBO\\LMEC_2°\\Machine Learning for Economists\\Project\\chi.mat")
chi <- as.data.frame((chi))
xi <- readMat("C:\\Users\\virpa\\Documents\\UNIBO\\LMEC_2°\\Machine Learning for Economists\\Project\\xi.mat")
xi <- as.data.frame(xi)
#r=5 q=3 

fcast.chi1 <- readMat("C:\\Users\\virpa\\Documents\\UNIBO\\LMEC_2°\\Machine Learning for Economists\\Project\\fcast_chi1.mat")
fcast.chi1 <- as.data.frame(fcast.chi1)
fcast.chi4 <- readMat("C:\\Users\\virpa\\Documents\\UNIBO\\LMEC_2°\\Machine Learning for Economists\\Project\\fcast_chi4.mat")
fcast.chi4 <- as.data.frame(fcast.chi4)

colnames(xi) <- colnames(data2)
colnames(chi) <- colnames(data2)
colnames(fcast.chi1) <- colnames(data2)
colnames(fcast.chi4) <- colnames(data2)

chi.1step <- rbind(chi,fcast.chi1)
xi.1step = act_data - chi.1step
chi.4step <- rbind(chi,fcast.chi4)
xi.4step = act_data - chi.4step

## AR(1)
library(forecast)

GDP <- act_data$GDPC1
GDP <- as.data.frame(GDP)
train_GDP <- window(GDP$GDP, end = 70)
test_GDP <- window(GDP$GDP, start = 71)
fit <- arima(train_GDP, order = c(1,0,0))
w_size <- length(train_GDP)

#1-STEP
h <- 1
n <- length(test_GDP)-h+1
fcmat1 <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- GDP[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat1[i,]<- forecast(refit, h = h)$mean
}

mean((fcmat1 - act_data[71:78,]$GDPC1)^2) #0.1671194

#4-STEP
h <- 4
n <- length(test_GDP)-h+1
fcmat4 <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- GDP[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat4[i,]<- forecast(refit, h = h)$mean
}

mean((fcmat4 - act_data[74:78,]$GDPC1)^2) #0.1920308

############# FHLR1 ############
#Only forecast through common part, i.e. chi
#1-STEP 
FHLR1 <- fcast.chi1$GDPC1
FHLR1 <- as.data.frame(FHLR1)
FHLR1 <- FHLR1 %>% 
  rename(GDPC1 = FHLR1) 

data2_gdp <- data2[,1]
data2_gdp <- as.data.frame(data2_gdp)
data2_gdp <- data2_gdp %>% 
  rename( GDPC1 = data2_gdp)

FHLR1 <- rbind(data2_gdp, FHLR1)

plot.ts(FHLR1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="GDPC1", xlab="Time (Quarters)")
points(act_data$GDPC1, type = "l", col = 1)
abline(v="70", col=8, lty=3, h=0)

mean((FHLR1[71:78,] - act_data[71:78,]$GDPC1)^2) #0.2005294

#4-STEP
FHLR1.4step <- fcast.chi4$GDPC1
FHLR1.4step <- as.data.frame(FHLR1.4step)
FHLR1.4step <- FHLR1.4step %>% 
  rename(GDPC1 = FHLR1.4step) 

FHLR1.4step <- rbind(data2_gdp, FHLR1.4step)

#to fix
#plot.ts(FHLR1.4step$GDPC1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="GDPC1", xlab="Time (Quarters)")
#points(act_data$GDPC1, type = "l", col = 1)
#abline(v="70", col=8, lty=3, h=0)

mean((FHLR1.4step[74:78,] - act_data[74:78,]$GDPC1)^2) #0.2455467

############# FHLR2 ############
#Forecast also idiosyncratic part, i.e. xi, through ar(1)
#1-STEP
xi_gdp <- xi.1step$GDPC1
xi_gdp <- as.data.frame(xi_gdp)
train_xi <- window(xi_gdp$xi_gdp, end = 70)
test_xi <- window(xi_gdp$xi_gdp, start = 71)
fit <- arima(train_xi, order = c(1,0,0))
w_size <- length(train_xi)

h <- 1
n <- length(test_xi)-h+1
fcmat1.xi <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- xi_gdp[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat1.xi[i,]<- forecast(refit, h = h)$mean[h]
}
mean((fcmat1.xi - xi.1step[71:78,]$GDPC1)^2) #0.2822138

#x=chi+xi
gdp_fhlr2 = FHLR1[71:78,] + fcmat1.xi
gdp_fhlr2 <- as.data.frame(gdp_fhlr2)
gdp_fhlr2 <- gdp_fhlr2 %>% 
  rename( GDPC1 = V1
  )
FHLR2 <- rbind(data2_gdp, gdp_fhlr2) 

plot.ts(FHLR2$GDPC1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="GDPC1", xlab="Time (Quarters)")
points(act_data$GDPC1, type = "l", col = 1)
abline(v="70", col=8, lty=3,h=0)

mean((FHLR2[71:78,] - act_data[71:78,]$GDPC1)^2) #0.2822138

#4-STEP
h <- 4
n <- length(test_xi)-h+1
fcmat4.xi <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- xi_gdp[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat4.xi[i,]<- forecast(refit, h = h)$mean
}

mean((fcmat4.xi[,4] - xi.4step[74:78,]$GDPC1)^2) #0.2592058

#x=chi+xi
gdp_fhlr2.4step = FHLR1.4step[74:78,] + fcmat4.xi[,4]
gdp_fhlr2.4step <- as.data.frame(gdp_fhlr2.4step)
gdp_fhlr2.4step <- gdp_fhlr2.4step %>% 
  rename( GDPC1 = gdp_fhlr2.4step)

FHLR2.4step <- rbind(data2_gdp, gdp_fhlr2.4step) 

#plot to fix
#plot.ts(FHLR2.4step$GDPC1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="GDPC1", xlab="Time (Quarters)")
#points(act_data$GDPC1, type = "l", col = 1)
#abline(v="70", col=8, lty=3,h=0)

mean((FHLR2.4step[71:75,] - act_data[74:78,]$GDPC1)^2) #0.2592058
