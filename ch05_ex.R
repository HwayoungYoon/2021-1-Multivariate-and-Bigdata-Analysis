# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 5.1
X <- as.data.frame(matrix(c(2,8,6,8,12,9,9,10), nc=2))

# 5.1.a
Xbar <- colMeans(X); Xbar

# 5.1.b
S <- cov(X); S

# 5.1.c
solve(S)

# 5.1.d
n <- dim(X)[[1]]
p <- dim(X)[[2]]
mu0 <- c(7,11)
T2 <- n*t(Xbar-mu0) %*% solve(S) %*% (Xbar-mu0); T2
(n-1)*p/(n-p)*qf(0.05, n, n-p, lower.tail=F)
f <- (n-p)/((n-1)*p)*T2
1-pf(f, p, n-p)

# 5.1.e
C <- matrix(c(1,1,-1,1), nc=2); C

# 5.1.f
cX <- t(C %*% t(X)); cX

# 5.1.g
cmu0 <- C %*% mu0
cXbar <- colMeans(cX)
cS <- cov(cX)

library(MASS)
cT2 <- n*t(cXbar-cmu0) %*% ginv(cS) %*% (cXbar-cmu0)
cT2; T2

# 5.1.h
library(ggplot2)
library(devtools)
library(proto)
library(plotly)
p <- ggplot(data=X, aes(x=X[,1], y=X[,2])) +
  geom_point() +
  stat_ellipse(geom="polygon", alpha=0.5) +
  labs(x="Xi1", y="Xi2")
fig <- ggplotly(p); fig

# 5.1.i
dist(t(X)[1:2,], method="euclidean")


########## 5.5
library(readxl)
EP <- read_xlsx("data/practice/Table5.9_EngineerPilot.xlsx")
EP <- as.data.frame(EP)
attach(EP)
EP.E <- EP[(group=="E"), 1:2]
EP.P <- EP[(group=="P"), 1:2]

# 5.5.a
Ebar <- colMeans(EP.E); Ebar
sE <- cov(EP.E); sE
Pbar <- colMeans(EP.P); Pbar
sP <- cov(EP.P); sP

# 5.5.b
nE <- nrow(EP.E)
nP <- nrow(EP.P)
p <- ncol(EP[,1:2])
S <- ((nE-1)*sE+(nP-1)*sP)/(nE+nP-2)

library(MASS)
T2_1 <- (nE*nP/(nE+nP))*t(Ebar-Pbar) %*% ginv(S) %*% (Ebar-Pbar); T2_1
(nE+nP-2)*p/(nE+nP-p-1)*qf(0.05, p, nE+nP-p-1, lower.tail=F)
f <- ((nE+nP-p-1)/((nE+nP-2)*p))*T2_1
1-pf(f, p, nE+nP-p-1)

# 5.5.c
library(MASS)
T2_2 <- t(Ebar-Pbar) %*% ginv(sE/nE+sP/nP) %*% (Ebar-Pbar); T2_2
qchisq(0.05, p, lower.tail=F)
pchisq(T2_2, p, lower.tail=F)


########## 5.7
library(readxl)
Edata <- read_xlsx("data/practice/Table5.11_EconomyPeriod.xlsx")
Edata <- as.data.frame(Edata)
attach(Edata)
EC <- Edata[(group=="C"), 1:4]
EP <- Edata[(group=="P"), 1:4]

# 5.7.a
barC <- colMeans(EC); barC
SC <- cov(EC); SC
barP <- colMeans(EP); barP
SP <- cov(EP); SP

# 5.7.b
nC <- nrow(EC)
nP <- nrow(EP)
S <- ((nC-1)*SC+(nP-1)*SP)/(nC+nP-2); S

# 5.7.c
p <- ncol(Edata[,1:4])
library(MASS)
T2_1 <- (nC*nP/(nC+nP))*t(barC-barP) %*% ginv(S) %*% (barC-barP); T2_1
(nC+nP-2)*p/(nC+nP-p-1)*qf(0.05, p, nC+nP-p-1, lower.tail=F)
f <- ((nC+nP-p-1)/((nC+nP-2)*p))*T2_1
1-pf(f, p, nC+nP-p-1)

# 5.7.d
library(MASS)
T2_2 <- t(barC-barP) %*% ginv(SC/nC+SP/nP) %*% (barC-barP); T2_2
qchisq(0.05, p, lower.tail=F)
pchisq(T2_2, p, lower.tail=F)
