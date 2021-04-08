# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 5.1
x <- rbind(c(2,12), c(8,9), c(6,9), c(8,10)); x

n <- dim(x)[[1]]; n
p <- dim(x)[[2]]; p

# scatter plot
plot(x[,1], x[,2], pch=19, cex=2, col="blue")

# 5.1.a
xbar <- colMeans(x); xbar

# 5.1.b
S <- cov(x); S

# 5.1.c
sinv <- solve(S); sinv

# 5.1.d mean vector under H0: mu=mu0
mu0 <- c(7,11); mu0
T2 <- n*t(xbar-mu0) %*% solve(S) %*% (xbar-mu0); T2
f <- ((n-p)/(n-1)*p)*T2; f
pvalue <- 1-pf(f, p, n-p); pvalue
pv <- pf(f, p, n-p, lower.tail=F); pv

# 5.1.e
C <- rbind(c(1,-1), c(1,1)); C
C %*% x[1,]
C %*% x[2,]
C %*% x[3,]
C %*% x[4,]
cx <- rbind(t(C %*% x[1,]), t(C %*% x[2,]), 
            t(C %*% x[3,]), t(C %*% x[4,])); cx
C %*% t(x)

# 5.1.f mean vector under H0
cmu0 <- C %*% mu0; cmu0
cxbar <- colMeans(cx); cxbar
cS <- cov(cx); cS

# 5.1.g
library(MASS)
cT2 <- n*t(cxbar-cmu0) %*% ginv(cS) %*% (cxbar-cmu0)
cT2; T2
cf <- ((n-p)/(n-1)*p)*cT2; cf
cpvalue <- 1-pf(cf, p, n-p); cpvalue

# 5.1.h

# 5.1.i
dist(t(x)[1:2,], method="euclidean")


########## 5.5
library(readxl)
A <- read_xlsx("data/practice/Table5.9_EngineerPilot.xlsx")
A <- as.data.frame(A); A
attach(A)
eng <- A[(group=="E"), 1:2]; eng
pil <- A[(group=="P"), 1:2]; pil

# 5.5.a
ebar <- colMeans(eng); ebar
s1 <- cov(eng); s1
pbar <- colMeans(pil); pbar
s2 <- cov(pil); s2

# 5.5.b
n1 <- nrow(eng); n1
n2 <- nrow(pil); n2
p <- ncol(A[,1:2]); p
spool <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2); spool

library(MASS)
T2 <- (n1*n2/(n1+n2))*t(ebar-pbar) %*% ginv(spool) %*% (ebar-pbar); spool
f <- ((n1+n2-p-1)/((n1+n2-2)*p))*T2; f
pvalue <- 1-pf(f, p, n1+n2-p-1); pvalue

# 5.5.c
library(MASS)
T2_2 <- t(ebar-pbar) %*% ginv(s1/n1+s2/n2) %*% (ebar-pbar); T2_2
p.value <- pchisq(T2_2, p, lower.tail=F); p.value


########## 5.7
library(readxl)
Edata <- read_xlsx("data/practice/Table5.11_EconomyPeriod.xlsx")
Edata <- as.data.frame(Edata); Edata
attach(Edata)
EC <- Edata[(group=="C"), 1:4]; EC
EP <- Edata[(group=="P"), 1:4]; EP

# 5.7.a
barC <- colMeans(EC); barC
SC <- cov(EC); SC
barP <- colMeans(EP); barP
SP <- cov(EP); SP

# 5.7.b
nC <- nrow(EC); nC
nP <- nrow(EP); nP
S <- ((nC-1)*SC+(nP-1)*SP)/(nC+nP-2); S

# 5.7.c
n <- ncol(Edata[,1:4]); n
library(MASS)
T2_1 <- (nC*nP/(nC+nP))*t(barC-barP) %*% ginv(S) %*% (barC-barP); T2_1
f1 <- ((nC+nP-n-1)/((nC+nP-2)*n))*T2_1; f1
p1 <- 1-pf(f1, n, nC+nP-n-1); p1

# 5.7.d
library(MASS)
T2_2 <- t(barC-barP) %*% ginv(SC/nC+SP/nP) %*% (barC-barP); T2_2
p2 <- pchisq(T2_2, n, lower.tail=F); p2
