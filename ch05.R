# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 5.1. Hotelling T^2 test

# 데이터 입력
sweat <- read.csv("data/sweat.csv", header=T); sweat
attach(sweat)
x <- sweat[,2:4]; x

p <- ncol(x); p
xbar <- apply(x, 2, mean); xbar
colMeans(x)
S <- cov(x); S
n <- dim(x)[[1]]; n

# 일집단 Hotelling T^2 test
library(MASS)
mu0 <- c(4,50,10); mu0
T2 <- n*t(xbar-mu0) %*% ginv(S) %*% (xbar-mu0); T2
f <- (n-p)/((n-1)*p)*T2; f
pvalue <- 1-pf(f, p, n-p); pvalue

# 동시 신뢰영역
library(ggplot2)
## 95%, multivariate t
ggplot(sweat, aes(x1,x2)) +
  geom_point() +
  stat_ellipse()
## 95%, multivariate n
ggplot(sweat, aes(x1,x2)) +
  geom_point() +
  stat_ellipse(type="norm")
## 90%, multivariate n
ggplot(sweat, aes(x1,x2)) +
  geom_point() +
  stat_ellipse(type="norm", level=0.90)

# 3차원 산점도
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x, pch="*", main="3d scatterplot")


#####################################
##### 예제 5.2. H0: male mean vector = female mean vector

# 데이터 입력
d <- read.csv("data/pschy.csv", header=T)
head(d)
attach(d)
## male data
xm <- d[gender==1, 2:5]; xm
## female data
xf <- d[gender==2, 2:5]; xf

sm <- cov(xm); sm
sf <- cov(xf); sf
xmbar <- colMeans(xm); xmbar
xfbar <- colMeans(xf); xfbar
n1 <- nrow(xm); n1
n2 <- nrow(xf); n2
S <- ((n1-1)*sm+(n2-1)*sf)/(n1+n2-2); S
p <- 4

# 두 모집단에 대한 Hotelling T^2 test
library(MASS)
T2 <- t(xmbar-xfbar) %*% ginv((1/n1+1/n2)*S) %*% (xmbar-xfbar); T2
f <- ((n1+n2-p-1)/((n1+n2-2)*p))*T2; f
pvalue <- 1-pf(f, p, n1+n2-p-1); pvalue

#####################################
##### 연습문제 5.1. obs (x1, x2)' 2*1 column vector

# 데이터 입력
x <- rbind(c(2,12), c(8,9), c(6,9), c(8,10)); x

n <- dim(x)[[1]]; n
p <- dim(x)[[2]]; p

# scatter plot
plot(x[,1], x[,2], pch=19, cex=2, col="blue")

# (a).
xbar <- colMeans(x); xbar

# (b).
S <- cov(x); S

# (c).
sinv <- solve(S); sinv

# (d). mean vector under H0: mu=mu0
mu0 <- c(7,11); mu0
T2 <- n*t(xbar-mu0) %*% solve(S) %*% (xbar-mu0); T2
f <- (n-p)/((n-1)*p)*T2; f
pvalue <- 1-pf(f, p, n-p); pvalue
pv <- pf(f, p, n-p, lower.tail=F); pv

# (e).
C <- rbind(c(1,-1), c(1,1)); C
C %*% x[1,]
C %*% x[2,]
C %*% x[3,]
C %*% x[4,]
cx <- rbind(t(C %*% x[1,]), t(C %*% x[2,]), 
            t(C %*% x[3,]), t(C %*% x[4,])); cx
C %*% t(x)

# (f). mean vector under H0
cmu0 <- C %*% mu0; cmu0
cxbar <- colMeans(cx); cxbar
cS <- cov(cx); cS

# (g).
library(MASS)
cT2 <- n*t(cxbar-cmu0) %*% ginv(cS) %*% (cxbar-cmu0)
cT2; T2
cf <- ((n-p)/(n-1)*p)*cT2; cf
cpvalue <- 1-pf(cf, p, n-p); cpvalue


#####################################
##### 연습문제 5.5.

# 데이터 입력
library(readxl)
A <- read_xlsx("data/practice/Table5.9_EngineerPilot.xlsx")
A <- as.data.frame(A); A
attach(A)
eng <- A[(group=="E"), 1:2]; eng
pil <- A[(group=="P"), 1:2]; pil

# (a).
ebar <- colMeans(eng); ebar
s1 <- cov(eng); s1
pbar <- colMeans(pil); pbar
s2 <- cov(pil); s2

## 동시 신뢰영역
library(ggplot2)
ggplot(A, aes(A[,1], A[,2], pch=group)) +
  geom_point() +
  stat_ellipse(aes(A[,1], A[,2], color=group), type="norm", level=0.9)

## another draw
install.packages("devtools")
install.packages("proto")
install.packages("plotly")
library(ggplot2)
library(devtools)
library(proto)
library(plotly)

p <- ggplot(data=A, aes(x=A[,1], y=A[,2])) +
  geom_point() +
  stat_ellipse(geom="polygon", alpha=0.5, aes(fill=group))
fig <- ggplotly(p); fig

# (b). 두 집단의 공분산이 같을 때 두 집단의 모평균벡터가 같은가
n1 <- nrow(eng); n1
n2 <- nrow(pil); n2
p <- ncol(A[,1:2]); p
spool <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2); spool

library(MASS)
T2 <- (n1*n2/(n1+n2))*t(ebar-pbar) %*% ginv(spool) %*% (ebar-pbar); T2
f <- ((n1+n2-p-1)/((n1+n2-2)*p))*T2; f
pvalue <- 1-pf(f, p, n1+n2-p-1); pvalue

# (c). 두 집단의 공분산이 다를 때 두 집단의 모평균벡터가 같은가
library(MASS)
T2_2 <- t(ebar-pbar) %*% ginv(s1/n1+s2/n2) %*% (ebar-pbar); T2_2
## n1-p, n2-p가 충분히 클 때
p.value <- pchisq(T2_2, p, lower.tail=F); p.value

# manova(6장)
fit <- manova(cbind(A[,1], A[,2]) ~ group)
summary(fit)


#####################################
##### 연습문제 5.7.

# 공분산행렬 동일성 검정
# 반응변수 개수 p=4
# 설명변수의 범주 개수 g=2
