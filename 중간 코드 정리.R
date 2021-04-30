# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 거리 측도

# 유클리드 거리
dist(X[1:2,], method="euclidean")

# 표준화 거리
D <- diag(cov(X))*diag(n)
X12 <- as.matrix(X[1,]-X[2,])
ds <- sqrt(X12 %*% solve(D) %*% t(X12)); ds

# 마할라노비스 거리
dm <- sqrt(X12 %*% solve(cov(X)) %*% t(X12)); dm


#####################################
##### 그래프

# 등고선 그림
mu1 <- 0; mu2 <- 0; s11 <- 10; s12 <- 15; s22 <- 10; rho <- 0.5
x1 <- seq(-10, 10, length=41); x2 <- x1
f <- function(x1,x2){
  term1 <- 1/(2*pi*sqrt(s11*s22*(1-rho^2)))
  term2 <- -1/(2*(1-rho^2))
  term3 <- (x1-mu1)^2/s11
  term4 <- (x2-mu2)^2/s22
  term5 <- 2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
  term1*exp(term2*(term3+term4-term5))
}
z <- outer(x1, x2, f)
win.graph()
contour(x1, x2, z, xlab="x1", ylab="x2", main="contour plot")


#####################################
##### 정규성 평가

# 일변량 정규성 Q-Q 그림
qqnorm(X)
qqline(X)

# 다변량 정규성 카이제곱 그림
chi2.plot <- function(x){
  xbar <- colMeans(x)
  n <- dim(x)[[1]]
  vp <- dim(x)[[2]]
  S <- cov(x)
  d <- mahalanobis(x, xbar, S)
  d2 <- sort(d)
  tt <- seq(1,n)
  t <- (tt-0.5)/n
  q <- qchisq(t, vp)
  plot(q, d2, pch="*", main="Chisquare plot for multivariate normality", 
       xlab="chi2 quantile", ylab="ordered Mahalanobis d^2")
  abline(0,1)
  return(list(xbar, S))
}


#####################################
##### 행렬

# 직교행렬
I <- round(t(X)%*%X, digits = 3)

# 스펙트럼 분해
eX <- eigen(X)
eX1 <- eX$values[[1]] * eX$vectors[,1] %*% t(eX$vectors[,1])
eX2 <- eX$values[[2]] * eX$vectors[,2] %*% t(eX$vectors[,2])
eX1+eX2; X

# 제곱근행렬
eX <- eigen(X)
eX1 <- sqrt(eX$values[[1]]) * eX$vectors[,1] %*% t(eX$vectors[,1])
eX2 <- sqrt(eX$values[[2]]) * eX$vectors[,2] %*% t(eX$vectors[,2])
eX1+eX2


#####################################
##### 다변량 기초

# 일반화분산
det(S)

# 총분산
sum(diag(S))

# 상호상관성 측도 : 조건수
max(eigen(R)$values)/min(eigen(R)$values)


#####################################
##### Hotelling T^2 검정

# 한 모집단
n <- dim(X)[[1]]
p <- dim(X)[[2]]
T2 <- n*t(barX-mu0) %*% solve(S) %*% (barX-mu0); T2
(n-1)*p/(n-p)*qf(0.05, n, n-p, lower.tail=F)
f <- (n-p)/((n-1)*p)*T2
1-pf(f, p, n-p)

# 두 모집단, 공분산행렬 같음
X1 <- X[(group==1), 1:2]; X2 <- X[(group==2), 1:2]
bar1 <- colMeans(X1); bar2 <- colMeans(X2)
s1 <- cov(X1); s2 <- cov(X2)
n1 <- nrow(X1); n2 <- nrow(X2)
p <- ncol(X[,1:2])
S <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)

library(MASS)
T2 <- (n1*n2/(n1+n2))*t(bar1-bar2) %*% ginv(S) %*% (bar1-bar2); T2
(n1+n2-2)*p/(n1+n2-p-1)*qf(0.05, p, n1+n2-p-1, lower.tail=F)
f <- ((n1+n2-p-1)/((n1+n2-2)*p))*T2
1-pf(f, p, n1+n2-p-1)

# 두 모집단, 공분산행렬 다름
library(MASS)
T2 <- t(bar1-bar2) %*% ginv(s1/n1+s2/n2) %*% (bar1-bar2); T2
qchisq(0.05, p, lower.tail=F)
pchisq(T2, p, lower.tail=F)

# 동시 신뢰영역
library(ggplot2); library(devtools); library(proto); library(plotly)
p <- ggplot(data=X, aes(x=X[,1], y=X[,2])) +
  geom_point() +
  stat_ellipse(geom="polygon", alpha=0.5) +
  labs(x="X1", y="X2")
fig <- ggplotly(p); fig
