# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 4.2
EX1 <- -3
EX2 <- 1
EX3 <- 4
VX1 <- 1
VX2 <- 5
VX3 <- 2
CX12 <- -2
CX13 <- 0
CX23 <- 0
EX <- matrix(c(EX1,EX2,EX3), nc=1)
E <- matrix(c(VX1,CX12,CX13,CX12,VX2,CX23,CX13,CX23,VX3), nc=3)

# 4.2.a
CX12
CX12/(sqrt(VX1)*sqrt(VX2))

# 4.2.b
CX13

# 4.2.c
CX23

# 4.2.d
CX13; CX23

# 4.2.e
CX13; CX23

# 4.2.f
CX13; CX23

# 4.2.g
a <- matrix(c(3,-2,1), nc=1)
t(a)%*%EX
t(a)%*%E%*%a

# 4.2.k
det(E)

# 4.2.m
eigen(E)

# 4.2.n
eigen(solve(E))


########## 4.3
E1 <- matrix(c(14,8,3,8,5,2,3,2,1), nc=3)
E2 <- matrix(c(6,6,1,6,8,2,1,2,1), nc=3)

# 일반화분산
det(E1); det(E2)

# 총분산
sum(diag(E1)); sum(diag(E2))


########## 4.4
library(readxl)
data4 <- read_xlsx("data/practice/Table4.6_2001KoreaIndustry.xlsx")
KI <- data4[,2:3]

# 4.4.a
colMeans(KI)
cov(KI)

# 4.4.b
attach(KI)
qqnorm(X1, main="총자본투자효율(X1)의 Q-Q 그림"); qqline(X1)
qqnorm(X2, main="노동소득 분배율(X2)의 Q-Q 그림"); qqline(X2)

# 4.4.c
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
chi2.plot(KI)
