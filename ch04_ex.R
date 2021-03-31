# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### chi2.plot
chi2.plot <- function(x){
  xbar <- colMeans(x)
  n <- dim(x)[[1]]            # number of observations
  vp <- dim(x)[[2]]           # number of variables
  S <- cov(x)
  d <- mahalanobis(x, xbar, S)  # Mahalanobis distance^2 ~chisq(vp)
  # if multivariate normality
  d2 <- sort(d)
  tt <- seq(1,n)
  t <- (tt-0.5)/n     # edf
  q <- qchisq(t, vp)
  plot(q, d2, pch="*", main="Chisquare plot for multivariate normality", 
       xlab="chi2 quantile", ylab="ordered Mahalanobis d^2")
  abline(0,1)
  return(list(xbar, S))
}


########## 4.2
# 4.2.a
# 4.2.b
# 4.2.c
# 4.2.d
# 4.2.e
# 4.2.f
# 4.2.g
# 4.2.h
# 4.2.i
# 4.2.j
# 4.2.k
# 4.2.l
# 4.2.m
# 4.2.n

########## 4.3
E1 <- matrix(c(14,8,3,8,5,2,3,2,1), nc=3)
E2 <- matrix(c(6,6,1,6,8,2,1,2,1), nc=3)
E1; E2

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
qqnorm(X1, main="Normal Q-Q plot of X1"); qqline(X1)
qqnorm(X2, main="Normal Q-Q plot of X2"); qqline(X2)

# 4.4.c
chi2.plot <- function(x){
  xbar <- colMeans(x)
  n <- dim(x)[[1]]            # number of observations
  vp <- dim(x)[[2]]           # number of variables
  S <- cov(x)
  d <- mahalanobis(x, xbar, S)  # Mahalanobis distance^2 ~chisq(vp)
  # if multivariate normality
  d2 <- sort(d)
  tt <- seq(1,n)
  t <- (tt-0.5)/n     # edf
  q <- qchisq(t, vp)
  plot(q, d2, pch="*", main="Chisquare plot for multivariate normality", 
       xlab="chi2 quantile", ylab="ordered Mahalanobis d^2")
  abline(0,1)
  return(list(xbar, S))
}
chi2.plot(KI)


########## 4.6
# 4.6.a
# 4.6.b
