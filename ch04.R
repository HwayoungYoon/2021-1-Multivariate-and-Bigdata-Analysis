# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 4.2.

# 데이터 입력
a <- read.csv("data/나이와수입.csv", header=T)
attach(a)
a
b <- a[,2:3]

# 평균벡터
m <- colMeans(b)
m

# 공분산행렬
S <- cov(b)
S

# scatter plot
plot(age, income)

# 
n <- length(age); n
t <- seq(1,n); t
age_s <- sort(age); age_s
income_s <- sort(income); income_s

# 
edf <- (t-0.5)/n; edf
q <- qnorm(edf, 0, 1); q

# 표 4.3.
cbind(t, age_s, edf, q)
cbind(t, income_s, edf, q)

# sample quantile
plot(q, age_s)

# normalized sample quantile
plot(q, scale(age_s))

# 그림 4.3.
## normal Q-Q plot
qqnorm(age)
## normal Q-Q line
qqline(age)

# 그림 4.4.
## normal Q-Q plot
qqnorm(income)
## normal Q-Q line
qqline(income)


#####################################
##### chisquare plot for multivariate normality

# chi2.plot 함수 생성
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

# chi2.plot 적용
b
chi2.plot(b)

temp <- chi2.plot(b)
temp
temp[[1]]
temp[[2]]


#####################################
##### Bivariate normal probability density

# 데이터 입력
mu1 <- 0    # setting the expected value of x1
mu2 <- 0    # setting the expected value of x2
s11 <- 10   # setting the variance of x1
s12 <- 15   # setting the covariance between x1 and x2
s22 <- 10   # setting the variance of x2
rho <- 0.5  # setting the correlation coefficient between x1 and x2
x1 <- seq(-10, 10, length=41) # generating the vector series x1
x2 <- x1                      # copying x1 to x2

# f 함수 생성
## setting up the function of the multivariate normal density
f <- function(x1,x2){
  term1 <- 1/(2*pi*sqrt(s11*s22*(1-rho^2)))
  term2 <- -1/(2*(1-rho^2))
  term3 <- (x1-mu1)^2/s11
  term4 <- (x2-mu2)^2/s22
  term5 <- 2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
  term1*exp(term2*(term3+term4-term5))
}

# calculating the density values
z <- outer(x1, x2, f)
z

# 그림 4.1. Graph1 : 투시도(3-D plot)
win.graph()
  persp(x1, x2, z, main="Two dimensional Normal Distribution", theta=30)

# 그림 4.2. Graph2 : 등고선 그림(contour plot)
win.graph()
  contour(x1, x2, z, xlab="x1", ylab="x2", main="contour plot")


#####################################
##### 

# 데이터 입력
tree <- read.csv("data/tree.csv", header=T)
cork <- tree[,2:5]
head(cork)

# 
summary(cork)
stem(tree$N)
boxplot(tree$N, xlab="N")
qqnorm(tree$N)
qqline(tree$N)
chi2.plot(cork)


#####################################
##### 연습문제 4.3.

# 데이터 입력
sigma1 <- matrix(c(14,8,3,8,5,2,3,2,1), nc=3)
sigma2 <- matrix(c(6,6,1,6,8,2,1,2,1), nc=3)
sigma1; sigma2

# 일반화분산
det(sigma1); det(sigma2)

# 총분산
sum(diag(sigma1)); sum(diag(sigma2))


#####################################
##### 연습문제 4.4.

# 데이터 입력
library(readxl)
Q <- read_xlsx("data/practice/Table4.6_2001KoreaIndustry.xlsx")
head(Q)
attach(Q)
Q4 <- Q[,2:3]

# (a). 평균벡터, 공분산행렬
m <- colMeans(Q4); m
S <- cov(Q4); S

# (b). Q-Q 그림
op <- par(mfrow=c(1,2))
qqnorm(X1, main="Normal Q-Q plot of X1"); qqline(X1)
qqnorm(X2, main="Normal Q-Q plot of X2"); qqline(X2)
par(op)
chi2.plot(Q4)
