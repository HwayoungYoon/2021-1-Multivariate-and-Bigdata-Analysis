# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 5.2. Hotelling T^2 test

# 데이터 입력
sweat <- read.csv("data/sweat.csv", header=T)
sweat
attach(sweat)
x <- sweat[,2:4]
x

# 그림 5.2.
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x, pch="*", main="3d scatterplot")

# for ginv
library(MASS)
p <- ncol(x); p
xbar <- apply(x, 2, mean); xbar
colMeans(x)
S <- cov(x); S
n <- dim(x)[[1]]; n
# mean vector under H0
mu0 <- c(4,50,10); mu0
T2 <- n*t(xbar-mu0) %*% ginv(S) %*% (xbar-mu0); T2
f <- ((n-p)/(n-1)*p)*T2; f
pvalue <- 1-pf(f, p, n-p); pvalue

library(ggplot2)
ggplot(sweat, aes(x1,x2)) +
  geom_point() +    # + : not in the front
  stat_ellipse()    # 95% default=multivariate t
ggplot(sweat, aes(x1,x2)) +
  geom_point() +              # + : not in the front
  stat_ellipse(type="norm")   # 95% default=multivariate t
ggplot(sweat, aes(x1,x2)) +
  geom_point() +                          # + : not in the front
  stat_ellipse(type="norm", level=0.90)   # 90% default=multivariate n


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


































