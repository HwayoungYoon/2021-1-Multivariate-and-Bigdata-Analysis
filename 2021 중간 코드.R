# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")

########## 1.
S <- matrix(c(25,-2,4,-2,4,1,4,1,9), nc=3)
S

# 1.1.
cov2cor(S)

# 1.2.1.
t(S)
# 1.2.2.
dim(S)
# 1.2.3.
eigen(S)$values

########## 2.
S <- matrix(c(3,2,2,6), nc=2)

# 2.3.
eigen(S)
(eigen(S)$vectors[1,1])^2+(eigen(S)$vectors[2,1])^2
(eigen(S)$vectors[1,2])^2+(eigen(S)$vectors[2,2])^2

# 2.4.
eS <- eigen(S)
eS1 <- eS$values[[1]] * eS$vectors[,1] %*% t(eS$vectors[,1]); eS1
eS2 <- eS$values[[2]] * eS$vectors[,2] %*% t(eS$vectors[,2]); eS2
eS1+eS2; S

########## 4.
tut <- read.csv("data.csv", header=T)
tut
attach(tut)
tutf <- tut[(sex=="f"), 2:3]
tutm <- tut[(sex=="m"), 2:3]

# 4.1.
mf <- colMeans(tutf); mf
mm <- colMeans(tutm); mm

# 4.2.
sf <- cov(tutf); sf
sm <- cov(tutm); sm

# 4.3. 일반화분산
det(sf)
det(sm)

# 4.4. 총분산
sum(diag(sf))
sum(diag(sm))

# 4.5. 유클리드 거리
dist(tutf[1:2,], method="euclidean")

# 4.6. 표준화 거리
D <- diag(sf)*diag(2)
tut12 <- as.matrix(tutf[1,]-tutf[2,])
ds <- sqrt(tut12 %*% solve(D) %*% t(tut12)); ds

# 4.7. 마할라노비스 거리
dm <- sqrt(tut12 %*% solve(sf) %*% t(tut12)); dm

# 4.8.
## 두 모집단, 공분산행렬 같음
X1 <- tutf; X2 <- tutm
bar1 <- mf; bar2 <- mm
s1 <- sf; s2 <- sm
n1 <- nrow(X1); n2 <- nrow(X2)
p <- ncol(tut[,1:2])
S <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2); S

library(MASS)
T2 <- (n1*n2/(n1+n2))*t(bar1-bar2) %*% ginv(S) %*% (bar1-bar2); T2
(n1+n2-2)*p/(n1+n2-p-1)*qf(0.05, p, n1+n2-p-1, lower.tail=F)
f <- ((n1+n2-p-1)/((n1+n2-2)*p))*T2
1-pf(f, p, n1+n2-p-1)

## 두 모집단, 공분산행렬 다름
library(MASS)
T2 <- t(bar1-bar2) %*% ginv(s1/n1+s2/n2) %*% (bar1-bar2); T2
qchisq(0.05, p, lower.tail=F)
pchisq(T2, p, lower.tail=F)

# 4.9.
attach(tut)
y <- cbind(X1, X2)
fit <- manova(y ~ sex)
summary(fit, test="Wilks")

# 4.10.
tu <- tut[,2:3]
tup <- princomp(tu, cor=T)
tup$loadings

########## 5.
S <- matrix(c(37,61,15,5,10,20,61,260,80,66,11,37,15,80,45,1,-13,12,5,66,
              1,15,25,-5,10,11,-13,25,49,-4,20,37,12,-5,-4,32), nc=6)
S

# 5.1. & 5.2.
Sp <- princomp(S, cor=T)
summary(Sp)
Sp$loadings
Sp$scores

# 5.3.
library(graphics)
screeplot(Sp, type="lines")
