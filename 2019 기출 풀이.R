# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 1번

# 데이터 입력
mu <- matrix(c(1,10), nc=1); mu
S <- matrix(c(4,3,3,9), nc=2); S

# 1.1 : 고유값, 고유벡터
eigen(S)

# 1.3 : 스펙트럼 분해
eS <- eigen(S)
eS1 <- eS$values[[1]] * eS$vectors[,1] %*% t(eS$vectors[,1])
eS2 <- eS$values[[2]] * eS$vectors[,2] %*% t(eS$vectors[,2])
eS1+eS2; S

# 1.4 : 공분산행렬 -> 상관행렬
R <- cov2cor(S); R

# 1.5 : 역행렬
library(MASS); ginv(S)
solve(S)

# 1.6 : 역행렬의 고유값, 고유벡터
eigen(solve(S))

# 1.7 : 조건수
max(eigen(R)$values)/min(eigen(R)$values)

# 1.8 : 다변량 정규분포의 성질
C1 <- matrix(c(1,-1), nc=1)
t(C1) %*% mu
t(C1) %*% S %*% C1

# 1.12 : 표본분포
mu
S/2


#####################################
##### 2번

# 데이터 입력
rab <- read.csv("data.csv", header=T)
rab
attach(rab)
rabA <- rab[(group=="a"), 2:3]
rabB <- rab[(group=="b"), 2:3]
rabC <- rab[(group=="c"), 2:3]

# 2.1 : 그룹별 평균벡터, 공분산행렬
mA <- colMeans(rabA); mA
sA <- cov(rabA); sA

# 2.2 : 그룹별 평균벡터, 공분산행렬
mB <- colMeans(rabB); mB
sB <- cov(rabB); sB

# 2.3 : 그룹별 평균벡터, 공분산행렬
mC <- colMeans(rabC); mC
sC <- cov(rabC); sC

# 2.4 : 유클리드 거리
dist(rabA[1:2,], method="euclidean")

# 2.5 : 표준화 거리
D <- diag(cov(rabA))*diag(2)
A12 <- as.matrix(rabA[1,]-rabA[2,])
ds <- sqrt(A12 %*% solve(D) %*% t(A12)); ds

# 2.6 : 마할라노비스 거리
A12 <- as.matrix(rabA[1,]-rabA[2,])
dm <- sqrt(A12 %*% solve(cov(rabA)) %*% t(A12)); dm

# 2.7 : Wilks 검정
rabAB <- rab[group!="c",]
attach(rabAB)
y <- cbind(X1, X2)
fit <- manova(y ~ group)
summary(fit, test="Wilks")

# 2.8 : 공분산행렬 다를 때 두 그룹간 평균벡터의 차이가 있나
nA <- nrow(rabA)
nB <- nrow(rabB)
p <- ncol(rab[,1:2])
S <- ((nA-1)*sA+(nB-1)*sB)/(nA+nB-2)
library(MASS)
T2 <- t(mA-mB) %*% ginv(sA/nA+sB/nB) %*% (mA-mB); T2
qchisq(0.05, p, lower.tail=F)
pchisq(T2, p, lower.tail=F)

# 2.9 : 그룹별 색상, 라벨 산점도
attach(rab)
Group <- factor(group)
Id <- factor(id)
group.num <- as.numeric(Group)
plot(X1, X2, pch=group.num, col=group.num, main="scatterplot with rabbit group") +
  text(X1, X2, labels=Id, adj=0, cex=1)

# 2.11 : Wilks 검정
attach(rab)
y <- cbind(X1, X2)
fit <- manova(y ~ group)
summary(fit, test="Wilks")

# 2.12 : ANOVA
attach(rab)
aX1 <- aov(X1 ~ group)
summary(aX1)

# 2.13 : 표준화벡터
mu <- colMeans(rab[,2:3]); mu
Sigma <- cov(rab[,2:3])
S <- sqrt(solve(Sigma))
S %*% t(rab[1,2:3]-mu)
