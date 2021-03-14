# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 기상과 대기오염 data

# 데이터 입력
dat <- read.csv("data/기상과대기오염.csv", header=T)
head(dat)

# scatter plot
plot(dat)

# parallel plot
library(lattice)
parallelplot(dat, main="parallel graph")

# face plot
library(aplpack)
faces(dat[,2:6], main="face plot for cork")

# star graph
library(aplpack)
stars(dat[,2:6], labels=dat[,1], main="star graph")
## (x,y): 화면에서의 위치 x=1~15
stars(dat[,2:6], key.loc=c(7.5, 1.5), labels=dat[,1], main="star graph")

# Nightinggale chart
stars(dat[,2:6], labels=dat[,1], draw.segments=T, main="Nightinggale chart")


#####################################
##### tree data

# 데이터 입력
tree <- read.csv("data/tree.csv", header=T)
head(tree)
# N E S W 변수만 선택
cork <- tree[,2:5]
head(cork)

# scatter plot (그림 1.11)
plot(cork)

# 평균, 공분산, 상관관계
m = colMeans(cork)
cv = cov(cork)
cr = cor(cork)
m; cv; cr

# parallel (그림 1.12)
library(lattice)
parallelplot(tree, main="parallel graph")

# star (그림 1.13)
stars(cork, labels=tree[,1], main="star graph")

# face plot (그림 1.14)
library(aplpack)
## no color
faces(cork, main="face plot for cork", face.type=0)
## santa
faces(cork, main="face plot for cork", face.type=2)

# 3차원 산점도 (그림 1.15)
library(lattice)
cloud(N~E*W, data=cork)

# 줄기-잎 그림
summary(cork)
stem(tree$N)
stem(tree$E)
stem(tree$S)
stem(tree$W)

# 상자그림
boxplot(tree$N, xlab="N")

# 일변량 정규성 그림
qqnorm(tree$N)
qqline(tree$N)

names(cork) = c("North", "East", "South", "West")
cork

boxplot(tree$N, tree$E, tree$S, tree$W,
        names=c("North", "East", "South", "West"))

## 1번과 2번 간의 유클리드
### method = "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
dist(cork[1:2,], method="euclidean")

D <- diag(cv)*diag(4)
D
obj12 <- as.matrix(cork[1,]-cork[2,])
obj12
dim(obj12)
ds <- sqrt(obj12 %*% solve(D) %*% t(obj12)); ds
dm <- sqrt(obj12 %*% solve(cv) %*% t(obj12)); dm

dim(cork[1,])
dim(cork[,1])
length(cork[,1])

mahalanobis(cork[1,], center=m, cov=cv)


#####################################
##### 예제 1.1

# 표 1.3 학생자료
st1 = c(90,80)
st2 = c(80,90)
st3 = c(75,80)
st4 = c(70,70)
st5 = c(65,80)
st = rbind(st1, st2, st3, st4, st5)
st

plot(st, pch="*", col="red", cex=2)
summary(st)

# 평균벡터, 공분산행렬, 상관행렬
mu = colMeans(st)
S = cov(st)
R = cor(st)
mu; S; R

## 1번과 2번 간의 유클리드 거리
de <- dist(st[1:2,], method="euclidean")
de

D <- rbind(c(S[1,1], 0), c(0, S[2,2]))
D

ds <- t(st[1,]-st[2,]) %*% solve(D) %*% (st[1,]-st[2,])
ds <- sqrt(ds)
ds

dm <- t(st[1,]-st[2,]) %*% solve(S) %*% (st[1,]-st[2,])
dm <- sqrt(dm)
dm


#####################################
##### draw ellipse

library(car)
x1=c(3,4,2,6,8,2); x2=c(5,5,4,7,10,5)
dataEllipse(x1, x2, levels=c(0.5,0.95), lty=1:2, xlim=c(1,8), ylim=c(1,10))

library(car)
m=c(4.17,6)
A= matrix(c(5.77,5,5,4.8), nrow=2)
plot(c(0,10), c(0,10), type="n", xlab="x", ylab="y")  # empty space 만들기
ellipse(center=m, shape=A,radius=sqrt(qchisq(.7, df=2)), xlim=c(0,10), ylim=c(1,10)) #타원

ctr<- c(4.17,6)     # ctr : 타원 중심(표본평균)
A<- matrix(c(5.77,5,5,4.8), nrow=2)    # A : 공분산 행렬
RR<- chol(A)
angles <- seq(0, 2*pi, length.out=200)
ell    <-  cbind(cos(angles), sin(angles)) %*% RR
ellCtr <- sweep(ell, 2, ctr, "+") 
plot(ellCtr, type="l", lwd=2, asp=1)
