# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 기상과 대기오염 data

# 데이터 입력
dat <- read.csv("data/기상과대기오염.csv", header=T)
head(dat)

# plot
plot(dat)

library(lattice)
parallelplot(dat, main="parallel graph")

install.packages("aplpack")
library(aplpack)
faces(dat[,2:6], main="face plot for cork")
stars(dat[,2:6], labels=dat[,1], main="star graph")
## (x,y): 화면에서의 위치 x=1~15
stars(dat[,2:6], key.loc=c(7.5, 1.5), labels=dat[,1], main="star graph")
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

# star m,(그림 1.13)
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

## 1번과 2번 간의 유클리드
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
