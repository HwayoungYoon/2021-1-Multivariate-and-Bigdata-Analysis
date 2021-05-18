# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 세 개 군집을 보여주는 이변량 랜덤 데이터 발생

library(mvtnorm)
dat <- rbind(rmvnorm(25, mean=c(3,2)),rmvnorm(20, mean=c(10,8)),rmvnorm(10, mean=c(20,1)))
plot(abs(dat), xlab=expression(x[1]), ylab=expression(x[2]), pch=8, col="blue")

dat1 <- cbind(c(rep(1,25), rep(2,20), rep(3,10)), dat)
head(dat1); tail(dat1)
plot(abs(dat), xlab=expression(x[1]), ylab=expression(x[2]), pch=dat1[,1], col=dat1[,1])


#####################################
##### 군집방법별 거리

par(mfrow=c(1,3))
set.seed(29)
x1 <- c(0.7,0.8,0.85,0.9,1.1,1,0.95)
x <- c(x1, x1+1.5)
y1 <- sample(x1)
y <- c(y1, y1+1)
plot(x, y, main="single")
lines(c(0.7,2.5), c(1.1,1.7), col="grey")

set.seed(29)
x1 <- c(0.7,0.8,0.85,0.9,1.1,1,0.95)
x <- c(x1, x1+1.5)
y1 <- sample(x1)
y <- c(y1, y1+1)
plot(x, y, main="complete")
lines(c(1.0,2.2), c(0.7,2.1), col="grey")

set.seed(29)
x1 <- c(0.7,0.8,0.85,0.9,1.1,1,0.95)
x <- c(x1, x1+1.5)
y1 <- sample(x1)
y <- c(y1, y1+1)
plot(x, y, main="average")
for (i in 1:7){
  for (j in 8:14)lines(x[c(i,j)], y[c(i,j)], col="grey")
}


#####################################
##### 예제 11.4 프로그램 11.1

# 데이터 입력





















