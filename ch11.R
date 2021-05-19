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
crime <- read.csv("data/crime.csv", header=T); crime
attach(crime)

# Clusterwise statistics
ccent <- function(y, cl){
    f <- function(i){ colMeans(y[cl==i,]) }
    x <- sapply(sort(unique(cl)), f)
    colnames(x) <- sort(unique(cl))
  return(x)
}

# Hierarchical cluster analysis
## ?hclust
x <- crime[,3:4]

## distance matrix
dx <- round(dist(x), digits=2); dx
D2 <- dist(x, method="manhattan"); D2

hc1 <- hclust(dist(x)^2, method="single")
plot(hc1, labels=city, hang=-1, main="Dandrogram:Single Linkage")
hc2 <- hclust(dist(x)^2, method="complete")
plot(hc2, labels=city, hang=-1, main="Complete Linkage")
hc3 <- hclust(dist(x)^2, method="ward.D")
plot(hc3, labels=city, hang=-1, main="Ward Method")
hc4 <- hclust(dist(x)^2, method="average")
plot(hc4, labels=city, hang=-1, main="Average Linkage")

cl.num <- 3
colnames(x) <- c("murder","rape")

win.graph()
hc1.result <- cutree(hc1, k=cl.num); hc1.result
plot(x, pch=hc1.result, main="single")
text(x, labels=city, adj=0, cex=0.5)
ccent(x, hc1.result)

win.graph()
hc2.result <- cutree(hc2, k=cl.num); hc2.result
plot(x, pch=hc2.result, main="complete")
text(x, labels=city, adj=0, cex=0.5)
ccent(x, hc2.result)

win.graph()
hc3.result <- cutree(hc3, k=cl.num); hc3.result
plot(x, pch=hc3.result, main="Ward")
text(x, labels=city, adj=0, cex=0.5)
ccent(x, hc3.result)

a <- cbind(crime, hc3.result); a
## cluster 1
c1.1 <- a[(a$hc3.result==1),]; c1.1
## cluster 2
c1.2 <- a[(a$hc3.result==2),]; c1.2
## cluster 3
c1.3 <- a[(a$hc3.result==3),]; c1.3

# K-means clustering















