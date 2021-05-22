# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 10.1
library(MASS)
group <- c(1,1,1,2,2,2)
x1 <- c(-2,0,-1,0,2,1)
x2 <- c(5,3,1,6,4,2)
dat <- data.frame(cbind(group, x1, x2))

# 10.1.a
g1 <- dat[(dat$group==1),]
g2 <- dat[(dat$group==2),]
m1 <- colMeans(g1[,2:3])
m2 <- colMeans(g2[,2:3])
s1 <- cov(g1[,2:3]); s1
s2 <- cov(g2[,2:3]); s2
(m1-m2) %*% solve(s1)
((m1-m2) %*% solve(s1) %*% (m1+m2))/2

# 10.1.b
(m1-m2) %*% solve(s1) %*% c(-1,2)


########## 10.2
library(MASS)
group <- c(1,1,1,2,2,2,3,3,3)
x1 <- c(-2,0,-1,0,2,1,1,0,-1)
x2 <- c(5,3,1,6,4,2,-2,0,4)
dat <- data.frame(cbind(group, x1, x2))
g1 <- dat[(dat$group==1),]
g2 <- dat[(dat$group==2),]
g3 <- dat[(dat$group==3),]

# 10.2.a
m1 <- colMeans(g1[,2:3]); m1
m2 <- colMeans(g2[,2:3]); m2
m3 <- colMeans(g3[,2:3]); m3
s1 <- cov(g1[,2:3]); s1
s2 <- cov(g2[,2:3]); s2
s3 <- cov(g3[,2:3]); s3

# 10.2.b
qd <- qda(group ~ x1+x2, prior=c(0.25,0.25,0.5)); qd
pc <- predict(qd, dat)
pc$class
round(pc$posterior, digits=3)
ct <- table(group, pc$class); ct
diag(prop.table(ct, 1))

correct.per <- sum(diag(ct))/sum(ct)
err.per <- 1-correct.per; err.per

log(0.25)-det(s1)/2-(t(m1) %*% solve(s1) %*% m1)/2; solve(s1)/2; t(m1) %*% solve(s1)
log(0.25)-det(s2)/2-(t(m2) %*% solve(s2) %*% m2)/2; solve(s2)/2; t(m2) %*% solve(s2)
log(0.5)-det(s3)/2-(t(m3) %*% solve(s3) %*% m3)/2; solve(s3)/2; t(m3) %*% solve(s3)

# 10.2.c
newx <- c(0,-1,-2)
new <- data.frame(rbind(dat, newx))
result <- predict(qd, newdata=new[10,])
round(result$posterior, digits=3)

# 10.2.d
ld <- lda(group ~ x1+x2, prior=c(0.25,0.25,0.5))
round(ld$scaling, digits=3)
pc <- predict(ld, dat[,2:3])
pc$class
round(pc$posterior, digits=3)
ct <- table(group, pc$class); ct
diag(prop.table(ct, 1))

correct.per <- sum(diag(ct))/sum(ct)
err.per <- 1-correct.per; err.per

# 10.2.e
newx <- c(0,-1,-2)
new <- data.frame(rbind(dat, newx))
result <- predict(ld, newdata=new[10,])
round(result$posterior, digits=3)
