# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 6.1. ANOVA, MANOVA, 오차제곱합행렬&처리제곱합행렬

# 데이터 입력
root <- read.csv("data/rootstock_manova.csv", header=T)
head(root)
attach(root)

# factor 지정
group <- factor(group)

# groupwise mean & variance
tapply(x1, group, mean)
tapply(x1, group, var)
tapply(x2, group, mean)
tapply(x2, group, var)

# overall mean vector
m <- colMeans(root[,2:3]); m

# groupwise mean vector and covariance matrix
m1 <- colMeans(root[(group==1), 2:3]); m1
s1 <- cov(root[(group==1), 2:3]); s1
m2 <- colMeans(root[(group==2), 2:3]); m2
s2 <- cov(root[(group==2), 2:3]); s2
m3 <- colMeans(root[(group==3), 2:3]); m3
s3 <- cov(root[(group==3), 2:3]); s3

group.num <- as.numeric(group)
plot(x1, x2, pch=group.num, col=group.num, main="scatterplot with root group") +
  text(x1, x2, labels=group.num, adj=0, cex=1)

# oneway ANOVA
## display Type I ANOVA table
ax1 <- aov(x1 ~ group)
summary(ax1)
ax2 <- aov(x2 ~ group)
summary(ax2)

# response vector
y <- cbind(x1, x2); y

# MANOVA
fit <- manova(y ~ group)
## Wilk's lambda
summary(fit, test="Wilks")
## Pillai's trace
summary(fit, test="Pillai")
## Roy's greatest root
summary(fit, test="Roy")
## Hotelling-Lawley trace
summary(fit, test="Hotelling")

# factor 지정
group <- factor(group)

n1 <- table(group)[[1]]; n1
n2 <- table(group)[[2]]; n2
n3 <- table(group)[[3]]; n3

# ANOVA SSCP matrix
## SSCP matrix for treatments
B <- n1*(m1-m) %*% t(m1-m) + n2*(m2-m) %*% t(m2-m) + n3*(m3-m) %*% t(m3-m); B
## SSCP matrix for errors
E <- (n1-1)*s1 + (n2-1)*s2 + (n3-1)*s3; E
test <- solve(E) %*% B
eigen(test)
B+E

# another
n <- n1+n2+n3
B <- (n-1)*cov(fit$fitted.values); B
E <- (n-1)*cov(fit$residuals); E
B+E

detach(root)
rm(root, group)


#####################################
##### 예제 6.7. ANOVA, MANOVA, 오차제곱합행렬&처리제곱합행렬

# 데이터 입력
rabbit <- read.csv("data/rabbit.csv", header=T)
head(rabbit)
attach(rabbit)

# factor 지정
group <- factor(group)

# groupwise mean & variance
tapply(x1, group, mean)
tapply(x1, group, var)
tapply(x2, group, mean)
tapply(x2, group, var)

# oneway ANOVA
## display Type I ANOVA table
ax1 <- aov(x1 ~ group)
summary(ax1)
ax2 <- aov(x2 ~ group)
summary(ax2)

# Tukey Honestly Significant Differences
TukeyHSD(ax1)
TukeyHSD(ax2)

# pairwise multiple comparison
pairwise.t.test(x1, group)
pairwise.t.test(x2, group)

# Plot Means with Error Bars
library(gplots)
group <- factor(group)
par(mfrow=c(1,2))
plotmeans(x1 ~ group, xlab="group", ylab="x1", main="Mean Plot \n with 95% CI")
plotmeans(x2 ~ group, xlab="group", ylab="x2", main="Mean Plot \n with 95% CI")

# Residual plot
plot(ax2, which=1)
# Normal Q-Q plot for residuals
plot(ax2,which=2)

# groupwise mean vector and covariance matrix
colMeans(rabbit[(group==1), 2:3])
s1 <- cov(rabbit[(group==1), 2:3]); s1
colMeans(rabbit[(group==2), 2:3])
s2 <- cov(rabbit[(group==2), 2:3]); s2
colMeans(rabbit[(group==3), 2:3])
s3 <- cov(rabbit[(group==3), 2:3]); s3

group.num <- as.numeric(group)
par(mfrow=c(1,1))
plot(x1, x2, pch=group.num, col=group.num, main="scatterplot with specified group") +
  text(x1, x2, labels=group.num, adj=0, cex=1)

# oneway MANOVA with 2 Dependent Variables
## factor 지정
group <- factor(group)
n1 <- table(group)[[1]]; n1
n2 <- table(group)[[2]]; n2
n3 <- table(group)[[3]]; n3

# response vector
y <- cbind(x1, x2); y

fit <- manova(y ~ group)
## Wilk's lambda
summary(fit, test="Wilks")
## Pillai's trace
summary(fit, test="Pillai")
## Roy's greatest root
summary(fit, test="Roy")
## Hotelling-Lawley trace
summary(fit, test="Hotelling")

# ANOVA SSCP matrix
m <- colMeans(rabbit[,2:3])
## SSCP matrix for treatments
B <- n1*(m1-m) %*% t(m1-m) + n2*(m2-m) %*% t(m2-m) + n3*(m3-m) %*% t(m3-m); B
## SSCP matrix for errors
E <- (n1-1)*s1 + (n2-1)*s2 + (n3-1)*s3; E
test <- solve(E) %*% B
eigen(test)
B+E

# another
## number of samples
n <- length(x1)
B <- (n-1)*cov(fit$fitted.values); B
E <- (n-1)*cov(fit$residuals); E
test <- solve(E) %*% B
eigen(test)
B+E

detach(rabbit)


#####################################
##### 다변량 일원배치 공분산행렬에 대한 동일성 검정

# 데이터 입력
rootstock <- read.csv("data/rootstock_manova.csv", header=T)
head(rootstock)
attach(rootstock)
## number of variables
p <- 2
## number of groups
g <- 3

g1 <- rootstock[(group==1), 2:3]
g2 <- rootstock[(group==2), 2:3]
g3 <- rootstock[(group==3), 2:3]

n1 <- nrow(g1); n2 <- nrow(g2); n3 <- nrow(g3)
v1 <- n1-1; v2 <- n2-1; v3 <- n3-1
v <- c(v1,v2,v3)

# group별 covariance
s1 <- cov(g1); s1 <- round(s1, digits=3); s1
s2 <- cov(g2); s2 <- round(s2, digits=3); s2
s3 <- cov(g3); s3 <- round(s3, digits=3); s3

# pooled covariance
spool <- ((n1-1)*s1+(n2-1)*s2+(n3-1)*s3)/(n1+n2+n3-3)
spool <- round(spool, digits=3); spool

# Box's M test
M1 <- (det(s1)/det(spool))^((n1-1)/2)
M2 <- (det(s2)/det(spool))^((n2-1)/2)
M3 <- (det(s3)/det(spool))^((n3-1)/2)
M <- M1*M2*M3; M

# 공분산행렬 동일성 검정
temp1 <- sum(1/v)
temp2 <- (2*p^2+3*p-1)/(6*(p+1)*(g-1))
c1 <- (temp1-1/sum(v))*temp2
## 검정통계량
u <- -2*(1-c1)*log(M)
df <- (g-1)*p*(p+1)/2
## p-값
p.value <- pchisq(u, df, lower.tail=F)

s1; s2; s3; spool
u; p.value


#####################################
##### 연습문제 6.6.

# 데이터 입력
n1 <- 10; n2 <- 20; n3 <- 50
xbar1 <- c(3,2,1); xbar2 <- c(4,1,1); xbar3 <- c(4,2,2)
s1 <- rbind(c(0.3,0.01,0.01), c(0.01,0.2,0.03), c(0.01,0.03,0.1))
s2 <- rbind(c(0.5,0.11,0.04), c(0.11,0.3,0.007), c(0.04,0.007,0.2))
s3 <- rbind(c(0.32,0.3,0.01), c(0.3,0.7,-0.03), c(0.01,-0.03,0.4))

xbar <- (n1*xbar1+n2*xbar2+n3*xbar3)/(n1+n2+n3); xbar

B <- n1*(xbar1-xbar) %*% t(xbar1-xbar) + n2*(xbar2-xbar) %*% 
  t(xbar2-xbar) + n3*(xbar3-xbar) %*% t(xbar3-xbar); B

E <- (n1-1)*s1 + (n2-1)*s2 + (n3-1)*s3; E

# smaller reject H0
lambda <- det(E)/det(B+E); lambda


#####################################
##### 연습문제 6.7.

# 데이터 입력
library(readxl)
fish <- read_xls("data/practice/Table6.13_fishcook.xls")
fish <- as.data.frame(fish)
head(fish)
attach(fish)

# response vector
y <- cbind(x1,x2,x3,x4); y

# MANOVA
fr <- manova(y ~ factor(method))
## Wilk's lambda
summary(fr, test="Wilks")
## Pillai's trace
summary(fr, test="Pillai")
## Roy's greatest root
summary(fr, test="Roy")
## Hotelling-Lawley trace
summary(fr, test="Hotelling")

# 3d scatter plot
library(scatterplot3d)
scatterplot3d(fish[,2:4], pch=as.numeric(method))

# 공분산행렬 동일성 검정
## 반응변수 개수
p <- 4
## 설명변수 범주 개수
g <- 3

m1 <- fish[(method==1), 2:5]
m2 <- fish[(method==2), 2:5]
m3 <- fish[(method==3), 2:5]

n1 <- nrow(m1); n2 <- nrow(m2); n3 <- nrow(m3)
v1 <- n1-1; v2 <- n2-1; v3 <- n3-1
v <- c(v1,v2,v3)

s1 <- cov(m1); s2 <- cov(m2); s3 <- cov(m3)
spool <- ((n1-1)*s1+(n2-1)*s2+(n3-1)*s3)/(n1+n2+n3-3)

M1 <- (det(s1)/det(spool))^((n1-1)/2)
M2 <- (det(s2)/det(spool))^((n2-1)/2)
M3 <- (det(s3)/det(spool))^((n3-1)/2)
M <- M1*M2*M3

temp2 <- (2*p^2+3*p-1)/(6*(p+1)*(g-1))
c1 <- (sum(1/v)-1/sum(v))*temp2
u <- -2*(1-c1)*log(M)
df <- (g-1)*p*(p+1)/2
p.value <- pchisq(u, df, lower.tail=F)

u; p.value
