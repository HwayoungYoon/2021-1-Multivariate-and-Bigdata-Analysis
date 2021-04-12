# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 6.1.

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
##### 예제 6.7.

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
## SSCP matrix for treatments
m <- colMeans(rabbit[,2:3])
B <- n1*(m1-m) %*% t(m1-m) + n2*(m2-m) %*% t(m2-m) + n3*(m3-m) %*% t(m3-m); B
## SSCP matrix for errors
E <- (n1-1)*s1 + (n2-1)*s2 + (n3-1)*s3; E

test <- solve(E) %*% B
eigen(test)

detach(rabbit)




























