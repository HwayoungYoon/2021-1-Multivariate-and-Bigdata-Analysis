# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 8.5. beiginner and pilot

# 데이터 입력
pilot <- read.csv("data/pilot_fact.csv", header=T)
head(pilot)
attach(pilot)

table(group)

x <- pilot[,2:7]; x
summary(x)

m <- colMeans(x); m
S <- cov(x); S
R <- cor(x); R

# eigen values and vectors of S and R
eigen(S)
eigen(R)

# no rotation
fact1 <- factanal(x, factors=3, rotation="none", scores="Bartlett"); fact1
# varimax is the default
fact2 <- factanal(x, factors=3, scores="regression"); fact2
# promax rotation
fact3 <- factanal(x, factor=3, rotation="promax"); fact3

# scree plot
library(graphics)
prin <- princomp(x)
screeplot(prin, npcs=6, type="lines", main="scree plot")

# plot of factor pattern
namevar <- names(fact2$loadings) <- c("x1", "x2", "x3", "x4", "x5", "x6")
plot(fact2$loadings[,1], fact2$loadings[,2], pch=16, xlab="factor1", ylab="factor2", main="factor pattern")
  text(x=fact2$loadings[,1], y=fact2$loadings[,2], labels=namevar)
  abline(v=0, h=0)

# plot of factor scores
plot(fact2$scores[,1], fact2$scores[,2], pch="*", xlab="factor1", ylab="factor2", main="factor scores")
plot(fact2$scores[,1], fact2$scores[,2], pch=group, xlab="factor1", ylab="factor2", main="factor scores")

plot(fact3$loadings[,1], fact3$loadings[,2], pch=16, xlab="factor1", ylab="factor2", main="factor pattern")
  text(x=fact3$loadings[,1], y=fact2$loadings[,2], labels=namevar, adj=0)
  abline(v=0, h=0)
plot(fact3$loadings[,1], fact3$loadings[,2], pch=16, xlab="factor1", ylab="factor3", main="factor pattern")
  text(x=fact2$loadings[,1], y=fact2$loadings[,3], labels=namevar, adj=0)
  abline(v=0, h=0)




































