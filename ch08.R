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


#####################################
##### 연습문제 8.8. 46명 당뇨병 환자
library(readxl)
d <- read_xlsx("data/practice/Table9.2_Diabetes.xlsx")
d <- as.data.frame(d)
head(d)
attach(d)
x <- d[,2:6]; x

# (b) 주성분법 이용 인자
p.cor <- princomp(x, cor=T)
summary(p.cor)
p.cor$loadings

# Gradient Projection Rrotation for factor anal
install.packages("GPArotation")
library(psych)
library(GPArotation)

# 주성분법
x_factor <- principal(x, nfactors=2, rotate="none", cor="cor")
x_factor

library(graphics)
screeplot(p.cor, npcs=5, type="lines", main="screeplot-correlation")

# factor scores scatterplot
factor1 <- p.cor$scores[,1]
factor2 <- p.cor$scores [,2]
plot (factor1, factor2, xlab="factor1", ylab="factor2")
# or
plot(x_factor$scores[,1], x_factor$scores[,2], pch="*",
     xlab="factor1", ylab="factor2", main="factor scores")

# factor loadings plot
a <- names(p.cor$loadings[,2]) <- c("Y1", "Y2", "X1", "X2", "X3")
plot(p.cor$loadings[,1], p.cor$loadings[,2], pch=19, col="blue",
     xlab="factor1", ylab="factor2", main="factor loadings")
  text(p.cor$loadings[,1], p.cor$loadings[,2], labels=a, adj=0, cex=1)

# varimax rotation
fact_var <- factanal(x, factors=2, rotation="varimax")
fact_var

# maximum likelihood : default
fact_ml <- factanal(x, factors=2, rotation="none")
fact_ml

# GPA rotation
TV <- GPFoblq(loadings(fact_ml), method="oblimin", normalize=T)
print(TV)
print(TV, table=T)
summary(TV)
