# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


#####################################
##### 예제 7.5.

# 데이터 입력
library(readxl)
son <- read_xls("data/son_prin.xls")
son <- as.data.frame(son)
attach(son)

m <- colMeans(son); m
S <- cov(son); S
R <- cor(son); R
eigen(S)  # eigen values and vectors of S
eigen(R)  # eigen values and vectors of R

# with correlation matrix
p_cor <- princomp(son, cor=T)
summary(p_cor)
attributes(p_cor)
## the standard deviations of the principal comp
p_cor$sdev
## the matrix of variable loadings
p_cor$loadings
## the scores of the supplied data on the principal
p_cor$scores

all <- cbind(son, p_cor$scores)
all
## sort by first pc
all[order(p_cor$scores[,1]),]
## sort by second pc
all[order(p_cor$scores[,2]),]

# scatterplot, scree plot and Biplot
plot(first, second, pch="*", main="scatterplot of head sizes")

library(graphics)
screeplot(p_cor, npcs=2, type="lines", main="scree plot-correlation")
biplot(p_cor, main="biplot of prin.comp 1 and 2")

# with covariance matrix
p_cov <- princomp(son)
summary(p_cov)
p_cov$sdev
p_cov$loadings
p_cov$scores


#####################################
##### 예제 7.6.

# 데이터 입력
pschy <- read.csv("data/pschy.csv", header=T)
pschy
attach(pschy)

# except gender column
ps <- pschy[,-1]
ps1 <- ps[gender==1,]
ps2 <- ps[gender==2,]

# with covariance matrix : male
p_cov1 <- princomp(ps1, cor=F)
summary(p_cov1)
attributes(p_cov1)
## the standard deviations of the principal component
p_cov1$sdev
## the matrix of variable loadings
p_cov1$loadings
## the scores of the principal components for data
p_cov1$scores

# with covariance matrix : female
p_cov2 <- princomp(ps2, cor=F)
summary(p_cov2)
attributes(p_cov2)
## the standard deviations of the principal component
p_cov2$sdev
## the matrix of variable loadings
p_cov2$loadings
## the scores of the principal components for data
p_cov2$scores

# scatterplot, scree plot and Biplot
library(lattice)
splom(ps1, pch="*", main="scatterplot of head sizes")
splom(ps2, pch="*", main="scatterplot of head sizes")

library(graphics)

screeplot(p_cov1, npcs=4, type="lines", main="scree plot-cov", sub="male")
biplot(p_cov1, main="principal plot", sub="male")

screeplot(p_cov2, npcs=4, type="lines", main="scree plot-cov", sub="female")
biplot(p_cov2, main="principal plot", sub="female")

par(mfrow=c(1,2))
screeplot(p_cov1, npcs=4, type="lines", main="scree plot-cov", sub="male")
screeplot(p_cov2, npcs=4, type="lines", main="scree plot-cov", sub="female")

par(mfrow=c(1,2))
biplot(p_cov1, main="principal plot", sub="male")
biplot(p_cov2, main="principal plot", sub="female")

# For all data male and female
p_cov <- princomp(ps, cor=F)  # with correlation matrix
summary(p_cov)
attributes(p_cov)
## the standard deviations of the principal component
p_cov$sdev
## the matrix of variable loadings
p_cov$loadings
## the scores of the principal components for data
p_cov$scores

all <- cbind(ps, p_cov$scores)
all
# sort by the first princomp
all[order(p_cor$scores[,1]),]

# scatterplot, scree plot and Biplot
library(lattice)
splom(ps, pch="*", main="scatterplot of head sizes")
screeplot(p_cov, npcs=4, type="lines", main="scree plot-correlation")
biplot(p_cov)

# with covariance matrix : All male and female
p_cov <- princomp(ps)
summary(p_cov)
p_cov$sdev
p_cov$loadings
p_cov$scores


#####################################
##### 연습문제 7.6.

# 표본공분산행렬
S <- matrix(c(4,2,0,2,9,6,0,6,16), ncol=3)
S
eS <- eigen(S); eS

rat1 <- eS$values[[1]]/sum(eS$values); rat1
rat2 <- eS$values[[2]]/sum(eS$values); rat2
rat3 <- eS$values[[3]]/sum(eS$values); rat3


#####################################
##### 연습문제 7.7.

library(readxl)
hema0 <- read_xlsx("data/practice/Table7.5_Blood.xlsx")
hema0 <- as.data.frame(hema0)
attach(hema0)
head(hema0)

hema <- hema0[,2:7]
p_cor <- princomp(hema, cor=T)
summary(p_cor)
attributes(p_cor)

p_cor$sdev
p_cor$loadings
p_cor$scores

# 첫 번째 주성분 : 
# 두 번째 주성분 : 
# 세 번째 주성분 : 

library(graphics)
biplot(p_cor)
