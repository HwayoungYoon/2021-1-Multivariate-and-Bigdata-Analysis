# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 7.2


########## 7.9
R <- matrix(c(1,0.71,0.58,0.56,0.65,0.71,1,0.71,0.6,0.69,0.58,0.71,1,0.75,
              0.71,0.56,0.6,0.75,1,0.74,0.65,0.69,0.71,0.74,1), nc=5)

# 7.9.a
eigen(R)$values[1]/sum(eigen(R)$values)*100
eigen(R)$values[2]/sum(eigen(R)$values)*100
eigen(R)$values[3]/sum(eigen(R)$values)*100
eigen(R)$values[4]/sum(eigen(R)$values)*100
eigen(R)$values[5]/sum(eigen(R)$values)*100

# 7.9.b
eigen(R)$vectors[,1]
eigen(R)$vectors[,2]


########## 7.11
library(readxl)
Hemo <- read_xlsx("data/practice/Table7.5_Blood.xlsx")
Hemo <- as.data.frame(Hemo[,2:7])
attach(Hemo)

# 7.11.a
Hp <- princomp(Hemo, cor=T)
summary(Hp)

# 7.11.b
Hp$loadings

# 7.11.c
biplot(Hp)

# 7.11.d
summary(Hp)

# 7.11.e
library(graphics)
screeplot(Hp, type="lines")
