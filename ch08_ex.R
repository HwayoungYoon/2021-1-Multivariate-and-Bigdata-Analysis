# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis")


########## 8.8
library(readxl)
Diabetes <- read_xlsx("data/practice/Table9.2_Diabetes.xlsx")
Diabetes <- as.data.frame(Diabetes)
attach(Diabetes)
Di <- Diabetes[,2:6]

# 8.8.a

# 8.8.b
library(psych)
library(GPArotation)
principal(Di, nfactors=2, rotate="none", cor="cor")

# 8.8.c
p.cor <- princomp(Di, cor=T)
library (graphics)
screeplot (p.cor, npcs=5, type="lines", main="Correlation Screeplot") 

# 8.8.d
lab <- names(p.cor$loadings[,2]) <- c("Y1", "Y2", "X1", "X2", "X3")
plot(p.cor$loadings[,1], p.cor$loadings[,2], xlab="factor1", ylab="factor2", main="Factor Loadings")
  text(p.cor$loadings[,1], p.cor$loadings[,2], labels=lab, adj=0, cex=1)
  abline(v=0, h=0)

# 8.8.e
factanal(Di, factors=2, rotation="varimax")

# 8.8.f
factanal(Di, factors=2, rotation="none")
