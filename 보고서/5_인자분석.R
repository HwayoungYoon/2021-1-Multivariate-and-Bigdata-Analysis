# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")


##########################################################################

attach(my_data)

# 주성분법
library(psych)
library(GPArotation)
fact1 <- principal(my_data, nfactors=4, rotate="varimax", cor="cor"); fact1

# 최대우도법 varimax rotation
fact2 <- factanal(my_data, factors=4, rotation="varimax"); fact2

# 최대우도법 promax rotation
fact3 <- factanal(my_data, factor=4, rotation="promax"); fact3

# 최대우도법 varimax 인자 패턴 plot
namevar <- names(fact2$loadings) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")
win.graph()
par(mfrow=c(2,3))
par(mar=c(4.1,4.1,2.1,2.1))
plot(fact2$loadings[,1], fact2$loadings[,2], xlab="factor1", ylab="factor2")
text(x=fact2$loadings[,1], y=fact2$loadings[,2], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,1], fact2$loadings[,3], xlab="factor1", ylab="factor3")
text(x=fact2$loadings[,1], y=fact2$loadings[,3], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,1], fact2$loadings[,4], xlab="factor1", ylab="factor4")
text(x=fact2$loadings[,1], y=fact2$loadings[,4], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,2], fact2$loadings[,3], xlab="factor2", ylab="factor3")
text(x=fact2$loadings[,2], y=fact2$loadings[,3], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,2], fact2$loadings[,4], xlab="factor2", ylab="factor4")
text(x=fact2$loadings[,2], y=fact2$loadings[,4], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,3], fact2$loadings[,4], xlab="factor3", ylab="factor4")
text(x=fact2$loadings[,3], y=fact2$loadings[,4], labels=namevar, adj=-0.2)
abline(v=0, h=0)


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
