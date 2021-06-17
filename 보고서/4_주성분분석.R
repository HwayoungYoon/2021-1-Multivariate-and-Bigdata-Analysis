# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")


##########################################################################

attach(my_data)

# 고유값
round(eigen(cor(my_data))$values, digits=3)

# 주성분분석
p_cor <- princomp(my_data, cor=T)
summary(p_cor)
round(p_cor$sdev, digits=3)
p_cor$loadings

# 스크리 그래프
library(graphics)
win.graph()
screeplot(p_cor, npcs=13, type="lines", main="scree plot")
abline(h=1, col="red")

# 2차원 주성분 그래프
win.graph()
biplot(p_cor, main="principal plot 2D")

# 3차원 주성분 그래프
library(rgl)
plot3d(p_cor$scores[,1:3], col=rainbow(1000), main="principal plot 3D")
rgl.snapshot("principal_plot_3d.png")


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
