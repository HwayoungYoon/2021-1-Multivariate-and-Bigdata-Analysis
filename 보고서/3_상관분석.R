# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")

# column명 변경
names(my_data) [names(my_data) == "age"] <- c("X1")
names(my_data) [names(my_data) == "sex"] <- c("X2")
names(my_data) [names(my_data) == "education"] <- c("X3")
names(my_data) [names(my_data) == "restrictions"] <- c("X4")
names(my_data) [names(my_data) == "health"] <- c("X5")
names(my_data) [names(my_data) == "healthcare"] <- c("X6")
names(my_data) [names(my_data) == "stress"] <- c("X7")
names(my_data) [names(my_data) == "social.safety"] <- c("X8")
names(my_data) [names(my_data) == "environment.feel"] <- c("X9")
names(my_data) [names(my_data) == "environmental.cost"] <- c("X10")
names(my_data) [names(my_data) == "environmental.issues"] <- c("X11")
names(my_data) [names(my_data) == "prevention.pollution"] <- c("X12")
names(my_data) [names(my_data) == "income"] <- c("X13")


##########################################################################

attach(my_data)

# 상관분석
cor.var <- my_data[,c(1,3:13)]
library(sjPlot)
tab_corr(cor.var, corr.method="spearman")

win.graph()
sjp.corr(cor.var, corr.method="spearman", sort.corr=F, wrap.labels=5)

library(corrplot)
win.graph()
corrplot.mixed(cor(cor.var, method="spearman"), lower="number", upper="pie", tl.col="black")


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
