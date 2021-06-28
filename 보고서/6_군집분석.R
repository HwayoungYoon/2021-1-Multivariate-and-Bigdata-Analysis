# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")


##########################################################################

attach(my_data)

# 군집 개수 결정
twss <- NULL
for (i in 1:10) {
  kc <- kmeans(my_data, i)
  twss <- c(twss, kc$tot.withinss)
}
win.graph()
plot(1:10, twss, type="b", xlab="군집수", ylab="TWSS", xaxt="n")
axis(side=1,at=c(1:10))

# K-means 군집분석
km <- kmeans(my_data, 2); km
library(factoextra)
win.graph()
fviz_cluster(km, data=my_data, geom="point")

# K-means 군집별 boxplot
win.graph()
par(mfrow=c(4,3))
par(mar=c(3.1,4.1,2.1,2.1))
boxplot(X1 ~ km$cluster, ylab="cluster", xlab="", main="X1", horizontal=T, col=c("lightyellow3","lightyellow1"))
boxplot(X4 ~ km$cluster, ylab="cluster", xlab="", main="X4", horizontal=T, col=c("lightpink3","lightpink1"))
boxplot(X9 ~ km$cluster, ylab="cluster", xlab="", main="X9", horizontal=T, col=c("lightblue3","lightblue1"))
boxplot(X3 ~ km$cluster, ylab="cluster", xlab="", main="X3", horizontal=T, col=c("lightyellow3","lightyellow1"))
boxplot(X5 ~ km$cluster, ylab="cluster", xlab="", main="X5", horizontal=T, col=c("lightpink3","lightpink1"))
boxplot(X10 ~ km$cluster, ylab="cluster", xlab="", main="X10", horizontal=T, col=c("lightblue3","lightblue1"))
boxplot(X8 ~ km$cluster, ylab="cluster", xlab="", main="X8", horizontal=T, col=c("lightyellow3","lightyellow1"))
boxplot(X6 ~ km$cluster, ylab="cluster", xlab="", main="X6", horizontal=T, col=c("lightpink3","lightpink1"))
boxplot(X11 ~ km$cluster, ylab="cluster", xlab="", main="X11", horizontal=T, col=c("lightblue3","lightblue1"))
boxplot(X13 ~ km$cluster, ylab="cluster", xlab="", main="X13", horizontal=T, col=c("lightyellow3","lightyellow1"))
boxplot(X7 ~ km$cluster, ylab="cluster", xlab="", main="X7", horizontal=T, col=c("lightpink3","lightpink1"))
boxplot(X12 ~ km$cluster, ylab="cluster", xlab="", main="X12", horizontal=T, col=c("lightblue3","lightblue1"))

# 모형기반 군집분석
library(mclust)
crime_mc <- Mclust(my_data, 2:13); crime_mc
table(crime_mc$classification)
win.graph()
plot(crime_mc, what="classification", col=c("pink","lightblue"))


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
