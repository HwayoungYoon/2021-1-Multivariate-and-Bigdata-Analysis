# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco_data.RData")
attach(Eco_data)


##########################################################################

# 연속형 변수만 선택된 데이터 생성
Eco_cont <- Eco_data[,c(1,10,15,39,47:49,66,69,76,78:81)]
attach(Eco_cont)

#################### 유클리드 거리행렬
## 최단연결법
hc1 <- hclust(dist(t(Eco_cont))^2, method="single")
plot(hc1, hang=-1, main="Single Linkage")
## 최장연결법
hc2 <- hclust(dist(t(Eco_cont))^2, method="complete")
plot(hc2, hang=-1, main="Complete Linkage")
## Ward 방법
hc3 <- hclust(dist(t(Eco_cont))^2, method="ward.D")
plot(hc3, hang=-1, main="Ward Method")
## 평균연결법
hc4 <- hclust(dist(t(Eco_cont))^2, method="average")
plot(hc4, hang=-1, main="Average Linkage")

#################### 맨하탄 거리행렬
## 최단연결법
hc1 <- hclust(dist(t(Eco_cont), method="manhattan"), method="single")
plot(hc1, hang=-1, main="Single Linkage")
## 최장연결법
hc2 <- hclust(dist(t(Eco_cont), method="manhattan"), method="complete")
plot(hc2, hang=-1, main="Complete Linkage")
## Ward 방법
hc3 <- hclust(dist(t(Eco_cont), method="manhattan"), method="ward.D")
plot(hc3, hang=-1, main="Ward Method")
## 평균연결법
hc4 <- hclust(dist(t(Eco_cont), method="manhattan"), method="average")
plot(hc4, hang=-1, main="Average Linkage")


#################### Clusterwise statistics
ccent <- function(y, cl){
  f <- function(i){ colMeans(y[cl==i,]) }
  x <- sapply(sort(unique(cl)), f)
  colnames(x) <- sort(unique(cl))
  return(x)
}

#################### K-means clustering
## 4개 군집
crime_k <- kmeans(t(Eco_cont), centers=4)
attributes(crime_k)
crime_k$cluster

## number of each cluster
kc <- table(crime_k$cluster); kc
plot(t(Eco_cont), pch=crime_k$cluster, col=crime_k$cluster, 
     main="K-means cluster", xlab="", ylab="")
text(t(Eco_cont), labels=colnames(Eco_cont), adj=0, cex=0.7)

## clusterwise info
ccent(Eco_cont, crime_k$cluster)

#################### Model-based clustering
## 군집개수 2~14개 사이 적절한 군집 결정
library(mclust)
crime_mc <- Mclust(t(Eco_cont), 2:14); crime_mc
attributes(crime_mc)

## 군집번호
crime_mc$classification

## number of each cluster
mc <- table(crime_mc$classification); mc

## 모형기반 방법 이용 결과 군집
plot(t(Eco_cont), pch=crime_mc$classification, col=crime_mc$classification, 
     main="Model-based clustering", xlab="", ylab="")
text(t(Eco_cont), labels=colnames(Eco_cont), adj=0, cex=0.7)


#################### 전체 Eco_data에 대한 군집분석
# 데이터 정리
Eco <- Eco_data[,c(1,2,10,15,39,47:49,65,66,69,75,76,78:81)]
attach(Eco)

# 군집 개수 결정
twss <- NULL
for (i in 1:10) {
  kc <- kmeans(Eco, i)
  twss <- c(twss, kc$tot.withinss)
}
win.graph()
plot(1:10, twss, type="b", xlab="군집수", ylab="TWSS", xaxt="n")
axis(side=1,at=c(1:10))

# K-means 군집분석
km <- kmeans(Eco, 2); km
library(factoextra)
win.graph()
fviz_cluster(km, data=Eco, geom="point")

# K-means 군집별 boxplot
win.graph()
par(mfrow=c(3,2))
par(mar=c(3.1,4.1,2.1,2.1))
boxplot(environmental.issues ~ km$cluster, ylab="cluster", xlab="", main="environmental.issues", horizontal=T, col=c("lightgray","gray"))
boxplot(previous.environment ~ km$cluster, ylab="cluster", xlab="", main="previous.environment", horizontal=T, col=c("lightgray","gray"))
boxplot(post.environment ~ km$cluster, ylab="cluster", xlab="", main="post.environment", horizontal=T, col=c("lightgray","gray"))
boxplot(environmental.cost ~ km$cluster, ylab="cluster", xlab="", main="environmental.cost", horizontal=T, col=c("lightgray","gray"))
boxplot(environment.feel ~ km$cluster, ylab="cluster", xlab="", main="environment.feel", horizontal=T, col=c("lightgray","gray"))
boxplot(prevention.pollution ~ km$cluster, ylab="cluster", xlab="", main="prevention.pollution", horizontal=T, col=c("lightgray","gray"))

# 모형기반 군집분석
library(mclust)
crime_mc <- Mclust(Eco, 2:13); crime_mc
table(crime_mc$classification)
win.graph()
plot(crime_mc, what="classification", col=c("pink","lightblue"))


##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
save(Eco_cont, file="Eco_cont.RData")
save(Eco, file="Eco.RData")
