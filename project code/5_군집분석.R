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


##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
save(Eco_cont, file="Eco_cont.RData")
