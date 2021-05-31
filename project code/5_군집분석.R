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


########## Clusterwise statistics
ccent <- function(y, cl){
  f <- function(i){ colMeans(y[cl==i,]) }
  x <- sapply(sort(unique(cl)), f)
  colnames(x) <- sort(unique(cl))
  return(x)
}

# number of clusters
cl.num <- 3
colnames(x) <- c("murder","rape")

win.graph()
hc1.result <- cutree(hc1, k=cl.num); hc1.result
plot(x, pch=hc1.result, main="single")
text(x, labels=city, adj=0, cex=0.5)
ccent(x, hc1.result)

win.graph()
hc2.result <- cutree(hc2, k=cl.num); hc2.result
plot(x, pch=hc2.result, main="complete")
text(x, labels=city, adj=0, cex=0.5)
ccent(x, hc2.result)

win.graph()
hc3.result <- cutree(hc3, k=cl.num); hc3.result
plot(x, pch=hc3.result, main="Ward")
text(x, labels=city, adj=0, cex=0.5)
ccent(x, hc3.result)

a <- cbind(crime, hc3.result); a
# cluster 1
c1.1 <- a[(a$hc3.result==1),]; c1.1
# cluster 2
c1.2 <- a[(a$hc3.result==2),]; c1.2
# cluster 3
c1.3 <- a[(a$hc3.result==3),]; c1.3

########## K-means clustering
# ?kmeans
# 3개 군집
crime_k <- kmeans(x, centers=3)
attributes(crime_k)
crime_k$cluster
crime_k$centers

########## Grouping
clus <- cbind(city, x, crime_k$cluster); clus
clus1 <- clus[(clus[,4]==1),]; clus1
clus2 <- clus[(clus[,4]==2),]; clus2
clus3 <- clus[(clus[,4]==3),]; clus3

# number of each cluster
kc <- table(crime_k$cluster); kc
plot(x, pch=crime_k$cluster, col=crime_k$cluster, main="K-means cluster")
text(x, labels=city, adj=0, cex=0.5)

# clusterwise info
ccent(x, crime_k$cluster)

library(mclust)
clPairs(x, classification=crime_k$cluster, symbols=1:3)

########## Model-based clustering
## 군집개수 2~5개 사이 적절한 군집 결정
library(mclust)
crime_mc <- Mclust(x, 2:5); crime_mc
crime_mc <- Mclust(x, G=3); crime_mc
attributes(crime_mc)
## 군집번호
crime_mc$classification

ccent(x, crime_mc$classification)

# number of each cluster
mc <- table(crime_mc$classification); mc

# 모형기반 방법 이용 결과 군집
plot(x, pch=crime_mc$classification, col=crime_mc$classification, main="Model-based clustering")
text(x, labels=city, adj=0, cex=0.5)

# Mclust 적용 결과 군집 그림
win.graph()
par(mfrow=c(2,2))
## 공분산행렬 형태에 따른 BIC
plot(crime_mc, what="BIC")
## 군집 형성 그림
plot(crime_mc, what="classification")
## 공분산행렬에 따른 uncertainty
plot(crime_mc, what="uncertainty")
## 확률밀도함수 등고선 그림
plot(crime_mc, what="density")

##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
