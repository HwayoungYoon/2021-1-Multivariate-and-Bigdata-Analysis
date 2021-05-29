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




##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
