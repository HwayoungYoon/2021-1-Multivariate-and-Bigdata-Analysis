# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco.RData")
attach(Eco)


##########################################################################

# 데이터 분리
library(caret)
set.seed(845)
data_part <- createDataPartition(y=Eco$environmental.issues, p=0.75, list=F)
data.train <- Eco[data_part,]
data.test <- Eco[-data_part,]

# 변수의 표준화
max1 <- apply(data.train, 2, max)
min1 <- apply(data.train, 2, min)
train.scaled <- as.data.frame(scale(data.train, center=min1, scale=max1-min1))
max2 <- apply(data.test, 2, max)
min2 <- apply(data.test, 2, min)
test.scaled <- as.data.frame(scale(data.test, center=min2, scale=max2-min2))

# 인공신경망 모델
library(neuralnet)
nn <- neuralnet(environmental.issues ~., train.scaled, hidden=0, linear.output=T)
plot(nn)
nn$result.matrix

# 모형 추정
pred.nn <- compute(nn, test.scaled[,c(1:15,17)])
pred <- pred.nn$net.result*(max(Eco$environmental.issues)-min(Eco$environmental.issues))+min(Eco$environmental.issues)

# 예측값과 실제값 비교
predict.data <- as.data.frame(round(head(cbind(data.test$environmental.issues, pred), 10), digits=3))
predict.data

# 타당성 평가: PMSE
sum((data.test$environmental.issues-pred)^2)/nrow(data.test)
