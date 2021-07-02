# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")


##########################################################################

# 데이터 분리
library(caret)
set.seed(845)
data_part <- createDataPartition(y=my_data$X11, p=0.75, list=F)
data.train <- my_data[data_part,]
data.test <- my_data[-data_part,]

# 변수의 표준화
max1 <- apply(data.train, 2, max)
min1 <- apply(data.train, 2, min)
train.scaled <- as.data.frame(scale(data.train, center=min1, scale=max1-min1))
max2 <- apply(data.test, 2, max)
min2 <- apply(data.test, 2, min)
test.scaled <- as.data.frame(scale(data.test, center=min2, scale=max2-min2))

# 인공신경망 모델
library(neuralnet)
nn <- neuralnet(X11 ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13, train.scaled, hidden=0, linear.output=T)
plot(nn)
nn$result.matrix

# 가중치 비교
start.weight <- round(nn$startweights[[1]][[1]], digits=3)
weight <- round(nn$weights[[1]][[1]], digits=3)
X <- c("편향","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X12","X13")
nn.weight <- as.data.frame(cbind(X, start.weight, weight))
nn.weight

# 모형 추정
pred.nn <- compute(nn, test.scaled[,c(1:10,12,13)])
pred <- pred.nn$net.result*(max(my_data$X11)-min(my_data$X11))+min(my_data$X11)

# 예측값과 실제값 비교
predict.data <- as.data.frame(round(head(cbind(data.test$X11, pred), 15), digits=3))
predict.data

# 타당성 평가: PMSE
sum((data.test$X11-pred)^2)/nrow(d.test)


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
