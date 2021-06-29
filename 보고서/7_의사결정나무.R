# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러들이기
load("my_data.RData")


##########################################################################

attach(my_data)

library(caret)
library(rpart)
library(rpart.plot)

# 데이터 분리
set.seed(845)
data_part <- createDataPartition(y=my_data$X11, p=0.75, list=F)
data_train <- my_data[data_part,]
data_test <- my_data[-data_part,]

# 나무 생성
fit.train <- rpart(X11 ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13, method="anova", data=data_train)
win.graph()
rpart.plot(fit.train)

printcp(fit.train)
win.graph()
plotcp(fit.train)

# 가지치기
fit.prune.train <- prune(fit.train, cp=fit.train$cptable[which.min(fit.train$capable[,"xerror"]),"CP"])
win.graph()
rpart.plot(fit.prune.train)
summary(fit.prune.train)

# 타당성 평가: PMSE
fit <- rpart(X11 ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13, method="anova", data=my_data)
fit.prune <- prune(fit, cp=fit$cptable[which.min(fit$capable[,"xerror"]),"CP"])
pred <- predict(fit.prune, my_data, type="vector")
mean((my_data$X11-pred)^2)

pred.test <- predict(fit.prune.train, data_test, type="vector")
mean((data_test$X11-pred.test)^2)


################################################################################

# 데이터 저장하기
attach(my_data)
save(my_data, file="my_data.RData")
