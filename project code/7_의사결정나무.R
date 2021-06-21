# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco.RData")
attach(Eco)


##########################################################################

library(caret)
library(rpart)
library(rpart.plot)

# 데이터 분리
set.seed(845)
data_part <- createDataPartition(y=Eco$environmental.issues, p=0.75, list=F)
data_train <- Eco[data_part,]
data_test <- Eco[-data_part,]

# 나무 생성
fit.train <- rpart(environmental.issues ~., method="anova", data=data_train)
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
fit <- rpart(environmental.issues ~., method="anova", data=Eco)
fit.prune <- prune(fit, cp=fit$cptable[which.min(fit$capable[,"xerror"]),"CP"])
pred <- predict(fit.prune, Eco, type="vector")
mean((Eco$environmental.issues-pred)^2)

pred.test <- predict(fit.prune.train, data_test, type="vector")
mean((data_test$environmental.issues-pred.test)^2)
