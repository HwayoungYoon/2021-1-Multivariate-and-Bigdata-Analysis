# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

################################################################################
##### 분석을 위한 데이터셋 생성

# 데이터 입력
raw_data <- read.csv("보건_교육_안전_가족_환경_2020_20210413_74060.csv", header=T)
names(raw_data)

# 선택된 변수만 가지는 새로운 데이터셋 생성
select_variables <- c(4,5,7,12:17,20:24,45:48,104:115,165:171,174:189,238)
select_data <- raw_data[,select_variables]
names(select_data)

# 결측값 확인
table(select_data[,1], useNA="ifany")
table(select_data[,2], useNA="ifany")
table(select_data[,3], useNA="ifany")
table(select_data[,4], useNA="ifany")
table(select_data[,5], useNA="ifany")
table(select_data[,6], useNA="ifany")
table(select_data[,7], useNA="ifany")
table(select_data[,8], useNA="ifany")
table(select_data[,9], useNA="ifany")
table(select_data[,10], useNA="ifany")
table(select_data[,11], useNA="ifany")
table(select_data[,12], useNA="ifany")
table(select_data[,13], useNA="ifany")
table(select_data[,14], useNA="ifany")
table(select_data[,15], useNA="ifany")
table(select_data[,16], useNA="ifany")
table(select_data[,17], useNA="ifany")
table(select_data[,18], useNA="ifany")
table(select_data[,19], useNA="ifany")
table(select_data[,20], useNA="ifany")
table(select_data[,21], useNA="ifany")
table(select_data[,22], useNA="ifany")
table(select_data[,23], useNA="ifany")
table(select_data[,24], useNA="ifany")
table(select_data[,25], useNA="ifany")
table(select_data[,26], useNA="ifany")
table(select_data[,27], useNA="ifany")
table(select_data[,28], useNA="ifany")
table(select_data[,29], useNA="ifany")
table(select_data[,30], useNA="ifany")
table(select_data[,31], useNA="ifany")
table(select_data[,32], useNA="ifany")
table(select_data[,33], useNA="ifany")
table(select_data[,34], useNA="ifany")
table(select_data[,35], useNA="ifany")
table(select_data[,36], useNA="ifany")
table(select_data[,37], useNA="ifany")
table(select_data[,38], useNA="ifany")
table(select_data[,39], useNA="ifany")
table(select_data[,40], useNA="ifany")
table(select_data[,41], useNA="ifany")
table(select_data[,42], useNA="ifany")
table(select_data[,43], useNA="ifany")
table(select_data[,44], useNA="ifany")
table(select_data[,45], useNA="ifany")
table(select_data[,46], useNA="ifany")
table(select_data[,47], useNA="ifany")
table(select_data[,48], useNA="ifany")
table(select_data[,49], useNA="ifany")
table(select_data[,50], useNA="ifany")
table(select_data[,51], useNA="ifany")
table(select_data[,52], useNA="ifany")
table(select_data[,53], useNA="ifany")
table(select_data[,54], useNA="ifany")

# 결측값을 가진 행 제거
library(dplyr)
mydata <- select_data %>% filter(!is.na(select_data[,3]))

# 결측값 확인
table(mydata[,1], useNA="ifany")
table(mydata[,2], useNA="ifany")
table(mydata[,3], useNA="ifany")
table(mydata[,4], useNA="ifany")
table(mydata[,5], useNA="ifany")
table(mydata[,6], useNA="ifany")
table(mydata[,7], useNA="ifany")
table(mydata[,8], useNA="ifany")
table(mydata[,9], useNA="ifany")
table(mydata[,10], useNA="ifany")
table(mydata[,11], useNA="ifany")
table(mydata[,12], useNA="ifany")
table(mydata[,13], useNA="ifany")
table(mydata[,14], useNA="ifany")
table(mydata[,15], useNA="ifany")
table(mydata[,16], useNA="ifany")
table(mydata[,17], useNA="ifany")
table(mydata[,18], useNA="ifany")
table(mydata[,19], useNA="ifany")
table(mydata[,20], useNA="ifany")
table(mydata[,21], useNA="ifany")
table(mydata[,22], useNA="ifany")
table(mydata[,23], useNA="ifany")
table(mydata[,24], useNA="ifany")
table(mydata[,25], useNA="ifany")
table(mydata[,26], useNA="ifany")
table(mydata[,27], useNA="ifany")
table(mydata[,28], useNA="ifany")
table(mydata[,29], useNA="ifany")
table(mydata[,30], useNA="ifany")
table(mydata[,31], useNA="ifany")
table(mydata[,32], useNA="ifany")
table(mydata[,33], useNA="ifany")
table(mydata[,34], useNA="ifany")
table(mydata[,35], useNA="ifany")
table(mydata[,36], useNA="ifany")
table(mydata[,37], useNA="ifany")
table(mydata[,38], useNA="ifany")
table(mydata[,39], useNA="ifany")
table(mydata[,40], useNA="ifany")
table(mydata[,41], useNA="ifany")
table(mydata[,42], useNA="ifany")
table(mydata[,43], useNA="ifany")
table(mydata[,44], useNA="ifany")
table(mydata[,45], useNA="ifany")
table(mydata[,46], useNA="ifany")
table(mydata[,47], useNA="ifany")
table(mydata[,48], useNA="ifany")
table(mydata[,49], useNA="ifany")
table(mydata[,50], useNA="ifany")
table(mydata[,51], useNA="ifany")
table(mydata[,52], useNA="ifany")
table(mydata[,53], useNA="ifany")
table(mydata[,54], useNA="ifany")

# 재부호화
attach(mydata)
mydata$분류코드.가구소득[분류코드.가구소득==610] <- 1
mydata$분류코드.가구소득[분류코드.가구소득==620] <- 2
mydata$분류코드.가구소득[분류코드.가구소득==630] <- 3
mydata$분류코드.가구소득[분류코드.가구소득==640] <- 4
mydata$분류코드.가구소득[분류코드.가구소득==650] <- 5
mydata$분류코드.가구소득[분류코드.가구소득==660] <- 6
mydata$분류코드.가구소득[분류코드.가구소득==678] <- 7

# column명 변경
attach(mydata)
names(mydata) [names(mydata) == "만나이"] <- c("age")
names(mydata) [names(mydata) == "성별"] <- c("sex")
names(mydata) [names(mydata) == "교육정도"] <- c("education")
names(mydata) [names(mydata) == "건강평가"] <- c("health")
names(mydata) [names(mydata) == "환경.보호.비용.부담"] <- c("environmental.cost")
names(mydata) [names(mydata) == "분류코드.가구소득"] <- c("income")
attach(mydata)

# 주성분분석
fit1  <- princomp(mydata[,4:9], cor=TRUE)
summary(fit1)
plot(fit1, type="lines")

fit2  <- princomp(mydata[,11:14], cor=TRUE)
summary(fit2)
plot(fit2, type="lines")

fit3  <- princomp(mydata[,15:18], cor=TRUE)
summary(fit3)
plot(fit3, type="lines")

fit4  <- princomp(mydata[,19:30], cor=TRUE)
summary(fit4)
plot(fit4, type="lines")

fit5  <- princomp(mydata[,31:37], cor=TRUE)
summary(fit5)
plot(fit5, type="lines")

fit6  <- princomp(mydata[,39:44], cor=TRUE)
summary(fit6)
plot(fit6, type="lines")

fit7  <- princomp(mydata[,45:53], cor=TRUE)
summary(fit7)
plot(fit7, type="lines")

# 변수생성 (주성분 분석의 결과)
mydata$restrictions = mydata[,4]+mydata[,5]+mydata[,6]+mydata[,7]+mydata[,8]+mydata[,9]
mydata$healthcare = mydata[,11]+mydata[,12]+mydata[,13]+mydata[,14]
mydata$stress = mydata[,15]+mydata[,16]+mydata[,17]+mydata[,18]
mydata$social.safety = mydata[,19]+mydata[,20]+mydata[,21]+mydata[,22]+mydata[,23]+mydata[,24]+mydata[,25]+mydata[,26]+mydata[,27]+mydata[,28]+mydata[,29]+mydata[,30]
mydata$environment.feel = mydata[,31]+mydata[,32]+mydata[,33]+mydata[,34]+mydata[,35]+mydata[,36]+mydata[,37]
mydata$environmental.issues = mydata[,39]+mydata[,40]+mydata[,41]+mydata[,42]+mydata[,43]+mydata[,44]
mydata$prevention.pollution = mydata[,45]+mydata[,46]+mydata[,47]+mydata[,48]+mydata[,49]+mydata[,50]+mydata[,51]+mydata[,52]+mydata[,53]

# 분석을 위한 데이터셋 생성
select_variables <- c(1:3,10,38,54:61)
my_data <- mydata[,select_variables]
my_data <- my_data[,c(1,2,3,7,4,8,9,10,11,5,12,13,6)]


################################################################################
##### 기초통계량

# 빈도표
library(summarytools)
view(freq(sex))

# 기초통계량
library(psych)
describeBy(my_data[,c(1,3:13)])

# 히스토그램
win.graph()
par(mfrow=c(3,4))
par(mar=c(3.1,4.1,2.1,2.1))
hist(age, col="lightyellow", xlab="")
hist(education, col="lightyellow", xlab="")
hist(income, col="lightyellow", xlab="")
hist(social.safety, col="lightyellow", xlab="")
hist(restrictions, col="lightpink", xlab="")
hist(health, col="lightpink", xlab="")
hist(healthcare, col="lightpink", xlab="")
hist(stress, col="lightpink", xlab="")
hist(environment.feel, col="lightblue", xlab="")
hist(environmental.cost, col="lightblue", xlab="")
hist(environmental.issues, col="lightblue", xlab="")
hist(prevention.pollution, col="lightblue", xlab="")

# 상자그림
win.graph()
par(mfrow=c(3,1))
par(mar=c(3.1,12.1,2.1,2.1))
boxplot(my_data[,c(1:3,8,13)], col="lightyellow",  horizontal=T, las=2, 
        names=c("age","sex","education","social.safety","income"))
boxplot(my_data[,4:7], col="lightpink",  horizontal=T, las=2, 
        names=c("restrictions","health","healthcare","stress"))
boxplot(my_data[,9:12], col="lightblue", horizontal=T, las=2, 
        names=c("environment.feel","environmental.cost","environmental.issues","prevention.pollution"))


################################################################################
##### 상관분석

cor.var <- my_data[,c(1,3:13)]
library(sjPlot)
tab_corr(cor.var, corr.method="spearman")

win.graph()
sjp.corr(cor.var, corr.method="spearman", sort.corr=F, wrap.labels=5)

library(corrplot)
win.graph()
corrplot.mixed(cor(cor.var, method="spearman"), lower="number", upper="pie", tl.col="black")


################################################################################
##### 주성분분석

# 고유값
round(eigen(cor(my_data))$values, digits=3)

# 주성분분석
p_cor <- princomp(my_data, cor=T)
summary(p_cor)
round(p_cor$sdev, digits=3)
p_cor$loadings

# 스크리 그래프
library(graphics)
win.graph()
screeplot(p_cor, npcs=13, type="lines", main="scree plot")
abline(h=1, col="red")

# 2차원 주성분 그래프
win.graph()
biplot(p_cor, main="principal plot 2D")

# 3차원 주성분 그래프
library(rgl)
plot3d(p_cor$scores[,1:3], col=rainbow(1000), main="principal plot 3D")
rgl.snapshot("principal_plot_3d.png")


################################################################################
##### 인자분석

# 주성분법
library(psych)
library(GPArotation)
fact1 <- principal(my_data, nfactors=4, rotate="varimax", cor="cor"); fact1

# 최대우도법 varimax rotation
fact2 <- factanal(my_data, factors=4, rotation="varimax"); fact2

# 최대우도법 promax rotation
fact3 <- factanal(my_data, factor=4, rotation="promax"); fact3

# 최대우도법 varimax 인자 패턴 plot
namevar <- names(fact2$loadings) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13")
win.graph()
par(mfrow=c(2,3))
par(mar=c(4.1,4.1,2.1,2.1))
plot(fact2$loadings[,1], fact2$loadings[,2], xlab="factor1", ylab="factor2")
text(x=fact2$loadings[,1], y=fact2$loadings[,2], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,1], fact2$loadings[,3], xlab="factor1", ylab="factor3")
text(x=fact2$loadings[,1], y=fact2$loadings[,3], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,1], fact2$loadings[,4], xlab="factor1", ylab="factor4")
text(x=fact2$loadings[,1], y=fact2$loadings[,4], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,2], fact2$loadings[,3], xlab="factor2", ylab="factor3")
text(x=fact2$loadings[,2], y=fact2$loadings[,3], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,2], fact2$loadings[,4], xlab="factor2", ylab="factor4")
text(x=fact2$loadings[,2], y=fact2$loadings[,4], labels=namevar, adj=-0.2)
abline(v=0, h=0)
plot(fact2$loadings[,3], fact2$loadings[,4], xlab="factor3", ylab="factor4")
text(x=fact2$loadings[,3], y=fact2$loadings[,4], labels=namevar, adj=-0.2)
abline(v=0, h=0)


################################################################################
##### 군집분석

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
##### 의사결정나무

library(caret)
library(rpart)
library(rpart.plot)

# 데이터 분리
set.seed(845)
data_part <- createDataPartition(y=my_data$X11, p=0.75, list=F)
data_train <- my_data[data_part,]
data_test <- my_data[-data_part,]

# 나무 생성
fit.train <- rpart(X11 ~ X1+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13, method="anova", data=data_train)
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
##### 인공신경망

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
sum((data.test$X11-pred)^2)/nrow(data.test)


################################################################################

# 데이터 저장하기
library(writexl)
write_xlsx(my_data, path="20180845윤화영_데이터.xlsx")
