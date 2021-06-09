# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco_data.RData")
attach(Eco_data)


##########################################################################

# 변수 생성
Eco_data$sex.re[sex == 1] <- 0
Eco_data$sex.re[sex == 2] <- 1

#################### Full model 1
regression.f1 <- lm(environmental.issues ~ sex.re+age+address+house.type+
                      health+stress+family.relationship+previous.environment+
                      post.environment+environmental.cost+study+income+
                      restrictions+social.safety+environment.feel+
                      prevention.pollution, data=Eco_data)
summary(regression.f1)
library(sjPlot)
tab_model(regression.f1, show.se=TRUE, show.fstat=TRUE, auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(regression.f1)
imcdiag(regression.f1)

# 표준화 계수값
tab_model(regression.f1, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# 변수선택
# 선택제거
step3 <- step(regression.f1, direction="both")
summary(step3)

#################### 변수선택 결과 모형
reg.mod <- lm(environmental.issues ~ sex.re + address + house.type + 
                stress + family.relationship + previous.environment + 
                post.environment + environmental.cost + study + income + 
                social.safety + environment.feel + prevention.pollution, 
              data = Eco_data)
summary(reg.mod)
tab_model(reg.mod, show.se=TRUE, show.fstat=TRUE, auto.label=TRUE)

# mctest 패키지 : 다중공선성 진단
library(mctest)
mc.plot(reg.mod)
imcdiag(reg.mod)

# 표준화 계수값
tab_model(reg.mod, show.se=TRUE, show.std=TRUE, auto.label=TRUE)

# Partial plots
library(ggplot2)
effect_plot(reg.mod, pred = sex.re, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = address, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = house.type, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = stress, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = family.relationship, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = previous.environment, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = post.environment, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = environmental.cost, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = study, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = income, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = social.safety, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = environment.feel, interval = TRUE, plot.points = TRUE)
effect_plot(reg.mod, pred = prevention.pollution, interval = TRUE, plot.points = TRUE)

# 잔차분석
# 더빈왓슨통계량 : 잔차의 독립성
library(car)
durbinWatsonTest(reg.mod)

# 표준화된 잔차의 잔차그림
id <- c(1:nrow(Eco_data))
resid <- rstandard(reg.mod)
par(mfrow=c(1,1))
plot(id, resid, main="잔차의 독립성", ylab="표준화잔차",pch=21)

# 잔차그림 : 오차의 정규성 및 이상점, 영향점
pred <- predict(reg.mod)
plot(pred, resid, main="잔차 vs 적합값", pch=21, col="red", 
     ylab="표준화잔차", xlab="적합값")
abline(0,0)

# 잔차그림 : 반응값의 설명력
library(sjPlot)
plot_residuals(reg.mod)

# 잔차그림 : 회귀분석에서 5개의 잔차그림
par(mfrow=c(3,2))
for(i in 1:5) plot(reg.mod, i)
par(mfrow=c(1,1))

# 잔차그림 : 회귀분석에서 5개의 잔차그림 중 4가지
library(ggfortify)
autoplot(reg.mod)

# Partial residuals plots
effect_plot(reg.mod, pred = sex.re, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = address, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = house.type, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = stress, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = family.relationship, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = previous.environment, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = post.environment, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = environmental.cost, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = study, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = income, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = social.safety, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = environment.feel, interval = TRUE, partial.residuals = TRUE)
effect_plot(reg.mod, pred = prevention.pollution, interval = TRUE, partial.residuals = TRUE)

# 이상점 제거
remove <- c("621")
Eco_data.out <- Eco_data[!row.names(Eco_data)%in%remove,]


##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
