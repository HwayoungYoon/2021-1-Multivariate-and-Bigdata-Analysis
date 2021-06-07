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


##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
