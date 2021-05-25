# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco_data.RData")
attach(Eco_data)


##########################################################################

# 성별/모든 연속형 변수
Eco_data$sex.factor <- factor(Eco_data$sex, levels=c(1,2), labels=c("남자","여자"))

#################### 환경문제에 대한 인식
## 분산 동질성 검정
var.test(environmental.issues ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(environmental.issues ~ sex.factor, var.equal=TRUE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$environmental.issues, group=Eco_data$sex.factor)

#################### 연령
## 분산 동질성 검정
var.test(age ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(age ~ sex.factor, var.equal=FALSE, data=Eco_data)

#################### 건강평가
## 분산 동질성 검정
var.test(health ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(health ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 스트레스 정도
## 분산 동질성 검정
var.test(stress ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(stress ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 가족 관계 만족도
## 분산 동질성 검정
var.test(family.relationship ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(family.relationship ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 생활 환경 상황 변화 5년 전
## 분산 동질성 검정
var.test(previous.environment ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(previous.environment ~ sex.factor, var.equal=TRUE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$previous.environment, group=Eco_data$sex.factor)

#################### 생활 환경 상황 변화 5년 후
## 분산 동질성 검정
var.test(post.environment ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(post.environment ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 환경보호 비용 부담
## 분산 동질성 검정
var.test(environmental.cost ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(environmental.cost ~ sex.factor, var.equal=FALSE, data=Eco_data)

#################### 교육정도
## 분산 동질성 검정
var.test(study ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(study ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 가구소득
## 분산 동질성 검정
var.test(income ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(income ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 활동 제약 상태
## 분산 동질성 검정
var.test(restrictions ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(restrictions ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 사회 안전에 대한 인식도
## 분산 동질성 검정
var.test(social.safety ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(social.safety ~ sex.factor, var.equal=TRUE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$social.safety, group=Eco_data$sex.factor)

#################### 현재 체감 환경
## 분산 동질성 검정
var.test(environment.feel ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(environment.feel ~ sex.factor, var.equal=TRUE, data=Eco_data)

#################### 환경오염 방지 노력
## 분산 동질성 검정
var.test(prevention.pollution ~ sex.factor, data=Eco_data)
## 연구가설 검증
t.test(prevention.pollution ~ sex.factor, var.equal=TRUE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$prevention.pollution, group=Eco_data$sex.factor)


##########################################################################

# 행정구역/모든 연속형 변수
Eco_data$address.factor <- factor(Eco_data$address, levels=c(0,1), labels=c("비수도권","수도권"))

#################### 환경문제에 대한 인식
## 분산 동질성 검정
var.test(environmental.issues ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(environmental.issues ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 연령
## 분산 동질성 검정
var.test(age ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(age ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 건강평가
## 분산 동질성 검정
var.test(health ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(health ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 스트레스 정도
## 분산 동질성 검정
var.test(stress ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(stress ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 가족 관계 만족도
## 분산 동질성 검정
var.test(family.relationship ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(family.relationship ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 생활 환경 상황 변화 5년 전
## 분산 동질성 검정
var.test(previous.environment ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(previous.environment ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 생활 환경 상황 변화 5년 후
## 분산 동질성 검정
var.test(post.environment ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(post.environment ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 환경보호 비용 부담
## 분산 동질성 검정
var.test(environmental.cost ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(environmental.cost ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 교육정도
## 분산 동질성 검정
var.test(study ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(study ~ address.factor, var.equal=FALSE, data=Eco_data)

#################### 가구소득
## 분산 동질성 검정
var.test(income ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(income ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 활동 제약 상태
## 분산 동질성 검정
var.test(restrictions ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(restrictions ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 사회 안전에 대한 인식도
## 분산 동질성 검정
var.test(social.safety ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(social.safety ~ address.factor, var.equal=TRUE, data=Eco_data)

#################### 현재 체감 환경
## 분산 동질성 검정
var.test(environment.feel ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(environment.feel ~ address.factor, var.equal=FALSE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$environment.feel, group=Eco_data$address.factor)

#################### 환경오염 방지 노력
## 분산 동질성 검정
var.test(prevention.pollution ~ address.factor, data=Eco_data)
## 연구가설 검증
t.test(prevention.pollution ~ address.factor, var.equal=TRUE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$prevention.pollution, group=Eco_data$address.factor)


##########################################################################

# 세대구분/모든 연속형 변수
Eco_data$house.type.factor <- factor(Eco_data$house.type, levels=c(0,1), labels=c("2인 이상 가구","1인 가구"))

#################### 환경문제에 대한 인식
## 분산 동질성 검정
var.test(environmental.issues ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(environmental.issues ~ house.type.factor, var.equal=FALSE, data=Eco_data)
## 집단에 따른 기술 통계량
library(psych)
describeBy(Eco_data$environmental.issues, group=Eco_data$house.type.factor)

#################### 연령
## 분산 동질성 검정
var.test(age ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(age ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 건강평가
## 분산 동질성 검정
var.test(health ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(health ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 스트레스 정도
## 분산 동질성 검정
var.test(stress ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(stress ~ house.type.factor, var.equal=FALSE, data=Eco_data)

#################### 가족 관계 만족도
## 분산 동질성 검정
var.test(family.relationship ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(family.relationship ~ house.type.factor, var.equal=FALSE, data=Eco_data)

#################### 생활 환경 상황 변화 5년 전
## 분산 동질성 검정
var.test(previous.environment ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(previous.environment ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 생활 환경 상황 변화 5년 후
## 분산 동질성 검정
var.test(post.environment ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(post.environment ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 환경보호 비용 부담
## 분산 동질성 검정
var.test(environmental.cost ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(environmental.cost ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 교육정도
## 분산 동질성 검정
var.test(study ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(study ~ house.type.factor, var.equal=FALSE, data=Eco_data)

#################### 가구소득
## 분산 동질성 검정
var.test(income ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(income ~ house.type.factor, var.equal=FALSE, data=Eco_data)

#################### 활동 제약 상태
## 분산 동질성 검정
var.test(restrictions ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(restrictions ~ house.type.factor, var.equal=FALSE, data=Eco_data)

#################### 사회 안전에 대한 인식도
## 분산 동질성 검정
var.test(social.safety ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(social.safety ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 현재 체감 환경
## 분산 동질성 검정
var.test(environment.feel ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(environment.feel ~ house.type.factor, var.equal=TRUE, data=Eco_data)

#################### 환경오염 방지 노력
## 분산 동질성 검정
var.test(prevention.pollution ~ house.type.factor, data=Eco_data)
## 연구가설 검증
t.test(prevention.pollution ~ house.type.factor, var.equal=TRUE, data=Eco_data)


##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
