# 작업공간 설정
setwd("C:/Users/ghkdu/Desktop/덕성여대/학습프로젝트/2020 사회조사")

# 데이터 불러들이기
load("Eco_data.RData")
attach(Eco_data)


##########################################################################

# 성별*행정구역 이원분산분석

#################### 환경문제에 대한 인식
anova(aov(environmental.issues ~ sex.factor*address.factor, data=Eco_data))

#################### 연령
anova(aov(age ~ sex.factor*address.factor, data=Eco_data))

#################### 건강평가
anova(aov(health ~ sex.factor*address.factor, data=Eco_data))

#################### 스트레스 정도
anova(aov(stress ~ sex.factor*address.factor, data=Eco_data))

#################### 가족 관계 만족도
anova(aov(family.relationship ~ sex.factor*address.factor, data=Eco_data))

#################### 생활 환경 상황 변화 5년 전
anova(aov(previous.environment ~ sex.factor*address.factor, data=Eco_data))

#################### 생활 환경 상황 변화 5년 후
anova(aov(post.environment ~ sex.factor*address.factor, data=Eco_data))

#################### 환경보호 비용 부담
anova(aov(environmental.cost ~ sex.factor*address.factor, data=Eco_data))

#################### 교육정도
anova(aov(study ~ sex.factor*address.factor, data=Eco_data))

#################### 가구소득
anova(aov(income ~ sex.factor*address.factor, data=Eco_data))

#################### 활동 제약 상태
anova(aov(restrictions ~ sex.factor*address.factor, data=Eco_data))

#################### 사회 안전에 대한 인식도
anova(aov(social.safety ~ sex.factor*address.factor, data=Eco_data))

#################### 현재 체감 환경
anova(aov(environment.feel ~ sex.factor*address.factor, data=Eco_data))

#################### 환경오염 방지 노력
anova(aov(prevention.pollution ~ sex.factor*address.factor, data=Eco_data))


##########################################################################

# 성별*세대구분 이원분산분석

#################### 환경문제에 대한 인식
anova(aov(environmental.issues ~ sex.factor*house.type.factor, data=Eco_data))

#################### 연령
anova(aov(age ~ sex.factor*house.type.factor, data=Eco_data))

#################### 건강평가
anova(aov(health ~ sex.factor*house.type.factor, data=Eco_data))

#################### 스트레스 정도
anova(aov(stress ~ sex.factor*house.type.factor, data=Eco_data))

#################### 가족 관계 만족도
anova(aov(family.relationship ~ sex.factor*house.type.factor, data=Eco_data))

#################### 생활 환경 상황 변화 5년 전
anova(aov(previous.environment ~ sex.factor*house.type.factor, data=Eco_data))

#################### 생활 환경 상황 변화 5년 후
anova(aov(post.environment ~ sex.factor*house.type.factor, data=Eco_data))

#################### 환경보호 비용 부담
anova(aov(environmental.cost ~ sex.factor*house.type.factor, data=Eco_data))

#################### 교육정도
anova(aov(study ~ sex.factor*house.type.factor, data=Eco_data))

#################### 가구소득
anova(aov(income ~ sex.factor*house.type.factor, data=Eco_data))

#################### 활동 제약 상태
anova(aov(restrictions ~ sex.factor*house.type.factor, data=Eco_data))

#################### 사회 안전에 대한 인식도
anova(aov(social.safety ~ sex.factor*house.type.factor, data=Eco_data))

#################### 현재 체감 환경
anova(aov(environment.feel ~ sex.factor*house.type.factor, data=Eco_data))

#################### 환경오염 방지 노력
anova(aov(prevention.pollution ~ sex.factor*house.type.factor, data=Eco_data))


##########################################################################

# 행정구역*세대구분 이원분산분석

#################### 환경문제에 대한 인식
anova(aov(environmental.issues ~ address.factor*house.type.factor, data=Eco_data))

#################### 연령
anova(aov(age ~ address.factor*house.type.factor, data=Eco_data))

#################### 건강평가
anova(aov(health ~ address.factor*house.type.factor, data=Eco_data))

#################### 스트레스 정도
anova(aov(stress ~ address.factor*house.type.factor, data=Eco_data))

#################### 가족 관계 만족도
anova(aov(family.relationship ~ address.factor*house.type.factor, data=Eco_data))

#################### 생활 환경 상황 변화 5년 전
anova(aov(previous.environment ~ address.factor*house.type.factor, data=Eco_data))

#################### 생활 환경 상황 변화 5년 후
anova(aov(post.environment ~ address.factor*house.type.factor, data=Eco_data))

#################### 환경보호 비용 부담
anova(aov(environmental.cost ~ address.factor*house.type.factor, data=Eco_data))

#################### 교육정도
anova(aov(study ~ address.factor*house.type.factor, data=Eco_data))

#################### 가구소득
anova(aov(income ~ address.factor*house.type.factor, data=Eco_data))

#################### 활동 제약 상태
anova(aov(restrictions ~ address.factor*house.type.factor, data=Eco_data))

#################### 사회 안전에 대한 인식도
anova(aov(social.safety ~ address.factor*house.type.factor, data=Eco_data))

#################### 현재 체감 환경
anova(aov(environment.feel ~ address.factor*house.type.factor, data=Eco_data))

#################### 환경오염 방지 노력
anova(aov(prevention.pollution ~ address.factor*house.type.factor, data=Eco_data))


##########################################################################

# 데이터 저장하기
save(Eco_data, file="Eco_data.RData")
