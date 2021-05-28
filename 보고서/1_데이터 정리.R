# 작업공간 설정
setwd("C:/R/Multivariate and Bigdata Analysis/2020 사회조사")

# 데이터 불러오기
raw_data <- read.csv("보건_교육_안전_가족_환경_2020_20210413_74060.csv", header=T)

# 저장된 변수명 확인
names(raw_data)

################################################################################

# 변수 선택 - 숫자형 벡터 만들기
select_variables <- c(4,5,7,12:17,20:24,45:48,104:115,165:171,174:189,238)

# 선택된 변수만 가지는 새로운 데이터셋 생성
select_data <- raw_data[,select_variables]
names(select_data)

#########################################################

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

# 결측값 행 제거
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

################################################################################

# 역방향 코딩 및 재부호화
attach(mydata)
mydata$분류코드.가구소득[분류코드.가구소득==610] <- 1
mydata$분류코드.가구소득[분류코드.가구소득==620] <- 2
mydata$분류코드.가구소득[분류코드.가구소득==630] <- 3
mydata$분류코드.가구소득[분류코드.가구소득==640] <- 4
mydata$분류코드.가구소득[분류코드.가구소득==650] <- 5
mydata$분류코드.가구소득[분류코드.가구소득==660] <- 6
mydata$분류코드.가구소득[분류코드.가구소득==678] <- 7


################################################################################

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

################################################################################

attach(mydata)

# column명 변경
names(mydata) [names(mydata) == "만나이"] <- c("age")
names(mydata) [names(mydata) == "성별"] <- c("sex")
names(mydata) [names(mydata) == "교육정도"] <- c("education")
names(mydata) [names(mydata) == "건강평가"] <- c("health")
names(mydata) [names(mydata) == "환경.보호.비용.부담"] <- c("environmental.cost")
names(mydata) [names(mydata) == "분류코드.가구소득"] <- c("income")

# 변수생성
mydata$restrictions = mydata[,4]+mydata[,5]+mydata[,6]+mydata[,7]+mydata[,8]+mydata[,9]
mydata$healthcare = mydata[,11]+mydata[,12]+mydata[,13]+mydata[,14]
mydata$stress = mydata[,15]+mydata[,16]+mydata[,17]+mydata[,18]
mydata$social.safety = mydata[,19]+mydata[,20]+mydata[,21]+mydata[,22]+mydata[,23]+mydata[,24]+mydata[,25]+mydata[,26]+mydata[,27]+mydata[,28]+mydata[,29]+mydata[,30]
mydata$environment.feel = mydata[,31]+mydata[,32]+mydata[,33]+mydata[,34]+mydata[,35]+mydata[,36]+mydata[,37]
mydata$environmental.issues = mydata[,39]+mydata[,40]+mydata[,41]+mydata[,42]+mydata[,43]+mydata[,44]
mydata$prevention.pollution = mydata[,45]+mydata[,46]+mydata[,47]+mydata[,48]+mydata[,49]+mydata[,50]+mydata[,51]+mydata[,52]+mydata[,53]

################################################################################

attach(mydata)

# 데이터 저장하기
select_variables <- c(1:3,10,38,54:61)
my_data <- mydata[,select_variables]
save(my_data, file="my_data.RData")
